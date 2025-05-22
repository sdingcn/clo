#include <algorithm>
#include <cctype>
#include <concepts>
#include <cstddef>
#include <cstdlib>
#include <deque>
#include <filesystem>
#include <fstream>
#include <functional>
#include <iostream>
#include <memory>
#include <optional>
#include <stdexcept>
#include <string>
#include <type_traits>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <variant>
#include <vector>

// ------------------------------
// global helper(s)
// ------------------------------

template <typename Type, typename Variant>
struct isAlternativeOfHelper {
    static constexpr bool value = false;
};

template <typename Type, typename... Alternative>
requires ((0 + ... + (std::same_as<Type, Alternative> ? 1 : 0)) == 1)
struct isAlternativeOfHelper<Type, std::variant<Alternative...>> {
    static constexpr bool value = true;
};

template <typename Type, typename Variant>
constexpr bool isAlternativeOf = isAlternativeOfHelper<Type, Variant>::value;

struct SourceLocation {
    SourceLocation(int l = 1, int c = 1): line(l), column(c) {}

    std::string toString() const {
        if (line <= 0 || column <= 0) {
            return "(SourceLocation N/A)";
        }
        return "(SourceLocation " + std::to_string(line) + " " + std::to_string(column) + ")";
    }
    void revert() {
        line = 1;
        column = 1;
    }
    void update(char c) {
        if (c == '\n') {
            line++;
            column = 1;
        } else {
            column++;
        }
    }

    int line;
    int column;
};

void panic(
    const std::string &type,
    const std::string &msg,
    const SourceLocation &sl = SourceLocation(0, 0)
) {
    throw std::runtime_error("[" + type + " error " + sl.toString() + "] " + msg);
}

// ------------------------------
// lexer
// ------------------------------

std::string quote(std::string s) {
    std::string r;
    r += '\"';
    for (char c : s) {
        if (c == '\\') {
            r += "\\\\";
        } else if (c == '\"') {
            r += "\\\"";
        } else {
            r += c;
        }
    }
    r += '\"';
    return r;
}

std::string unquote(std::string s) {
    int n = s.size();
    if (!((n >= 2) &&
          (s[0] == '\"') &&
          (s[n - 1] == '\"'))) {
        panic("unquote", "invalid quoted string");
    }
    s = s.substr(1, n - 2);
    std::reverse(s.begin(), s.end());
    std::string r;
    while (s.size()) {
        char c = s.back();
        s.pop_back();
        if (c == '\\') {
            if (s.size()) {
                char c1 = s.back();
                s.pop_back();
                if (c1 == '\\') {
                    r += '\\';
                } else if (c1 == '"') {
                    r += '"';
                } else if (c1 == 't') {
                    r += '\t';
                } else if (c1 == 'n') {
                    r += '\n';
                } else {
                    panic("unquote", "invalid escape sequence");
                }
            } else {
                panic("unquote", "incomplete escape sequence");
            }
        } else {
            r += c;
        }
    }
    return r;
}

struct SourceStream {
    SourceStream(std::string s): source(std::move(s)) {
        std::string charstr =
            "`1234567890-=~!@#$%^&*()_+"
            "qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM"
            "[]\\;',./{}|:\"<>? \t\n";
        std::unordered_set<char> charset(charstr.begin(), charstr.end());
        for (char c : source) {
            if (!charset.contains(c)) {
                panic("lexer", "unsupported character", sl);
            }
            sl.update(c);
        }
        sl.revert();
        std::reverse(source.begin(), source.end());
    }

    bool hasNext() const {
        return source.size() > 0;
    }
    char peekNext() const {
        return source.back();
    }
    char popNext() {
        char c = source.back();
        source.pop_back();
        sl.update(c);
        return c;
    }
    SourceLocation getNextSourceLocation() const {
        return sl;
    }

    std::string source;
    SourceLocation sl;
};

struct Token {
    Token(SourceLocation s, std::string t) : sl(s), text(std::move(t)) {}

    SourceLocation sl;
    // for string literals, use the original (quoted) versions in the source stream
    std::string text;
};

std::deque<Token> lex(std::string source) {
    SourceStream ss(std::move(source));

    std::function<std::optional<Token>()> nextToken =
        [&ss, &nextToken]() -> std::optional<Token> {
        // skip whitespaces
        while (ss.hasNext() && std::isspace(ss.peekNext())) {
            ss.popNext();
        }
        if (!ss.hasNext()) {
            return std::nullopt;
        }
        // read the next token
        auto startsl = ss.getNextSourceLocation();
        std::string text = "";
        // integer literal
        if (std::isdigit(ss.peekNext()) || ss.peekNext() == '-' || ss.peekNext() == '+') {
            if (ss.peekNext() == '-' || ss.peekNext() == '+') {
                text += ss.popNext();
            }
            bool hasDigit = false;
            while (ss.hasNext() && std::isdigit(ss.peekNext())) {
                hasDigit = true;
                text += ss.popNext();
            }
            if (!hasDigit) {
                panic("lexer", "incomplete integer literal", startsl);
            }
        // string literal
        } else if (ss.peekNext() == '"') {
            text += ss.popNext();
            bool complete = false;
            bool escape = false;
            while (ss.hasNext()) {
                if ((!escape) && ss.peekNext() == '"') {
                    text += ss.popNext();
                    complete = true;
                    break;
                } else {
                    char c = ss.popNext();
                    if (c == '\\') {
                        escape = true;
                    } else {
                        escape = false;
                    }
                    text += c;
                }
            }
            if (!complete) {
                panic("lexer", "incomplete string literal", startsl);
            }
        // variable / keyword
        } else if (std::isalpha(ss.peekNext()) || ss.peekNext() == '_') {
            while (
                ss.hasNext() && (
                    std::isalpha(ss.peekNext()) ||
                    std::isdigit(ss.peekNext()) ||
                    ss.peekNext() == '_'
                )
            ) {
               text += ss.popNext();
            }
        // intrinsic
        } else if (ss.peekNext() == '.') {
            while (ss.hasNext() && !(std::isspace(ss.peekNext()) || ss.peekNext() == ')')) {
                text += ss.popNext();
            }
        // special symbol
        } else if (std::string("(){}@").find(ss.peekNext()) != std::string::npos) {
            text += ss.popNext();
        // comment
        } else if (ss.peekNext() == '#') {
            while (ss.hasNext() && ss.peekNext() != '\n') {
                ss.popNext();
            }
            // nextToken() will consume the \n and recursively continue
            return nextToken();
        } else {
            panic("lexer", "unsupported starting character", startsl);
        }
        return Token(startsl, std::move(text));
    };

    std::deque<Token> tokens;
    while (true) {
        auto ret = nextToken();
        if (ret.has_value()) {
            tokens.push_back(ret.value());
        } else {
            break;
        }
    }
    return tokens;
}

// ------------------------------
// AST, parser, and static analysis
// ------------------------------

enum class TraversalMode {
    TOP_DOWN,
    BOTTOM_UP
};

// this also prevents implicitly-declared move constructors and move assignment operators
// use clone() to copy the entire tree
#define DELETE_COPY(CLASS) \
    CLASS(const CLASS &) = delete; \
    CLASS &operator=(const CLASS &) = delete

struct ExprNode {
    DELETE_COPY(ExprNode);
    virtual ~ExprNode() {}
    ExprNode(SourceLocation s): sl(s) {}

    virtual ExprNode *clone() const = 0;
    virtual void traverse(
        TraversalMode mode,
        // callback may have states
        std::function<void(ExprNode*)> &callback
    ) = 0;
    virtual std::string toString() const = 0;
    virtual void computeFreeVars() = 0;
    virtual void computeTail(bool parentTail) = 0;

    SourceLocation sl;
    // static information for free variables in the expression
    std::unordered_set<std::string> freeVars;
    // static information for tail position in the parent expression
    bool tail = false;
};

// every value is accessed by reference to its location on the heap 
using Location = int;

// AST nodes store original tokens (string for IntegerNode)
struct IntegerNode : public ExprNode {
    DELETE_COPY(IntegerNode);
    virtual ~IntegerNode() {}
    IntegerNode(SourceLocation s, std::string v): ExprNode(s), val(std::move(v)) {}

    // covariant return type for override
    virtual IntegerNode *clone() const override {
        auto inode = new IntegerNode(sl, val);
        inode->freeVars = freeVars;
        inode->tail = tail;
        return inode;
    }
    virtual void traverse(
        TraversalMode,
        std::function<void(ExprNode*)> &callback
    ) override {
        callback(this);
    }
    virtual std::string toString() const override {
        return val;
    }
    virtual void computeFreeVars() override {
    }
    virtual void computeTail(bool parentTail) override {
        tail = parentTail;
    }

    std::string val;
    // location on the heap: statically pre-computed
    Location loc = -1;
};

// AST nodes store original tokens (quoted string for StringNode)
struct StringNode : public ExprNode {
    DELETE_COPY(StringNode);
    virtual ~StringNode() {}
    StringNode(SourceLocation s, std::string v): ExprNode(s), val(std::move(v)) {}

    // covariant return type
    virtual StringNode *clone() const override {
        auto snode = new StringNode(sl, val);
        snode->freeVars = freeVars;
        snode->tail = tail;
        return snode;
    }
    virtual void traverse(
        TraversalMode,
        std::function<void(ExprNode*)> &callback
    ) override {
        callback(this);
    }
    virtual std::string toString() const override {
        return val;
    }
    virtual void computeFreeVars() override {
    }
    virtual void computeTail(bool parentTail) override {
        tail = parentTail;
    }

    std::string val;
    // location on the heap: statically pre-computed
    Location loc = -1;
};

struct VariableNode : public ExprNode {
    DELETE_COPY(VariableNode);
    virtual ~VariableNode() {}
    VariableNode(SourceLocation s, std::string n): ExprNode(s), name(std::move(n)) {}

    virtual VariableNode *clone() const override {
        auto vnode = new VariableNode(sl, name);
        vnode->freeVars = freeVars;
        vnode->tail = tail;
        return vnode;
    }
    virtual void traverse(
        TraversalMode,
        std::function<void(ExprNode*)> &callback
    ) override {
        callback(this);
    }
    virtual std::string toString() const override {
        return name;
    }
    virtual void computeFreeVars() override {
        freeVars.insert(name);
    }
    virtual void computeTail(bool parentTail) override {
        tail = parentTail;
    }

    std::string name;
};

struct LambdaNode : public ExprNode {
    DELETE_COPY(LambdaNode);
    virtual ~LambdaNode() {
        for (auto v : varList) {
            delete v;
        }
        delete expr;
    }
    LambdaNode(SourceLocation s, std::vector<VariableNode*> v, ExprNode *e):
        ExprNode(s), varList(std::move(v)), expr(e) {}

    virtual LambdaNode *clone() const override {
        std::vector<VariableNode*> newVarList;
        for (auto v : varList) {
            newVarList.push_back(v->clone());
        }
        ExprNode *newExpr = expr->clone();
        auto lnode = new LambdaNode(sl, std::move(newVarList), newExpr);
        lnode->freeVars = freeVars;
        lnode->tail = tail;
        return lnode;
    }
    virtual void traverse(
        TraversalMode mode,
        std::function<void(ExprNode*)> &callback
    ) override {
        if (mode == TraversalMode::TOP_DOWN) {
            callback(this);
            _traverseSubtree(mode, callback);
        } else {
            _traverseSubtree(mode, callback);
            callback(this);
        }
    }
    virtual std::string toString() const override {
        std::string ret = "lambda (";
        for (auto v : varList) {
            ret += v->toString();
            ret += " ";
        }
        if (ret.back() == ' ') {
            ret.pop_back();
        }
        ret += ") ";
        ret += expr->toString();
        return ret;
    }
    virtual void computeFreeVars() override {
        expr->computeFreeVars();
        freeVars.insert(expr->freeVars.begin(), expr->freeVars.end());
        for (auto var : varList) {
            freeVars.erase(var->name);
        }
    }
    virtual void computeTail(bool parentTail) override {
        tail = parentTail;
        for (auto var : varList) {
            var->computeTail(false);
        }
        expr->computeTail(true);
    }

    std::vector<VariableNode*> varList;
    ExprNode *expr;
private:
    void _traverseSubtree(TraversalMode mode, std::function<void(ExprNode*)> &callback) {
        for (auto var : varList) {
            var->traverse(mode, callback);
        }
        expr->traverse(mode, callback);
    }
};

struct LetrecNode : public ExprNode {
    DELETE_COPY(LetrecNode);
    virtual ~LetrecNode() {
        for (auto &ve : varExprList) {
            delete ve.first;
            delete ve.second;
        }
        delete expr;
    }
    LetrecNode(SourceLocation s, std::vector<std::pair<VariableNode*, ExprNode*>> v, ExprNode *e):
        ExprNode(s), varExprList(std::move(v)), expr(e) {}

    virtual LetrecNode *clone() const override {
        std::vector<std::pair<VariableNode*, ExprNode*>> newVarExprList;
        for (const auto &ve : varExprList) {
            // the evaluation order of the two clones are irrelevant
            newVarExprList.push_back(std::make_pair(ve.first->clone(), ve.second->clone()));
        }
        ExprNode *newExpr = expr->clone();
        auto lnode = new LetrecNode(sl, std::move(newVarExprList), newExpr);
        lnode->freeVars = freeVars;
        lnode->tail = tail;
        return lnode;
    }
    virtual void traverse(
        TraversalMode mode,
        std::function<void(ExprNode*)> &callback
    ) override {
        if (mode == TraversalMode::TOP_DOWN) {
            callback(this);
            _traverseSubtree(mode, callback);
        } else {
            _traverseSubtree(mode, callback);
            callback(this);
        }
    }
    virtual std::string toString() const override {
        std::string ret = "letrec (";
        for (const auto &ve : varExprList) {
            ret += ve.first->toString();
            ret += " ";
            ret += ve.second->toString();
            ret += " ";
        }
        if (ret.back() == ' ') {
            ret.pop_back();
        }
        ret += ") ";
        ret += expr->toString();
        return ret;
    }
    virtual void computeFreeVars() override {
        expr->computeFreeVars();
        freeVars.insert(expr->freeVars.begin(), expr->freeVars.end());
        for (auto &ve : varExprList) {
            ve.second->computeFreeVars();
            freeVars.insert(ve.second->freeVars.begin(), ve.second->freeVars.end());
        }
        for (auto &ve : varExprList) {
            freeVars.erase(ve.first->name);
        }
    }
    virtual void computeTail(bool parentTail) override {
        tail = parentTail;
        for (auto &ve : varExprList) {
            ve.first->computeTail(false);
            ve.second->computeTail(false);
        }
        expr->computeTail(tail);
    }
    
    std::vector<std::pair<VariableNode*, ExprNode*>> varExprList;
    ExprNode *expr;
private:
    void _traverseSubtree(TraversalMode mode, std::function<void(ExprNode*)> &callback) {
        for (auto &ve : varExprList) {
            ve.first->traverse(mode, callback);
            ve.second->traverse(mode, callback);
        }
        expr->traverse(mode, callback);
    }
};

struct IfNode : public ExprNode {
    DELETE_COPY(IfNode);
    virtual ~IfNode() {
        delete cond;
        delete branch1;
        delete branch2;
    }
    IfNode(SourceLocation s, ExprNode *c, ExprNode *b1, ExprNode *b2):
        ExprNode(s), cond(c), branch1(b1), branch2(b2) {}

    virtual IfNode *clone() const override {
        // the evaluation order of the three clones are irrelevant
        auto inode = new IfNode(sl, cond->clone(), branch1->clone(), branch2->clone());
        inode->freeVars = freeVars;
        inode->tail = tail;
        return inode;
    }
    virtual void traverse(
        TraversalMode mode,
        std::function<void(ExprNode*)> &callback
    ) override {
        if (mode == TraversalMode::TOP_DOWN) {
            callback(this);
            _traverseSubtree(mode, callback);
        } else {
            _traverseSubtree(mode, callback);
            callback(this);
        }
    }
    virtual std::string toString() const override {
        return "if " + cond->toString() + " " + branch1->toString() + " " + branch2->toString();
    }
    virtual void computeFreeVars() override {
        cond->computeFreeVars();
        freeVars.insert(cond->freeVars.begin(), cond->freeVars.end());
        branch1->computeFreeVars();
        freeVars.insert(branch1->freeVars.begin(), branch1->freeVars.end());
        branch2->computeFreeVars();
        freeVars.insert(branch2->freeVars.begin(), branch2->freeVars.end());
    }
    virtual void computeTail(bool parentTail) override {
        tail = parentTail;
        cond->computeTail(false);
        branch1->computeTail(tail);
        branch2->computeTail(tail);
    }

    ExprNode *cond;
    ExprNode *branch1;
    ExprNode *branch2;
private:
    void _traverseSubtree(TraversalMode mode, std::function<void(ExprNode*)> &callback) {
        cond->traverse(mode, callback);
        branch1->traverse(mode, callback);
        branch2->traverse(mode, callback);
    }
};

struct SequenceNode : public ExprNode {
    DELETE_COPY(SequenceNode);
    virtual ~SequenceNode() {
        for (auto e : exprList) {
            delete e;
        }
    }
    SequenceNode(SourceLocation s, std::vector<ExprNode*> e):
        ExprNode(s), exprList(std::move(e)) {}

    virtual SequenceNode *clone() const override {
        std::vector<ExprNode*> newExprList;
        for (auto e : exprList) {
            newExprList.push_back(e->clone());
        }
        auto snode = new SequenceNode(sl, std::move(newExprList));
        snode->freeVars = freeVars;
        snode->tail = tail;
        return snode;
    }
    virtual void traverse(
        TraversalMode mode,
        std::function<void(ExprNode*)> &callback
    ) override {
        if (mode == TraversalMode::TOP_DOWN) {
            callback(this);
            _traverseSubtree(mode, callback);
        } else {
            _traverseSubtree(mode, callback);
            callback(this);
        }
    }
    virtual std::string toString() const override {
        std::string ret = "{";
        for (auto e : exprList) {
            ret += e->toString();
            ret += " ";
        }
        if (ret.back() == ' ') {
            ret.pop_back();
        }
        ret += "}";
        return ret;
    }
    virtual void computeFreeVars() override {
        for (auto e : exprList) {
            e->computeFreeVars();
            freeVars.insert(e->freeVars.begin(), e->freeVars.end());
        }
    }
    virtual void computeTail(bool parentTail) override {
        tail = parentTail;
        int n = exprList.size();
        for (int i = 0; i < n - 1; i++) {
            exprList[i]->computeTail(false);
        }
        exprList[n - 1]->computeTail(tail);
    }

    std::vector<ExprNode*> exprList;
private:
    void _traverseSubtree(TraversalMode mode, std::function<void(ExprNode*)> &callback) {
        for (auto e : exprList) {
            e->traverse(mode, callback);
        }
    }
};

struct IntrinsicCallNode : public ExprNode {
    DELETE_COPY(IntrinsicCallNode);
    virtual ~IntrinsicCallNode() {
        for (auto a : argList) {
            delete a;
        }
    }
    IntrinsicCallNode(SourceLocation s, std::string i, std::vector<ExprNode*> a):
        ExprNode(s), intrinsic(std::move(i)), argList(std::move(a)) {}

    virtual IntrinsicCallNode *clone() const override {
        std::vector<ExprNode*> newArgList;
        for (auto a : argList) {
            newArgList.push_back(a->clone());
        }
        auto inode = new IntrinsicCallNode(sl, intrinsic, std::move(newArgList));
        inode->freeVars = freeVars;
        inode->tail = tail;
        return inode;
    }
    virtual void traverse(
        TraversalMode mode,
        std::function<void(ExprNode*)> &callback
    ) override {
        if (mode == TraversalMode::TOP_DOWN) {
            callback(this);
            _traverseSubtree(mode, callback);
        } else {
            _traverseSubtree(mode, callback);
            callback(this);
        }
    }
    virtual std::string toString() const override {
        std::string ret = "(" + intrinsic;
        for (auto a : argList) {
            ret += " ";
            ret += a->toString();
        }
        ret += ")";
        return ret;
    }
    virtual void computeFreeVars() override {
        for (auto a : argList) {
            a->computeFreeVars();
            freeVars.insert(a->freeVars.begin(), a->freeVars.end());
        }
    }
    virtual void computeTail(bool parentTail) override {
        tail = parentTail;
        for (auto a : argList) {
            a->computeTail(false);
        }
    }

    std::string intrinsic;
    std::vector<ExprNode*> argList;
private:
    void _traverseSubtree(TraversalMode mode, std::function<void(ExprNode*)> &callback) {
        for (auto a : argList) {
            a->traverse(mode, callback);
        }
    }
};

struct ExprCallNode : public ExprNode {
    DELETE_COPY(ExprCallNode);
    virtual ~ExprCallNode() {
        delete expr;
        for (auto a : argList) {
            delete a;
        }
    }
    ExprCallNode(SourceLocation s, ExprNode *e, std::vector<ExprNode*> a):
        ExprNode(s), expr(e), argList(std::move(a)) {}

    virtual ExprCallNode *clone() const override {
        ExprNode *newExpr = expr->clone();
        std::vector<ExprNode*> newArgList;
        for (auto a : argList) {
            newArgList.push_back(a->clone());
        }
        auto enode = new ExprCallNode(sl, newExpr, std::move(newArgList));
        enode->freeVars = freeVars;
        enode->tail = tail;
        return enode;
    }
    virtual void traverse(
        TraversalMode mode,
        std::function<void(ExprNode*)> &callback
    ) override {
        if (mode == TraversalMode::TOP_DOWN) {
            callback(this);
            _traverseSubtree(mode, callback);
        } else {
            _traverseSubtree(mode, callback);
            callback(this);
        }
    }
    virtual std::string toString() const override {
        std::string ret = "(" + expr->toString();
        for (auto a : argList) {
            ret += " ";
            ret += a->toString();
        }
        ret += ")";
        return ret;
    }
    virtual void computeFreeVars() override {
        expr->computeFreeVars();
        freeVars.insert(expr->freeVars.begin(), expr->freeVars.end());
        for (auto a : argList) {
            a->computeFreeVars();
            freeVars.insert(a->freeVars.begin(), a->freeVars.end());
        }
    }
    virtual void computeTail(bool parentTail) override {
        tail = parentTail;
        expr->computeTail(false);
        for (auto a : argList) {
            a->computeTail(false);
        }
    }

    ExprNode *expr;
    std::vector<ExprNode*> argList;
private:
    void _traverseSubtree(TraversalMode mode, std::function<void(ExprNode*)> &callback) {
        expr->traverse(mode, callback);
        for (auto a : argList) {
            a->traverse(mode, callback);
        }
    }
};

struct AtNode : public ExprNode {
    DELETE_COPY(AtNode);
    virtual ~AtNode() {
        delete var;
        delete expr;
    }
    AtNode(SourceLocation s, VariableNode *v, ExprNode *e): ExprNode(s), var(v), expr(e) {}

    virtual AtNode *clone() const override {
        // the evaluation order of the two clones are irrelevant
        auto anode = new AtNode(sl, var->clone(), expr->clone());
        anode->freeVars = freeVars;
        anode->tail = tail;
        return anode;
    }
    virtual void traverse(
        TraversalMode mode,
        std::function<void(ExprNode*)> &callback
    ) override {
        if (mode == TraversalMode::TOP_DOWN) {
            callback(this);
            _traverseSubtree(mode, callback);
        } else {
            _traverseSubtree(mode, callback);
            callback(this);
        }
    }
    virtual std::string toString() const override {
        return "@ " + var->toString() + " " + expr->toString();
    }
    virtual void computeFreeVars() override {
        expr->computeFreeVars();
        freeVars.insert(expr->freeVars.begin(), expr->freeVars.end());
    }
    virtual void computeTail(bool parentTail) override {
        tail = parentTail;
        var->computeTail(false);
        expr->computeTail(false);
    }

    VariableNode *var;
    ExprNode *expr;
private:
    void _traverseSubtree(TraversalMode mode, std::function<void(ExprNode*)> &callback) {
        var->traverse(mode, callback);
        expr->traverse(mode, callback);
    }
};

#undef DELETE_COPY

ExprNode *parse(std::deque<Token> tokens) {
    auto isIntegerToken = [](const Token &token) {
        return token.text.size() > 0 && (
            std::isdigit(token.text[0]) ||
            token.text[0] == '-' ||
            token.text[0] == '+'
        );
    };
    auto isStringToken = [](const Token &token) {
        return token.text.size() > 0 && token.text[0] == '"';
    };
    auto isIntrinsicToken = [](const Token &token) {
        return token.text.size() > 0 && token.text[0] == '.';
    };
    auto isVariableToken = [](const Token &token) {
        return token.text.size() > 0 && (std::isalpha(token.text[0]) || token.text[0] == '_');
    };
    auto isTheToken = [](const std::string &s) {
        return [s](const Token &token) {
            return token.text == s;
        };
    };
    auto consume = [&tokens]<typename Callable>(const Callable &predicate) -> Token {
        if (tokens.size() == 0) {
            panic("parser", "incomplete token stream");
        }
        auto token = tokens.front();
        tokens.pop_front();
        if (!predicate(token)) {
            panic("parser", "unexpected token", token.sl);
        }
        return token;
    };

    std::function<IntegerNode*()> parseInteger;
    std::function<StringNode*()> parseString;
    std::function<VariableNode*()> parseVariable;
    std::function<LambdaNode*()> parseLambda;
    std::function<LetrecNode*()> parseLetrec;
    std::function<IfNode*()> parseIf;
    std::function<SequenceNode*()> parseSequence;
    std::function<IntrinsicCallNode*()> parseIntrinsicCall;
    std::function<ExprCallNode*()> parseExprCall;
    std::function<AtNode*()> parseAt;
    std::function<ExprNode*()> parseExpr;

    parseInteger = [&]() -> IntegerNode* {
        auto token = consume(isIntegerToken);
        return new IntegerNode(token.sl, token.text);
    };
    parseString = [&]() -> StringNode* {  // don't unquote here: AST keeps raw tokens
        auto token = consume(isStringToken);
        return new StringNode(token.sl, token.text);
    };
    parseVariable = [&]() -> VariableNode* {
        auto token = consume(isVariableToken);
        return new VariableNode(token.sl, std::move(token.text));
    };
    parseLambda = [&]() -> LambdaNode* {
        auto start = consume(isTheToken("lambda"));
        consume(isTheToken("("));
        std::vector<VariableNode*> varList;
        while (tokens.size() && isVariableToken(tokens[0])) {
            varList.push_back(parseVariable());
        }
        consume(isTheToken(")"));
        auto expr = parseExpr();
        return new LambdaNode(start.sl, std::move(varList), expr);
    };
    parseLetrec = [&]() -> LetrecNode* {
        auto start = consume(isTheToken("letrec"));
        consume(isTheToken("("));
        std::vector<std::pair<VariableNode*, ExprNode*>> varExprList;
        while (tokens.size() && isVariableToken(tokens[0])) {
            // enforce the evaluation order of v; e
            auto v = parseVariable();
            auto e = parseExpr();
            varExprList.emplace_back(v, e);
        }
        consume(isTheToken(")"));
        auto expr = parseExpr();
        return new LetrecNode(start.sl, std::move(varExprList), expr);
    };
    parseIf = [&]() -> IfNode* {
        auto start = consume(isTheToken("if"));
        auto cond = parseExpr();
        auto branch1 = parseExpr();
        auto branch2 = parseExpr();
        return new IfNode(start.sl, cond, branch1, branch2);
    };
    parseSequence = [&]() -> SequenceNode* {
        auto start = consume(isTheToken("{"));
        std::vector<ExprNode*> exprList;
        while (tokens.size() && tokens[0].text != "}") {
            exprList.push_back(parseExpr());
        }
        if (!exprList.size()) {
            panic("parser", "zero-length sequence", start.sl);
        }
        consume(isTheToken("}"));
        return new SequenceNode(start.sl, std::move(exprList));
    };
    parseIntrinsicCall = [&]() -> IntrinsicCallNode* {
        auto start = consume(isTheToken("("));
        auto intrinsic = consume(isIntrinsicToken);
        std::vector<ExprNode*> argList;
        while (tokens.size() && tokens[0].text != ")") {
            argList.push_back(parseExpr());
        }
        consume(isTheToken(")"));
        return new IntrinsicCallNode(start.sl, std::move(intrinsic.text), std::move(argList));
    };
    parseExprCall = [&]() -> ExprCallNode* {
        auto start = consume(isTheToken("("));
        auto expr = parseExpr();
        std::vector<ExprNode*> argList;
        while (tokens.size() && tokens[0].text != ")") {
            argList.push_back(parseExpr());
        }
        consume(isTheToken(")"));
        return new ExprCallNode(start.sl, expr, std::move(argList));
    };
    parseAt = [&]() -> AtNode* {
        auto start = consume(isTheToken("@"));
        auto var = parseVariable();
        auto expr = parseExpr();
        return new AtNode(start.sl, var, expr);
    };
    parseExpr = [&]() -> ExprNode* {
        if (!tokens.size()) {
            panic("parser", "incomplete token stream");
            // unreachable
            return nullptr;
        } else if (isIntegerToken(tokens[0])) {
            return parseInteger();
        } else if (isStringToken(tokens[0])) {
            return parseString();
        } else if (tokens[0].text == "lambda") {
            return parseLambda();
        } else if (tokens[0].text == "letrec") {
            return parseLetrec();
        } else if (tokens[0].text == "if") {
            return parseIf();
        // check keywords before var to avoid recognizing keywords as vars
        } else if (isVariableToken(tokens[0])) {
            return parseVariable();
        } else if (tokens[0].text == "{") {
            return parseSequence();
        } else if (tokens[0].text == "(") {
            if (tokens.size() < 2) {
                panic("parser", "incomplete token stream");
                // unreachable
                return nullptr;
            }
            if (isIntrinsicToken(tokens[1])) {
                return parseIntrinsicCall();
            } else {
                return parseExprCall();
            }
        } else if (tokens[0].text == "@") {
            return parseAt();
        } else {
            panic("parser", "unrecognized token", tokens[0].sl);
            // unreachable
            return nullptr;
        }
    };

    auto expr = parseExpr();
    return expr;
}

std::vector<int> encodeNodePath(const ExprNode *node, const ExprNode *root) {
    if (node == nullptr) {
        panic("serialization", "node is nullptr");
        // unreachable
        return std::vector<int>();
    }
    if (root == nullptr) {
        panic("serialization", "root is nullptr");
        // unreachable
        return std::vector<int>();
    }
    std::vector<int> nodePath;
    std::function<bool(std::vector<int>&, const ExprNode*)>
        findPath = [&](std::vector<int> &curPath, const ExprNode *curNode) -> bool {
        if (curNode == node) {
            // copy
            nodePath = curPath;
            return true;
        } else {
            if (auto inode = dynamic_cast<const IntegerNode*>(curNode)) {
                return false;
            } else if (auto snode = dynamic_cast<const StringNode*>(curNode)) {
                return false;
            } else if (auto vnode = dynamic_cast<const VariableNode*>(curNode)) {
                return false;
            } else if (auto lnode = dynamic_cast<const LambdaNode*>(curNode)) {
                int numArgs = lnode->varList.size();
                for (int i = 0; i < numArgs; i++) {
                    curPath.push_back(i);
                    if (findPath(curPath, lnode->varList[i])) {
                        return true;
                    }
                    curPath.pop_back();
                }
                curPath.push_back(numArgs);
                if (findPath(curPath, lnode->expr)) {
                    return true;
                }
                curPath.pop_back();
                return false;
            } else if (auto lnode = dynamic_cast<const LetrecNode*>(curNode)) {
                int numVars = lnode->varExprList.size();
                for (int i = 0; i < numVars; i++) {
                    curPath.push_back(2 * i);
                    if (findPath(curPath, lnode->varExprList[i].first)) {
                        return true;
                    }
                    curPath.pop_back();
                    curPath.push_back(2 * i + 1);
                    if (findPath(curPath, lnode->varExprList[i].second)) {
                        return true;
                    }
                    curPath.pop_back();
                }
                curPath.push_back(2 * numVars);
                if (findPath(curPath, lnode->expr)) {
                    return true;
                }
                curPath.pop_back();
                return false;
            } else if (auto inode = dynamic_cast<const IfNode*>(curNode)) {
                curPath.push_back(0);
                if (findPath(curPath, inode->cond)) {
                    return true;
                }
                curPath.pop_back();
                curPath.push_back(1);
                if (findPath(curPath, inode->branch1)) {
                    return true;
                }
                curPath.pop_back();
                curPath.push_back(2);
                if (findPath(curPath, inode->branch2)) {
                    return true;
                }
                curPath.pop_back();
                return false;
            } else if (auto snode = dynamic_cast<const SequenceNode*>(curNode)) {
                int numExprs = snode->exprList.size();
                for (int i = 0; i < numExprs; i++) {
                    curPath.push_back(i);
                    if (findPath(curPath, snode->exprList[i])) {
                        return true;
                    }
                    curPath.pop_back();
                }
                return false;
            } else if (auto inode = dynamic_cast<const IntrinsicCallNode*>(curNode)) {
                int numArgs = inode->argList.size();
                for (int i = 0; i < numArgs; i++) {
                    curPath.push_back(i);
                    if (findPath(curPath, inode->argList[i])) {
                        return true;
                    }
                    curPath.pop_back();
                }
                return false;
            } else if (auto enode = dynamic_cast<const ExprCallNode*>(curNode)) {
                curPath.push_back(0);
                if (findPath(curPath, enode->expr)) {
                    return true;
                }
                curPath.pop_back();
                int numArgs = enode->argList.size();
                for (int i = 0; i < numArgs; i++) {
                    curPath.push_back(i + 1);
                    if (findPath(curPath, enode->argList[i])) {
                        return true;
                    }
                    curPath.pop_back();
                }
                return false;
            } else if (auto anode = dynamic_cast<const AtNode*>(curNode)) {
                curPath.push_back(0);
                if (findPath(curPath, anode->var)) {
                    return true;
                }
                curPath.pop_back();
                curPath.push_back(1);
                if (findPath(curPath, anode->expr)) {
                    return true;
                }
                curPath.pop_back();
                return false;
            } else {
                panic("serialization", "unrecognized AST node");
                // unreachable
                return false;
            }
        }
    };
    std::vector<int> currentPath;
    if (findPath(currentPath, root)) {
        return nodePath;
    } else {
        panic("serialization", "node not found", node->sl);
        // unreachable
        return std::vector<int>();
    }
}

const ExprNode *decodeNodePath(const std::vector<int> &path, const ExprNode *root) {
    const ExprNode *curNode = root;
    for (int i : path) {
        if (auto inode = dynamic_cast<const IntegerNode*>(curNode)) {
            return nullptr;
        } else if (auto snode = dynamic_cast<const StringNode*>(curNode)) {
            return nullptr;
        } else if (auto vnode = dynamic_cast<const VariableNode*>(curNode)) {
            return nullptr;
        } else if (auto lnode = dynamic_cast<const LambdaNode*>(curNode)) {
            int numArgs = lnode->varList.size();
            if (i < numArgs) {
                curNode = lnode->varList[i];
            } else if (i == numArgs) {
                curNode = lnode->expr;
            } else {
                return nullptr;
            }
        } else if (auto lnode = dynamic_cast<const LetrecNode*>(curNode)) {
            int numVars = lnode->varExprList.size();
            if (i < numVars * 2) {
                int index = i / 2;
                int offset = i % 2;
                if (offset == 0) {
                    curNode = lnode->varExprList[index].first;
                } else {
                    curNode = lnode->varExprList[index].second;
                }
            } else if (i == numVars * 2) {
                curNode = lnode->expr;
            } else {
                return nullptr;
            }
        } else if (auto inode = dynamic_cast<const IfNode*>(curNode)) {
            if (i == 0) {
                curNode = inode->cond;
            } else if (i == 1) {
                curNode = inode->branch1;
            } else if (i == 2) {
                curNode = inode->branch2;
            } else {
                return nullptr;
            }
        } else if (auto snode = dynamic_cast<const SequenceNode*>(curNode)) {
            int numExprs = snode->exprList.size();
            if (i < numExprs) {
                curNode = snode->exprList[i];
            } else {
                return nullptr;
            }
        } else if (auto inode = dynamic_cast<const IntrinsicCallNode*>(curNode)) {
            int numArgs = inode->argList.size();
            if (i < numArgs) {
                curNode = inode->argList[i];
            } else {
                return nullptr;
            }
        } else if (auto enode = dynamic_cast<const ExprCallNode*>(curNode)) {
            if (i == 0) {
                curNode = enode->expr;
            } else {
                int numArgs = enode->argList.size();
                if (i <= numArgs) {
                    curNode = enode->argList[i - 1];
                } else {
                    return nullptr;
                }
            }
        } else if (auto anode = dynamic_cast<const AtNode*>(curNode)) {
            if (i == 0) {
                curNode = anode->var;
            } else if (i == 1) {
                curNode = anode->expr;
            } else {
                return nullptr;
            }
        } else {
            panic("serialization", "unrecognized AST node");
        }
    }
    return curNode;
}

using Path = std::vector<int>;

std::string pathToString(const Path &path) {
    std::string s = "[";
    for (int i : path) {
        s += " ";
        s += std::to_string(i);
    }
    s += " ]";
    return s;
}

Path stringToPath(const std::string &s) {
    // TODO
    return Path();
}

// ------------------------------
// runtime
// ------------------------------

struct Void {
    Void() = default;

    std::string toString() const {
        return "<void>";
    }
};

struct Integer {
    Integer(int v): value(v) {}

    std::string toString() const {
        return std::to_string(value);
    }

    int value = 0;
};

struct String {  // for string literals, this class contains the unquoted ones
    String(std::string v): value(std::move(v)) {}

    std::string toString() const {
        return quote(value);
    }

    std::string value;
};

// variable environment; newer variables have larger indices
using Env = std::vector<std::pair<std::string, Location>>;

std::string envToString(const Env &env) {
    std::string s = "[";
    for (auto &p : env) {
        s += " ";
        s += p.first;
        s += " ";
        s += std::to_string(p.second);
    }
    s += " ]";
    return s;
}

Env stringToEnv(const std::string &s) {
    // TODO
    return Env();
}

std::optional<Location> lookup(const std::string &name, const Env &env) {
    for (auto p = env.rbegin(); p != env.rend(); p++) {
        if (p->first == name) {
            return p->second;
        }
    }
    return std::nullopt;
}

struct Closure {
    // a closure should copy its environment
    Closure(Env e, const LambdaNode *f): env(std::move(e)), fun(f) {}

    std::string toString(const ExprNode *root) const {
        std::string s = envToString(env);
        s += " ";
        s += pathToString(encodeNodePath(fun, root));
        return s;
    }

    Env env;
    const LambdaNode *fun;
};

using Value = std::variant<Void, Integer, String, Closure>;

std::string valueToString(const Value &v, const ExprNode *root) {
    if (std::holds_alternative<Void>(v)) {
        return std::get<Void>(v).toString();
    } else if (std::holds_alternative<Integer>(v)) {
        return std::get<Integer>(v).toString();
    } else if (std::holds_alternative<String>(v)) {
        return std::get<String>(v).toString();
    } else {
        return std::get<Closure>(v).toString(root);
    }
}

Value stringToValue(const std::string &s) {
    // TODO
    return Void();
}

// stack layer

struct Layer {
    // a default argument is evaluated each time the function is called without
    // that argument (not important here)
    Layer(std::shared_ptr<Env> e, const ExprNode *x, bool f = false):
        env(std::move(e)), expr(x), frame(f) {}
    
    Layer(std::string) {
        // TODO: deserialize
    }

    std::string serialize(const ExprNode *root) const {
        std::string serialized_layer;
        // bool frame;
        if (frame) {
            serialized_layer += "frame";
        } else {
            serialized_layer += "nonframe";
        }
        // std::shared_ptr<Env> env;
        if (frame) {
            serialized_layer += " ";
            serialized_layer += envToString(*env);
        }
        // const ExprNode *expr;
        serialized_layer += " ";
        if (expr != nullptr) {
            serialized_layer += pathToString(encodeNodePath(expr, root));
        } else {
            serialized_layer += "[ -1 ]";
        }
        // int pc = 0;
        serialized_layer += " ";
        serialized_layer += std::to_string(pc);
        // std::vector<Location> local;
        serialized_layer += " [";
        for (auto &l : local) {
            serialized_layer += " ";
            serialized_layer += std::to_string(l);
        }
        serialized_layer += " ]";
        return serialized_layer;
    }

    // whether this is a frame
    bool frame;
    // one env per frame (closure call layer)
    std::shared_ptr<Env> env;
    const ExprNode *expr;
    // program counter inside this expr
    int pc = 0;
    // temporary local information for evaluation
    std::vector<Location> local;
};

class State {
public:
    State(std::string src) {
        source = std::move(src);
        // parsing and static analysis (TODO: exceptions?)
        expr = parse(lex(source));
        std::function<void(ExprNode*)> checkDuplicate = [](ExprNode *e) -> void {
            if (auto lnode = dynamic_cast<LambdaNode*>(e)) {
                std::unordered_set<std::string> varNames;
                for (auto var : lnode->varList) {
                    if (varNames.contains(var->name)) {
                        panic("sema", "duplicate parameter names", lnode->sl);
                    }
                    varNames.insert(var->name);
                }
            } else if (auto lnode = dynamic_cast<LetrecNode*>(e)) {
                std::unordered_set<std::string> varNames;
                for (const auto &ve : lnode->varExprList) {
                    if (varNames.contains(ve.first->name)) {
                        panic("sema", "duplicate binding names", lnode->sl);
                    }
                    varNames.insert(ve.first->name);
                }
            }
        };
        expr->traverse(TraversalMode::TOP_DOWN, checkDuplicate);
        expr->computeFreeVars();
        expr->computeTail(false);
        // pre-allocate integer literals and string literals
        std::function<void(ExprNode*)> preAllocate = [this](ExprNode *e) -> void {
            if (auto inode = dynamic_cast<IntegerNode*>(e)) {
                // TODO: exceptions
                inode->loc = this->_new<Integer>(std::stoi(inode->val));
            } else if (auto snode = dynamic_cast<StringNode*>(e)) {
                snode->loc = this->_new<String>(unquote(snode->val));
            }
        };
        expr->traverse(TraversalMode::TOP_DOWN, preAllocate);
        numLiterals = heap.size();
        // the main frame (which cannot be removed by TCO)
        stack.emplace_back(std::make_shared<Env>(), nullptr, true);
        // the first expression (using the env of the main frame)
        stack.emplace_back(stack.back().env, expr);
    }
    State(const State &state):
        source(state.source),
        expr(state.expr->clone()),
        stack(state.stack),
        heap(state.heap),
        numLiterals(state.numLiterals),
        resultLoc(state.resultLoc) {
    }
    State &operator=(const State &state) {
        if (this != &state) {
            source = state.source;
            delete expr;
            expr = state.expr->clone();
            stack = state.stack;
            heap = state.heap;
            numLiterals = state.numLiterals;
            resultLoc = state.resultLoc;
        }
        return *this;
    }
    State(State &&state):
        source(std::move(state.source)),
        expr(state.expr),
        stack(std::move(state.stack)),
        heap(std::move(state.heap)),
        numLiterals(state.numLiterals),
        resultLoc(state.resultLoc) {
        state.expr = nullptr;
    }
    State &operator=(State &&state) {
        if (this != &state) {
            source = std::move(state.source);
            delete expr;
            expr = state.expr;
            state.expr = nullptr;
            stack = std::move(state.stack);
            heap = std::move(state.heap);
            numLiterals = state.numLiterals;
            resultLoc = state.resultLoc;
        }
        return *this;
    }
    ~State() {
        if (expr != nullptr) {
            delete expr;
        }
    }

    // returns true iff the step is completed without reaching the end of evaluation
    bool step() {
        // be careful! this reference may be invalidated after modifying the stack
        // so always keep stack change as the last operation(s)
        auto &layer = stack.back();
        // main frame; end of evaluation
        if (layer.expr == nullptr) {
            return false;
        }
        // evaluations for every case
        if (auto inode = dynamic_cast<const IntegerNode*>(layer.expr)) {
            resultLoc = inode->loc;
            stack.pop_back();
        } else if (auto snode = dynamic_cast<const StringNode*>(layer.expr)) {
            resultLoc = snode->loc;
            stack.pop_back();
        } else if (auto vnode = dynamic_cast<const VariableNode*>(layer.expr)) {
            auto varName = vnode->name;
            auto loc = lookup(varName, *(layer.env));
            if (!loc.has_value()) {
                _errorStack();
                panic("runtime", "undefined variable " + varName, layer.expr->sl);
            }
            resultLoc = loc.value();
            stack.pop_back();
        } else if (auto lnode = dynamic_cast<const LambdaNode*>(layer.expr)) {
            // copy the statically used part of the env into the closure
            Env savedEnv;
            // copy
            auto usedVars = lnode->freeVars;
            for (auto ptr = layer.env->rbegin(); ptr != layer.env->rend(); ptr++) {
                if (usedVars.empty()) {
                    break;
                }
                if (usedVars.contains(ptr->first)) {
                    savedEnv.push_back(*ptr);
                    usedVars.erase(ptr->first);
                }
            }
            std::reverse(savedEnv.begin(), savedEnv.end());
            resultLoc = _new<Closure>(savedEnv, lnode);
            stack.pop_back();
        } else if (auto lnode = dynamic_cast<const LetrecNode*>(layer.expr)) {
            // unified argument recording
            if (layer.pc > 1 && layer.pc <= static_cast<int>(lnode->varExprList.size()) + 1) {
                auto varName = lnode->varExprList[layer.pc - 2].first->name;
                auto loc = lookup(
                    varName,
                    *(layer.env)
                );
                // this shouldn't happen since those variables are newly introduced by letrec
                if (!loc.has_value()) {
                    _errorStack();
                    panic("runtime", "undefined variable " + varName, layer.expr->sl);
                }
                // copy (inherited resultLoc)
                heap[loc.value()] = heap[resultLoc];
            }
            // create all new locations
            if (layer.pc == 0) {
                layer.pc++;
                for (const auto &[var, _] : lnode->varExprList) {
                    layer.env->push_back(std::make_pair(
                        var->name,
                        _new<Void>()
                    ));
                }
            // evaluate bindings
            } else if (layer.pc <= static_cast<int>(lnode->varExprList.size())) {
                layer.pc++;
                // note: growing the stack might invalidate the reference "layer"
                //       but this is fine since next time "layer" will be re-bound
                stack.emplace_back(
                    layer.env,
                    lnode->varExprList[layer.pc - 2].second
                );
            // evaluate body
            } else if (layer.pc == static_cast<int>(lnode->varExprList.size()) + 1) {
                layer.pc++;
                stack.emplace_back(
                    layer.env,
                    lnode->expr
                );
            // finish letrec
            } else {
                int nParams = lnode->varExprList.size();
                for (int i = 0; i < nParams; i++) {
                    layer.env->pop_back();
                }
                // this layer cannot be optimized by TCO because we need nParams to revert env
                // no need to update resultLoc: inherited from body evaluation
                stack.pop_back();
            }
        } else if (auto inode = dynamic_cast<const IfNode*>(layer.expr)) {
            // evaluate condition
            if (layer.pc == 0) {
                layer.pc++;
                stack.emplace_back(layer.env, inode->cond);
            // evaluate one branch
            } else if (layer.pc == 1) {
                layer.pc++;
                // inherited condition value
                if (!std::holds_alternative<Integer>(heap[resultLoc])) {
                    _errorStack();
                    panic("runtime", "wrong cond type", layer.expr->sl);
                }
                if (std::get<Integer>(heap[resultLoc]).value) {
                    stack.emplace_back(layer.env, inode->branch1);
                } else {
                    stack.emplace_back(layer.env, inode->branch2);
                }
            // finish if
            } else {
                // no need to update resultLoc: inherited
                stack.pop_back();
            }
        } else if (auto snode = dynamic_cast<const SequenceNode*>(layer.expr)) {
            // evaluate one-by-one
            if (layer.pc < static_cast<int>(snode->exprList.size())) {
                layer.pc++;
                stack.emplace_back(
                    layer.env,
                    snode->exprList[layer.pc - 1]
                );
            // finish
            } else {
                // sequence's value is the last expression's value
                // no need to update resultLoc: inherited
                stack.pop_back();
            }
        } else if (auto inode = dynamic_cast<const IntrinsicCallNode*>(layer.expr)) {
            // unified argument recording
            if (layer.pc > 0 && layer.pc <= static_cast<int>(inode->argList.size())) {
                layer.local.push_back(resultLoc);
            }
            // evaluate arguments
            if (layer.pc < static_cast<int>(inode->argList.size())) {
                layer.pc++;
                stack.emplace_back(
                    layer.env,
                    inode->argList[layer.pc - 1]
                );
            // intrinsic call doesn't grow the stack
            } else {
                auto value = _callIntrinsic(
                    layer.expr->sl,
                    inode->intrinsic,
                    // intrinsic call is pass by reference
                    layer.local
                );
                resultLoc = _moveNew(std::move(value));
                stack.pop_back();
            }
        } else if (auto enode = dynamic_cast<const ExprCallNode*>(layer.expr)) {
            // unified argument recording
            if (layer.pc > 2 && layer.pc <= static_cast<int>(enode->argList.size()) + 2) {
                layer.local.push_back(resultLoc);
            }
            // evaluate the callee
            if (layer.pc == 0) {
                layer.pc++;
                stack.emplace_back(
                    layer.env,
                    enode->expr
                );
            // initialization
            } else if (layer.pc == 1) {
                layer.pc++;
                // inherited callee location
                layer.local.push_back(resultLoc);
            // evaluate arguments
            } else if (layer.pc <= static_cast<int>(enode->argList.size()) + 1) {
                layer.pc++;
                stack.emplace_back(
                    layer.env,
                    enode->argList[layer.pc - 3]
                );
            // call
            } else if (layer.pc == static_cast<int>(enode->argList.size()) + 2) {
                layer.pc++;
                auto exprLoc = layer.local[0];
                if (!std::holds_alternative<Closure>(heap[exprLoc])) {
                    _errorStack();
                    panic("runtime", "calling a non-callable", layer.expr->sl);
                }
                auto &closure = std::get<Closure>(heap[exprLoc]);
                // types will be checked inside the closure call
                if (
                    static_cast<int>(layer.local.size()) - 1 !=
                    static_cast<int>(closure.fun->varList.size())
                ) {
                    _errorStack();
                    panic("runtime", "wrong number of arguments", layer.expr->sl);
                }
                int nArgs = static_cast<int>(closure.fun->varList.size());
                // lexical scope: copy the env from the closure definition place
                auto newEnv = closure.env;
                for (int i = 0; i < nArgs; i++) {
                    // closure call is pass by reference
                    newEnv.push_back(std::make_pair(
                        closure.fun->varList[i]->name,
                        layer.local[i + 1]
                    ));
                }
                // tail call optimization
                if (enode->tail) {
                    while (!(stack.back().frame)) {
                        stack.pop_back();
                    }
                    // pop the frame
                    stack.pop_back();
                }
                // evaluation of the closure body
                stack.emplace_back(
                    // new frame has new env
                    std::make_shared<Env>(std::move(newEnv)),
                    closure.fun->expr,
                    true
                );
            // finish
            } else {
                // no need to update resultLoc: inherited
                stack.pop_back();
            }
        } else if (auto anode = dynamic_cast<const AtNode*>(layer.expr)) {
            // evaluate the expr
            if (layer.pc == 0) {
                layer.pc++;
                stack.emplace_back(layer.env, anode->expr);
            } else {
                // inherited resultLoc
                if (!std::holds_alternative<Closure>(heap[resultLoc])) {
                    _errorStack();
                    panic("runtime", "@ wrong type", layer.expr->sl);
                }
                auto varName = anode->var->name;
                auto loc = lookup(
                    varName,
                    std::get<Closure>(heap[resultLoc]).env
                );
                if (!loc.has_value()) {
                    _errorStack();
                    panic("runtime", "undefined variable " + varName, layer.expr->sl);
                }
                // "access by reference"
                resultLoc = loc.value();
                stack.pop_back();
            }
        } else {
            _errorStack();
            panic("runtime", "unrecognized AST node", layer.expr->sl);
        }
        return true;
    }
    void execute() {
        // can choose different initial values here
        int gc_threshold = numLiterals + 64;
        while (step()) {
            int total = heap.size();
            if (total > gc_threshold) {
                int removed = _gc();
                int live = total - removed;
                // see also "Optimal heap limits for reducing browser memory use" (OOPSLA 2022)
                // for the square root solution
                gc_threshold = live * 2;
            }
        }
    }
    const Value &getResult() const {
        return heap[resultLoc];
    }
    const ExprNode *getExpr() const {
        return expr;
    }
    std::string serialize() const {
        std::string serialized_state;
        // std::string source;
        serialized_state += source;
        // (skipped, because source is more accurate) ExprNode *expr;
        // std::vector<Layer> stack;
        serialized_state += " [";
        for (auto &layer : stack) {
            serialized_state += " ";
            serialized_state += layer.serialize(expr);
        }
        serialized_state += " ]";
        // std::vector<Value> heap;
        serialized_state += " [";
        for (auto &value : heap) {
            serialized_state += " ";
            serialized_state += valueToString(value, expr);
        }
        serialized_state += " ]";
        // int numLiterals = 0;
        serialized_state += " ";
        serialized_state += std::to_string(numLiterals);
        // Location resultLoc;
        serialized_state += " ";
        serialized_state += std::to_string(resultLoc);
        return serialized_state;
    }
private:
    template <typename... Alt>
    requires (true && ... && (std::same_as<Alt, Value> || isAlternativeOf<Alt, Value>))
    void _typecheck(SourceLocation sl, const std::vector<Location> &args) {
        bool ok = args.size() == sizeof...(Alt);
        int i = -1;
        ok = ok && (true && ... && (
            i++,
            [&] {
                if constexpr (std::same_as<Alt, Value>) {
                    return true;
                } else {
                    return std::holds_alternative<Alt>(heap[args[i]]);
                }
            } ()
        ));
        if (!ok) {
            _errorStack();
            panic("runtime", "type error on intrinsic call", sl);
        }
    }
    // intrinsic dispatch
    Value _callIntrinsic(
        SourceLocation sl, const std::string &name, const std::vector<Location> &args
    ) {
        if (name == ".void") {
            _typecheck<>(sl, args);
            return Void();
        } else if (name == ".+") {
            _typecheck<Integer, Integer>(sl, args);
            return Integer(
                std::get<Integer>(heap[args[0]]).value +
                std::get<Integer>(heap[args[1]]).value
            );
        } else if (name == ".-") {
            _typecheck<Integer, Integer>(sl, args);
            return Integer(
                std::get<Integer>(heap[args[0]]).value -
                std::get<Integer>(heap[args[1]]).value
            );
        } else if (name == ".*") {
            _typecheck<Integer, Integer>(sl, args);
            return Integer(
                std::get<Integer>(heap[args[0]]).value *
                std::get<Integer>(heap[args[1]]).value
            );
        } else if (name == "./") {
            _typecheck<Integer, Integer>(sl, args);
            int d = std::get<Integer>(heap[args[1]]).value;
            if (d == 0) {
                panic("runtime", "division by zero", sl);
            }
            return Integer(
                std::get<Integer>(heap[args[0]]).value /
                d
            );
        } else if (name == ".%") {
            _typecheck<Integer, Integer>(sl, args);
            int d = std::get<Integer>(heap[args[1]]).value;
            if (d == 0) {
                panic("runtime", "division by zero", sl);
            }
            return Integer(
                std::get<Integer>(heap[args[0]]).value %
                d
            );
        } else if (name == ".<") {
            _typecheck<Integer, Integer>(sl, args);
            return Integer(
                std::get<Integer>(heap[args[0]]).value <
                std::get<Integer>(heap[args[1]]).value ? 1 : 0
            );
        } else if (name == ".<=") {
            _typecheck<Integer, Integer>(sl, args);
            return Integer(
                std::get<Integer>(heap[args[0]]).value <=
                std::get<Integer>(heap[args[1]]).value ? 1 : 0
            );
        } else if (name == ".>") {
            _typecheck<Integer, Integer>(sl, args);
            return Integer(
                std::get<Integer>(heap[args[0]]).value >
                std::get<Integer>(heap[args[1]]).value ? 1 : 0
            );
        } else if (name == ".>=") {
            _typecheck<Integer, Integer>(sl, args);
            return Integer(
                std::get<Integer>(heap[args[0]]).value >=
                std::get<Integer>(heap[args[1]]).value ? 1 : 0
            );
        } else if (name == ".=") {
            _typecheck<Integer, Integer>(sl, args);
            return Integer(
                std::get<Integer>(heap[args[0]]).value ==
                std::get<Integer>(heap[args[1]]).value ? 1 : 0
            );
        } else if (name == "./=") {
            _typecheck<Integer, Integer>(sl, args);
            return Integer(
                std::get<Integer>(heap[args[0]]).value !=
                std::get<Integer>(heap[args[1]]).value ? 1 : 0
            );
        } else if (name == ".and") {
            _typecheck<Integer, Integer>(sl, args);
            return Integer(
                std::get<Integer>(heap[args[0]]).value &&
                std::get<Integer>(heap[args[1]]).value ? 1 : 0
            );
        } else if (name == ".or") {
            _typecheck<Integer, Integer>(sl, args);
            return Integer(
                std::get<Integer>(heap[args[0]]).value ||
                std::get<Integer>(heap[args[1]]).value ? 1 : 0
            );
        } else if (name == ".not") {
            _typecheck<Integer>(sl, args);
            return Integer(
                std::get<Integer>(heap[args[0]]).value ? 0 : 1
            );
        } else if (name == ".s+") {
            _typecheck<String, String>(sl, args);
            return String(
                std::get<String>(heap[args[0]]).value +
                std::get<String>(heap[args[1]]).value
            );
        } else if (name == ".s<") {
            _typecheck<String, String>(sl, args);
            return Integer(
                std::get<String>(heap[args[0]]).value <
                std::get<String>(heap[args[1]]).value ? 1 : 0
            );
        } else if (name == ".s<=") {
            _typecheck<String, String>(sl, args);
            return Integer(
                std::get<String>(heap[args[0]]).value <=
                std::get<String>(heap[args[1]]).value ? 1 : 0
            );
        } else if (name == ".s>") {
            _typecheck<String, String>(sl, args);
            return Integer(
                std::get<String>(heap[args[0]]).value >
                std::get<String>(heap[args[1]]).value ? 1 : 0
            );
        } else if (name == ".s>=") {
            _typecheck<String, String>(sl, args);
            return Integer(
                std::get<String>(heap[args[0]]).value >=
                std::get<String>(heap[args[1]]).value ? 1 : 0
            );
        } else if (name == ".s=") {
            _typecheck<String, String>(sl, args);
            return Integer(
                std::get<String>(heap[args[0]]).value ==
                std::get<String>(heap[args[1]]).value ? 1 : 0
            );
        } else if (name == ".s/=") {
            _typecheck<String, String>(sl, args);
            return Integer(
                std::get<String>(heap[args[0]]).value !=
                std::get<String>(heap[args[1]]).value ? 1 : 0
            );
        } else if (name == ".s||") {
            _typecheck<String>(sl, args);
            return Integer(
                std::get<String>(heap[args[0]]).value.size()
            );
        } else if (name == ".s[]") {
            _typecheck<String, Integer, Integer>(sl, args);
            int n = std::get<String>(heap[args[0]]).value.size();
            int l = std::get<Integer>(heap[args[1]]).value;
            int r = std::get<Integer>(heap[args[2]]).value;
            if (!(
                (0 <= l && l < n) &&
                (0 <= r && r < n) &&
                (l <= r)
            )) {
                panic("runtime", "invalid substring range", sl);
            }
            return String(
                std::get<String>(heap[args[0]]).value.substr(l, r - l)
            );
        } else if (name == ".quote") {
            _typecheck<String>(sl, args);
            return String(
                quote(std::get<String>(heap[args[0]]).value)
            );
        } else if (name == ".unquote") {
            _typecheck<String>(sl, args);
            return String(
                unquote(std::get<String>(heap[args[0]]).value)
            );
        } else if (name == ".s->i") {
            _typecheck<String>(sl, args);
            return Integer(
                std::stoi(std::get<String>(heap[args[0]]).value)  // TODO: exceptions
            );
        } else if (name == ".i->s") {
            _typecheck<Integer>(sl, args);
            return String(
                std::to_string(std::get<Integer>(heap[args[0]]).value)
            );
        } else if (name == ".type") {
            _typecheck<Value>(sl, args);
            int label = -1;
            if (std::holds_alternative<Void>(heap[args[0]])) {
                label = 0;
            } else if (std::holds_alternative<Integer>(heap[args[0]])) {
                label = 1;
            } else {
                label = 2;
            }
            return Integer(label);
        } else if (name == ".eval") {
            _typecheck<String>(sl, args);
            State state(std::get<String>(heap[args[0]]).value);
            state.execute();
            return state.getResult();  // this should be a copy
        } else if (name == ".getchar") {
            _typecheck<>(sl, args);
            auto c = std::cin.get();
            if (std::cin.eof()) {
                return Void();
            } else {
                std::string s;
                s.push_back(static_cast<char>(c));
                return String(s);
            }
        } else if (name == ".getint") {
            _typecheck<>(sl, args);
            int v;
            if (std::cin >> v) {
                return Integer(v);
            } else {
                return Void();
            }
        } else if (name == ".putstr") {
            _typecheck<String>(sl, args);
            std::cout << std::get<String>(heap[args[0]]).value;
            return Void();
        } else if (name == ".flush") {
            _typecheck<>(sl, args);
            std::cout << std::flush;
            return Void();
        } else {
            _errorStack();
            panic("runtime", "unrecognized intrinsic call", sl);
            // unreachable
            return Void();
        }
    }
    // memory management
    template <typename V, typename... Args>
    requires isAlternativeOf<V, Value>
    Location _new(Args&&... args) {
        heap.push_back(std::move(V(std::forward<Args>(args)...)));
        return heap.size() - 1;
    }
    Location _moveNew(Value v) {
        heap.push_back(std::move(v));
        return heap.size() - 1;
    }
    std::unordered_set<Location> _mark() {
        std::unordered_set<Location> visited;
        // for each traversed location, specifically handle the closure case
        std::function<void(Location)> traverseLocation =
            // "this" captures the current object by reference
            [this, &visited, &traverseLocation](Location loc) {
            if (!(visited.contains(loc))) {
                visited.insert(loc);
                if (std::holds_alternative<Closure>(heap[loc])) {
                    for (const auto &[_, l] : std::get<Closure>(heap[loc]).env) {
                        traverseLocation(l);
                    }
                }
            }
        };
        // traverse the stack
        for (const auto &layer : stack) {
            // only frames "own" the environments
            if (layer.frame) {
                for (const auto &[_, loc] : (*(layer.env))) {
                    traverseLocation(loc);
                }
            }
            // but each layer can still have locals
            for (const auto v : layer.local) {
                traverseLocation(v);
            }
        }
        // traverse the resultLoc
        traverseLocation(resultLoc);
        return visited;
    }
    std::pair<int, std::unordered_map<Location, Location>>
        _sweepAndCompact(const std::unordered_set<Location> &visited) {
        std::unordered_map<Location, Location> relocation;
        Location n = heap.size();
        Location i{numLiterals}, j{numLiterals};
        while (j < n) {
            if (visited.contains(j)) {
                if (i < j) {
                    heap[i] = std::move(heap[j]);
                    relocation[j] = i;
                }
                i++;
            }
            j++;
        }
        heap.resize(i);
        return std::make_pair(n - i, std::move(relocation));
    }
    void _relocate(const std::unordered_map<Location, Location> &relocation) {
        auto reloc = [&relocation](Location &loc) -> void {
            if (relocation.contains(loc)) {
                loc = relocation.at(loc);
            }
        };
        // traverse the stack
        for (auto &layer : stack) {
            // only frames "own" the environments
            if (layer.frame) {
                for (auto &[_, loc] : (*(layer.env))) {
                    reloc(loc);
                }
            }
            // but each layer can still have locals
            for (auto &v : layer.local) {
                reloc(v);
            }
        }
        // traverse the resultLoc
        reloc(resultLoc);
        // traverse the closure values
        for (auto &v : heap) {
            if (std::holds_alternative<Closure>(v)) {
                auto &c = std::get<Closure>(v);
                for (auto &[_, loc] : c.env) {
                    reloc(loc);
                }
            }
        }
    }
    int _gc() {
        auto visited = _mark();
        const auto &[removed, relocation] = _sweepAndCompact(visited);
        _relocate(relocation);
        return removed;
    }
    std::vector<SourceLocation> _getFrameSLs() {
        std::vector<SourceLocation> frameSLs;
        for (const auto &l : stack) {
            if (l.frame) {
                if (l.expr == nullptr) {  // main frame
                    frameSLs.emplace_back(1, 1);
                } else {
                    frameSLs.push_back(l.expr->sl);
                }
            }
        }
        return frameSLs;
    }
    void _errorStack() {
        auto frameSLs = _getFrameSLs();
        std::cerr << "\n>>> stack trace printed below\n";
        for (auto sl : frameSLs) {
            std::cerr << "calling function body at " << sl.toString() << "\n";
        }
    }

    // states
    std::string source;
    ExprNode *expr;
    std::vector<Layer> stack;
    std::vector<Value> heap;
    int numLiterals = 0;
    Location resultLoc = -1;
};

// ------------------------------
// main
// ------------------------------

std::string readSource(const std::string &spath) {
    if (!std::filesystem::exists(spath)) {
        throw std::runtime_error(spath + " does not exist.");
    }
    static constexpr std::size_t BLOCK = 1024;
    std::ifstream in(spath);
    in.exceptions(std::ios_base::badbit);
    std::string source;
    char buf[BLOCK];
    while (in.read(buf, BLOCK)) {
        source.append(buf, in.gcount());
    }
    source.append(buf, in.gcount());
    return source;
}

int main(int argc, char **argv) {
    if (argc != 2) {
        std::cerr << "Usage: " << argv[0] << " <source-path>\n";
        std::exit(EXIT_FAILURE);
    }
    try {
        std::string source = readSource(argv[1]);
        State state(std::move(source));
        state.execute();
        std::cout << "<end-of-stdout>\n"
                  << valueToString(state.getResult(), state.getExpr()) << std::endl;
        std::cerr << "***** final state serialization *****\n"
                  << '|' << state.serialize() << '|' << std::endl;
    } catch (const std::runtime_error &e) {
        std::cerr << e.what() << std::endl;
        std::exit(EXIT_FAILURE);
    }
}

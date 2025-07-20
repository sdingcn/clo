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
#include <sstream>
#include <type_traits>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <variant>
#include <vector>

#define DBG(x) \
    std::cerr << "[+] LINE " << __LINE__ \
              << ": " << (x) << std::endl
// TODO: add DBG everywhere and use a macro to control all of them

namespace utils {

    // ------------------------------
    // global utils
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
        SourceLocation(int l = 0, int c = 0) : line(l), column(c) {}

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
            }
            else {
                column++;
            }
        }

        int line;
        int column;
    };

    void panic(
        const std::string& type,
        const std::string& msg,
        const SourceLocation& sl = SourceLocation()
    ) {
        if (sl.line <= 0 || sl.column <= 0) {
            throw std::runtime_error("[" + type + " error] " + msg);
        } else {
            throw std::runtime_error("[" + type + " error " + sl.toString() + "] " + msg);
        }
    }

}  // namespace utils

namespace syntax {

    // ------------------------------
    // lexer
    // ------------------------------

    std::string quote(std::string s) {
        // Note: quote will not change newlines back to
        // the form "\\n" and thus can produce multi-line strings
        std::string r;
        r += '\"';
        for (char c : s) {
            if (c == '\\') {
                r += "\\\\";
            }
            else if (c == '\"') {
                r += "\\\"";
            }
            else {
                r += c;
            }
        }
        r += '\"';
        return r;
    }

    std::string unquote(std::string s) {
        int n = s.size();
        if (!((n >= 2) && (s[0] == '\"') && (s[n - 1] == '\"'))) {
            utils::panic("unquote", "invalid quoted string");
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
                    }
                    else if (c1 == '"') {
                        r += '"';
                    }
                    else if (c1 == 't') {
                        r += '\t';
                    }
                    else if (c1 == 'n') {
                        r += '\n';
                    }
                    else {
                        utils::panic("unquote", "invalid escape sequence");
                    }
                }
                else {
                    utils::panic("unquote", "incomplete escape sequence");
                }
            }
            else {
                r += c;
            }
        }
        return r;
    }

    struct SourceStream {
        SourceStream(std::string s) : source(std::move(s)) {
            std::string charstr =
                "`1234567890-=~!@#$%^&*()_+"
                "qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM"
                "[]\\;',./{}|:\"<>? \t\n";
            std::unordered_set<char> charset(charstr.begin(), charstr.end());
            for (char c : source) {
                if (!charset.contains(c)) {
                    utils::panic("lexer", "unsupported character", sl);
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
        utils::SourceLocation getNextSourceLocation() const {
            return sl;
        }

        std::string source;
        utils::SourceLocation sl;
    };

    struct Token {
        Token(utils::SourceLocation s, std::string t) : sl(s), text(std::move(t)) {}

        utils::SourceLocation sl;
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
                    utils::panic("lexer", "incomplete integer literal", startsl);
                }
                // string literal
            }
            else if (ss.peekNext() == '"') {
                text += ss.popNext();
                bool complete = false;
                bool escape = false;
                while (ss.hasNext()) {
                    if ((!escape) && ss.peekNext() == '"') {
                        text += ss.popNext();
                        complete = true;
                        break;
                    }
                    else {
                        char c = ss.popNext();
                        if (c == '\\') {
                            escape = true;
                        }
                        else {
                            escape = false;
                        }
                        text += c;
                    }
                }
                if (!complete) {
                    utils::panic("lexer", "incomplete string literal", startsl);
                }
                // variable / keyword
            }
            else if (std::isalpha(ss.peekNext()) || ss.peekNext() == '_') {
                while (ss.hasNext() && (
                    std::isalpha(ss.peekNext()) || std::isdigit(ss.peekNext()) ||
                    ss.peekNext() == '_')) {
                    text += ss.popNext();
                }
                // intrinsic
            }
            else if (ss.peekNext() == '.') {
                while (ss.hasNext() && !(std::isspace(ss.peekNext()) || ss.peekNext() == ')')) {
                    text += ss.popNext();
                }
                // special symbol
            }
            else if (std::string("(){}@").find(ss.peekNext()) != std::string::npos) {
                text += ss.popNext();
                // comment
            }
            else if (ss.peekNext() == '#') {
                while (ss.hasNext() && ss.peekNext() != '\n') {
                    ss.popNext();
                }
                // nextToken() will consume the \n and recursively continue
                return nextToken();
            }
            else {
                utils::panic("lexer", "unsupported starting character", startsl);
            }
            return Token(startsl, std::move(text));
        };

        std::deque<Token> tokens;
        while (true) {
            auto ret = nextToken();
            if (ret.has_value()) {
                tokens.push_back(ret.value());
            }
            else {
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

#define BASIC_INFO_PARMDECL \
    utils::SourceLocation s = utils::SourceLocation(), \
    std::unordered_set<std::string> f = std::unordered_set<std::string>(), \
    bool t = false

    struct ExprNode {
        DELETE_COPY(ExprNode);
        virtual ~ExprNode() {}
        ExprNode(BASIC_INFO_PARMDECL)
            : sl(s), freeVars(f), tail(t) {}

        virtual ExprNode* clone() const = 0;
        virtual void traverse(
            TraversalMode mode,
            // callback may have states
            std::function<void(ExprNode*)>& callback) = 0;
        virtual std::string toString() const = 0;
        virtual void computeFreeVars() = 0;
        virtual void computeTail(bool parentTail) = 0;

        utils::SourceLocation sl;
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
        IntegerNode(std::string v, BASIC_INFO_PARMDECL)
            : ExprNode(s, f, t), val(std::move(v)) {}

        // covariant return type for override
        virtual IntegerNode* clone() const override {
            auto inode = new IntegerNode(val, sl, freeVars, tail);
            inode->loc = loc;
            return inode;
        }
        virtual void traverse(
            TraversalMode,
            std::function<void(ExprNode*)>& callback) override {
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
        StringNode(std::string v, BASIC_INFO_PARMDECL)
            : ExprNode(s, f, t), val(std::move(v)) {}

        // covariant return type
        virtual StringNode* clone() const override {
            auto snode = new StringNode(val, sl, freeVars, tail);
            snode->loc = loc;
            return snode;
        }
        virtual void traverse(
            TraversalMode,
            std::function<void(ExprNode*)>& callback) override {
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
        VariableNode(std::string n, BASIC_INFO_PARMDECL)
            : ExprNode(s, f, t), name(std::move(n)) {}

        virtual VariableNode* clone() const override {
            auto vnode = new VariableNode(name, sl, freeVars, tail);
            return vnode;
        }
        virtual void traverse(
            TraversalMode,
            std::function<void(ExprNode*)>& callback) override {
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
        LambdaNode(std::vector<VariableNode*> v, ExprNode* e, BASIC_INFO_PARMDECL) :
            ExprNode(s, f, t), varList(std::move(v)), expr(e) {}

        virtual LambdaNode* clone() const override {
            std::vector<VariableNode*> newVarList;
            for (auto v : varList) {
                newVarList.push_back(v->clone());
            }
            ExprNode* newExpr = expr->clone();
            auto lnode = new LambdaNode(std::move(newVarList), newExpr, sl, freeVars, tail);
            return lnode;
        }
        virtual void traverse(
            TraversalMode mode,
            std::function<void(ExprNode*)>& callback) override {
            if (mode == TraversalMode::TOP_DOWN) {
                callback(this);
                _traverseSubtree(mode, callback);
            }
            else {
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
        ExprNode* expr;
    private:
        void _traverseSubtree(TraversalMode mode, std::function<void(ExprNode*)>& callback) {
            for (auto var : varList) {
                var->traverse(mode, callback);
            }
            expr->traverse(mode, callback);
        }
    };

    struct LetrecNode : public ExprNode {
        DELETE_COPY(LetrecNode);
        virtual ~LetrecNode() {
            for (auto& ve : varExprList) {
                delete ve.first;
                delete ve.second;
            }
            delete expr;
        }
        LetrecNode(
            std::vector<std::pair<VariableNode*, ExprNode*>> v, ExprNode* e,
            BASIC_INFO_PARMDECL) :
            ExprNode(s, f, t), varExprList(std::move(v)), expr(e) {}

        virtual LetrecNode* clone() const override {
            std::vector<std::pair<VariableNode*, ExprNode*>> newVarExprList;
            for (const auto& ve : varExprList) {
                // the evaluation order of the two clones are irrelevant
                newVarExprList.push_back(std::make_pair(ve.first->clone(), ve.second->clone()));
            }
            ExprNode* newExpr = expr->clone();
            auto lnode = new LetrecNode(std::move(newVarExprList), newExpr, sl, freeVars, tail);
            return lnode;
        }
        virtual void traverse(
            TraversalMode mode,
            std::function<void(ExprNode*)>& callback) override {
            if (mode == TraversalMode::TOP_DOWN) {
                callback(this);
                _traverseSubtree(mode, callback);
            }
            else {
                _traverseSubtree(mode, callback);
                callback(this);
            }
        }
        virtual std::string toString() const override {
            std::string ret = "letrec (";
            for (const auto& ve : varExprList) {
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
            for (auto& ve : varExprList) {
                ve.second->computeFreeVars();
                freeVars.insert(ve.second->freeVars.begin(), ve.second->freeVars.end());
            }
            for (auto& ve : varExprList) {
                freeVars.erase(ve.first->name);
            }
        }
        virtual void computeTail(bool parentTail) override {
            tail = parentTail;
            for (auto& ve : varExprList) {
                ve.first->computeTail(false);
                ve.second->computeTail(false);
            }
            expr->computeTail(tail);
        }

        std::vector<std::pair<VariableNode*, ExprNode*>> varExprList;
        ExprNode* expr;
    private:
        void _traverseSubtree(TraversalMode mode, std::function<void(ExprNode*)>& callback) {
            for (auto& ve : varExprList) {
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
        IfNode(ExprNode* c, ExprNode* b1, ExprNode* b2, BASIC_INFO_PARMDECL) :
            ExprNode(s, f, t), cond(c), branch1(b1), branch2(b2) {}

        virtual IfNode* clone() const override {
            // the evaluation order of the three clones are irrelevant
            auto inode = new IfNode(cond->clone(), branch1->clone(), branch2->clone(),
                sl, freeVars, tail);
            return inode;
        }
        virtual void traverse(
            TraversalMode mode,
            std::function<void(ExprNode*)>& callback) override {
            if (mode == TraversalMode::TOP_DOWN) {
                callback(this);
                _traverseSubtree(mode, callback);
            }
            else {
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

        ExprNode* cond;
        ExprNode* branch1;
        ExprNode* branch2;
    private:
        void _traverseSubtree(TraversalMode mode, std::function<void(ExprNode*)>& callback) {
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
        SequenceNode(std::vector<ExprNode*> e, BASIC_INFO_PARMDECL) :
            ExprNode(s, f, t), exprList(std::move(e)) {}

        virtual SequenceNode* clone() const override {
            std::vector<ExprNode*> newExprList;
            for (auto e : exprList) {
                newExprList.push_back(e->clone());
            }
            auto snode = new SequenceNode(std::move(newExprList), sl, freeVars, tail);
            return snode;
        }
        virtual void traverse(
            TraversalMode mode,
            std::function<void(ExprNode*)>& callback) override {
            if (mode == TraversalMode::TOP_DOWN) {
                callback(this);
                _traverseSubtree(mode, callback);
            }
            else {
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
        void _traverseSubtree(TraversalMode mode, std::function<void(ExprNode*)>& callback) {
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
        IntrinsicCallNode(std::string i, std::vector<ExprNode*> a, BASIC_INFO_PARMDECL):
            ExprNode(s, f, t), intrinsic(std::move(i)), argList(std::move(a)) {}

        virtual IntrinsicCallNode* clone() const override {
            std::vector<ExprNode*> newArgList;
            for (auto a : argList) {
                newArgList.push_back(a->clone());
            }
            auto inode = new IntrinsicCallNode(
                intrinsic, std::move(newArgList), sl, freeVars, tail);
            return inode;
        }
        virtual void traverse(
            TraversalMode mode,
            std::function<void(ExprNode*)>& callback) override {
            if (mode == TraversalMode::TOP_DOWN) {
                callback(this);
                _traverseSubtree(mode, callback);
            }
            else {
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
        void _traverseSubtree(TraversalMode mode, std::function<void(ExprNode*)>& callback) {
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
        ExprCallNode(ExprNode* e, std::vector<ExprNode*> a, BASIC_INFO_PARMDECL) :
            ExprNode(s, f, t), expr(e), argList(std::move(a)) {}

        virtual ExprCallNode* clone() const override {
            ExprNode* newExpr = expr->clone();
            std::vector<ExprNode*> newArgList;
            for (auto a : argList) {
                newArgList.push_back(a->clone());
            }
            auto enode = new ExprCallNode(newExpr, std::move(newArgList), sl, freeVars, tail);
            return enode;
        }
        virtual void traverse(
            TraversalMode mode,
            std::function<void(ExprNode*)>& callback) override {
            if (mode == TraversalMode::TOP_DOWN) {
                callback(this);
                _traverseSubtree(mode, callback);
            }
            else {
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

        ExprNode* expr;
        std::vector<ExprNode*> argList;
    private:
        void _traverseSubtree(TraversalMode mode, std::function<void(ExprNode*)>& callback) {
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
        AtNode(VariableNode* v, ExprNode* e, BASIC_INFO_PARMDECL) :
            ExprNode(s, f, t), var(v), expr(e) {}

        virtual AtNode* clone() const override {
            // the evaluation order of the two clones are irrelevant
            auto anode = new AtNode(var->clone(), expr->clone(), sl, freeVars, tail);
            return anode;
        }
        virtual void traverse(
            TraversalMode mode,
            std::function<void(ExprNode*)>& callback) override {
            if (mode == TraversalMode::TOP_DOWN) {
                callback(this);
                _traverseSubtree(mode, callback);
            }
            else {
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

        VariableNode* var;
        ExprNode* expr;
    private:
        void _traverseSubtree(TraversalMode mode, std::function<void(ExprNode*)>& callback) {
            var->traverse(mode, callback);
            expr->traverse(mode, callback);
        }
    };

#undef BASIC_INFO_PARMDECL
#undef DELETE_COPY

    ExprNode* parse(std::deque<Token> tokens) {
        auto isIntegerToken = [](const Token& token) {
            return token.text.size() > 0 && (
                std::isdigit(token.text[0]) || token.text[0] == '-' || token.text[0] == '+');
        };
        auto isStringToken = [](const Token& token) {
            return token.text.size() > 0 && token.text[0] == '"';
        };
        auto isIntrinsicToken = [](const Token& token) {
            return token.text.size() > 0 && token.text[0] == '.';
        };
        auto isVariableToken = [](const Token& token) {
            return token.text.size() > 0 && (std::isalpha(token.text[0]) || token.text[0] == '_');
        };
        auto isTheToken = [](const std::string& s) {
            return [s](const Token& token) {
                return token.text == s;
            };
        };
        auto consume = [&tokens]<typename Callable>(const Callable & predicate) -> Token {
            if (tokens.size() == 0) {
                utils::panic("parser", "incomplete token stream");
            }
            auto token = tokens.front();
            tokens.pop_front();
            if (!predicate(token)) {
                utils::panic("parser", "unexpected token", token.sl);
            }
            return token;
        };

        std::function<IntegerNode* ()> parseInteger;
        std::function<StringNode* ()> parseString;
        std::function<VariableNode* ()> parseVariable;
        std::function<LambdaNode* ()> parseLambda;
        std::function<LetrecNode* ()> parseLetrec;
        std::function<IfNode* ()> parseIf;
        std::function<SequenceNode* ()> parseSequence;
        std::function<IntrinsicCallNode* ()> parseIntrinsicCall;
        std::function<ExprCallNode* ()> parseExprCall;
        std::function<AtNode* ()> parseAt;
        std::function<ExprNode* ()> parseExpr;

        parseInteger = [&]() -> IntegerNode* {
            auto token = consume(isIntegerToken);
            return new IntegerNode(token.text, token.sl);
        };
        parseString = [&]() -> StringNode* {  // don't unquote here: AST keeps raw tokens
            auto token = consume(isStringToken);
            return new StringNode(token.text, token.sl);
        };
        parseVariable = [&]() -> VariableNode* {
            auto token = consume(isVariableToken);
            return new VariableNode(std::move(token.text), token.sl);
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
            return new LambdaNode(std::move(varList), expr, start.sl);
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
            return new LetrecNode(std::move(varExprList), expr, start.sl);
        };
        parseIf = [&]() -> IfNode* {
            auto start = consume(isTheToken("if"));
            auto cond = parseExpr();
            auto branch1 = parseExpr();
            auto branch2 = parseExpr();
            return new IfNode(cond, branch1, branch2, start.sl);
        };
        parseSequence = [&]() -> SequenceNode* {
            auto start = consume(isTheToken("{"));
            std::vector<ExprNode*> exprList;
            while (tokens.size() && tokens[0].text != "}") {
                exprList.push_back(parseExpr());
            }
            if (!exprList.size()) {
                utils::panic("parser", "zero-length sequence", start.sl);
            }
            consume(isTheToken("}"));
            return new SequenceNode(std::move(exprList), start.sl);
        };
        parseIntrinsicCall = [&]() -> IntrinsicCallNode* {
            auto start = consume(isTheToken("("));
            auto intrinsic = consume(isIntrinsicToken);
            std::vector<ExprNode*> argList;
            while (tokens.size() && tokens[0].text != ")") {
                argList.push_back(parseExpr());
            }
            consume(isTheToken(")"));
            return new IntrinsicCallNode(std::move(intrinsic.text), std::move(argList), start.sl);
        };
        parseExprCall = [&]() -> ExprCallNode* {
            auto start = consume(isTheToken("("));
            auto expr = parseExpr();
            std::vector<ExprNode*> argList;
            while (tokens.size() && tokens[0].text != ")") {
                argList.push_back(parseExpr());
            }
            consume(isTheToken(")"));
            return new ExprCallNode(expr, std::move(argList), start.sl);
        };
        parseAt = [&]() -> AtNode* {
            auto start = consume(isTheToken("@"));
            auto var = parseVariable();
            auto expr = parseExpr();
            return new AtNode(var, expr, start.sl);
        };
        parseExpr = [&]() -> ExprNode* {
            if (!tokens.size()) {
                utils::panic("parser", "incomplete token stream");
                // unreachable
                return nullptr;
            }
            else if (isIntegerToken(tokens[0])) {
                return parseInteger();
            }
            else if (isStringToken(tokens[0])) {
                return parseString();
            }
            else if (tokens[0].text == "lambda") {
                return parseLambda();
            }
            else if (tokens[0].text == "letrec") {
                return parseLetrec();
            }
            else if (tokens[0].text == "if") {
                return parseIf();
                // check keywords before var to avoid recognizing keywords as vars
            }
            else if (isVariableToken(tokens[0])) {
                return parseVariable();
            }
            else if (tokens[0].text == "{") {
                return parseSequence();
            }
            else if (tokens[0].text == "(") {
                if (tokens.size() < 2) {
                    utils::panic("parser", "incomplete token stream");
                    // unreachable
                    return nullptr;
                }
                if (isIntrinsicToken(tokens[1])) {
                    return parseIntrinsicCall();
                }
                else {
                    return parseExprCall();
                }
            }
            else if (tokens[0].text == "@") {
                return parseAt();
            }
            else {
                utils::panic("parser", "unrecognized token", tokens[0].sl);
                // unreachable
                return nullptr;
            }
        };

        auto expr = parseExpr();
        return expr;
    }

}  // namespace syntax

namespace runtime {

    // ------------------------------
    // runtime values (those things can be copied (including pointer members))
    // ------------------------------

    struct Void {
        Void() = default;
    };

    struct Integer {
        Integer(int v) : value(v) {}

        int value = 0;
    };

    struct String {  // for string literals, this class contains the unquoted ones
        String(std::string v) : value(std::move(v)) {}

        std::string value;
    };

    // variable environment; newer variables have larger indices
    using Env = std::vector<std::pair<std::string, syntax::Location>>;

    std::optional<syntax::Location> lookup(const std::string& name, const Env& env) {
        for (auto p = env.rbegin(); p != env.rend(); p++) {
            if (p->first == name) {
                return p->second;
            }
        }
        return std::nullopt;
    }

    struct Closure {
        // a closure should copy its environment
        Closure(Env e, const syntax::LambdaNode* f) : env(std::move(e)), fun(f) {}

        Env env;
        const syntax::LambdaNode* fun;
    };

    using Value = std::variant<Void, Integer, String, Closure>;

    // stack layer

    struct Layer {
        // a default argument is evaluated each time the function is called without
        // that argument (not important here)
        Layer(
            Env e = Env(),
            const syntax::ExprNode* x = nullptr,
            bool f = false,
            int p = 0,
            std::vector<syntax::Location> l = std::vector<syntax::Location>()) :
            frame(f), env(std::move(e)), expr(x), pc(p), local(std::move(l)) {}

        // whether this is a frame
        bool frame;
        // one env per frame (closure call layer)
        Env env;
        const syntax::ExprNode* expr;
        // program counter inside this expr
        int pc;
        // temporary local information for evaluation
        std::vector<syntax::Location> local;
    };

}  // namespace runtime

namespace serialization {

    // ------------------------------
    // serialization and de-serialization utilities
    // ------------------------------

    // Sentence is the type of serialized program states (excluding the source code)
    using Sentence = std::deque<std::string>;
    // every group inside a sentence is like
    // "(" "<label>" ... ")"
    // "[" "<label>" ... "]"
    // "{" "<label>" ... "}"
    // "<" "<label>" ... ">"
    // and groups can be nested
    // so the group header is "(" "<label>" and has length 2 (two words)
    constexpr int groupHeaderLength = 2;

    Sentence operator,(const Sentence& s1, const Sentence& s2) {
        Sentence s;
        for (auto& w : s1) {
            s.push_back(w);
        }
        for (auto& w : s2) {
            s.push_back(w);
        }
        return s;
    }

    Sentence slice(const Sentence& s0, int i, int j) {
        int s0Size = s0.size();
        if (!(0 <= i && i <= j && j <= s0Size)) {
            utils::panic("serialization", "invalid slicing");
        }
        Sentence s;
        for (int k = i; k < j; k++) {
            s.push_back(s0[k]);
        }
        return s;
    }

    std::string join(const Sentence& s) {
        std::string str;
        for (auto& w : s) {
            if (str != "") {
                str += " ";
            }
            str += w;
        }
        return str;
    }

    Sentence takeTo(Sentence& s0, const std::string& w) {
        Sentence s;
        while (s0.size() > 0 && s0.front() != w) {
            s.push_back(std::move(s0.front()));
            s0.pop_front();
        }
        if (s0.size() > 0) {
            s.push_back(std::move(s0.front()));
            s0.pop_front();
            return s;
        } else {
            utils::panic("serialization", "takeTo failed");
            // unreachable
            return Sentence();
        }
    }

    // representation of AST node location
    using Path = std::vector<int>;

    Path encodeNodePath(const syntax::ExprNode* node, const syntax::ExprNode* root) {
        if (node == nullptr) {
            utils::panic("serialization", "node is nullptr");
        }
        if (root == nullptr) {
            utils::panic("serialization", "root is nullptr");
        }
        Path nodePath;
        std::function<bool(Path&, const syntax::ExprNode*)>
            findPath = [&](Path& curPath, const syntax::ExprNode* curNode) -> bool {
            if (curNode == node) {
                // copy
                nodePath = curPath;
                return true;
            }
            else {
                if (dynamic_cast<const syntax::IntegerNode*>(curNode) ||
                    dynamic_cast<const syntax::StringNode*>(curNode) ||
                    dynamic_cast<const syntax::VariableNode*>(curNode)) {
                    return false;
                }
                else if (auto lnode = dynamic_cast<const syntax::LambdaNode*>(curNode)) {
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
                }
                else if (auto lnode = dynamic_cast<const syntax::LetrecNode*>(curNode)) {
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
                }
                else if (auto inode = dynamic_cast<const syntax::IfNode*>(curNode)) {
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
                }
                else if (auto snode = dynamic_cast<const syntax::SequenceNode*>(curNode)) {
                    int numExprs = snode->exprList.size();
                    for (int i = 0; i < numExprs; i++) {
                        curPath.push_back(i);
                        if (findPath(curPath, snode->exprList[i])) {
                            return true;
                        }
                        curPath.pop_back();
                    }
                    return false;
                }
                else if (auto inode = dynamic_cast<const syntax::IntrinsicCallNode*>(curNode)) {
                    int numArgs = inode->argList.size();
                    for (int i = 0; i < numArgs; i++) {
                        curPath.push_back(i);
                        if (findPath(curPath, inode->argList[i])) {
                            return true;
                        }
                        curPath.pop_back();
                    }
                    return false;
                }
                else if (auto enode = dynamic_cast<const syntax::ExprCallNode*>(curNode)) {
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
                }
                else if (auto anode = dynamic_cast<const syntax::AtNode*>(curNode)) {
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
                }
                else {
                    utils::panic("serialization", "unrecognized AST node");
                    // unreachable
                    return false;
                }
            }
        };
        Path currentPath;
        if (findPath(currentPath, root)) {
            return nodePath;
        }
        else {
            utils::panic("serialization", "node not found", node->sl);
            // unreachable
            return Path();
        }
    }

    const syntax::ExprNode* decodeNodePath(const Path& path, const syntax::ExprNode* root) {
        if (root == nullptr) {
            utils::panic("serialization", "root is nullptr");
        }
        const syntax::ExprNode* curNode = root;
        bool failed = false;
        for (int i : path) {
            if (dynamic_cast<const syntax::IntegerNode*>(curNode)||
                dynamic_cast<const syntax::StringNode*>(curNode) ||
                dynamic_cast<const syntax::VariableNode*>(curNode)) {
                failed = true;
                break;
            }
            else if (auto lnode = dynamic_cast<const syntax::LambdaNode*>(curNode)) {
                int numArgs = lnode->varList.size();
                if (0 <= i && i < numArgs) {
                    curNode = lnode->varList[i];
                }
                else if (i == numArgs) {
                    curNode = lnode->expr;
                }
                else {
                    failed = true;
                    break;
                }
            }
            else if (auto lnode = dynamic_cast<const syntax::LetrecNode*>(curNode)) {
                int numVars = lnode->varExprList.size();
                if (0 <= i && i < numVars * 2) {
                    int index = i / 2;
                    int offset = i % 2;
                    if (offset == 0) {
                        curNode = lnode->varExprList[index].first;
                    }
                    else {
                        curNode = lnode->varExprList[index].second;
                    }
                }
                else if (i == numVars * 2) {
                    curNode = lnode->expr;
                }
                else {
                    failed = true;
                    break;
                }
            }
            else if (auto inode = dynamic_cast<const syntax::IfNode*>(curNode)) {
                if (i == 0) {
                    curNode = inode->cond;
                }
                else if (i == 1) {
                    curNode = inode->branch1;
                }
                else if (i == 2) {
                    curNode = inode->branch2;
                }
                else {
                    failed = true;
                    break;
                }
            }
            else if (auto snode = dynamic_cast<const syntax::SequenceNode*>(curNode)) {
                int numExprs = snode->exprList.size();
                if (0 <= i && i < numExprs) {
                    curNode = snode->exprList[i];
                }
                else {
                    failed = true;
                    break;
                }
            }
            else if (auto inode = dynamic_cast<const syntax::IntrinsicCallNode*>(curNode)) {
                int numArgs = inode->argList.size();
                if (0 <= i && i < numArgs) {
                    curNode = inode->argList[i];
                }
                else {
                    failed = true;
                    break;
                }
            }
            else if (auto enode = dynamic_cast<const syntax::ExprCallNode*>(curNode)) {
                if (i == 0) {
                    curNode = enode->expr;
                }
                else {
                    int numArgs = enode->argList.size();
                    if (1 <= i && i <= numArgs) {
                        curNode = enode->argList[i - 1];
                    }
                    else {
                        failed = true;
                        break;
                    }
                }
            }
            else if (auto anode = dynamic_cast<const syntax::AtNode*>(curNode)) {
                if (i == 0) {
                    curNode = anode->var;
                }
                else if (i == 1) {
                    curNode = anode->expr;
                }
                else {
                    failed = true;
                    break;
                }
            }
            else {
                utils::panic("serialization", "unrecognized AST node");
            }
        }
        if (failed) {
            utils::panic("serialization", "path not found");
        }
        return curNode;
    }

    const syntax::ExprNode* migrateASTNode(
        const syntax::ExprNode* root1,
        const syntax::ExprNode* root2,
        const syntax::ExprNode* node1
    ) {
        auto path = encodeNodePath(node1, root1);
        return decodeNodePath(path, root2);
    }

    Sentence pathToSentence(const Path& path) {
        Sentence s;
        s.push_back("[");
        s.push_back("pth");
        for (int i : path) {
            s.push_back(std::to_string(i));
        }
        s.push_back("]");
        return s;
    }

    Path sentenceToPath(const Sentence& s) {
        if (s.size() < 3) {  // shortest sentence is [ pth ]
            utils::panic("serialization", "sentence is invalid to be converted to path");
        }
        Path path;
        for (auto p = s.begin() + groupHeaderLength; p + 1 != s.end(); p++) {
            path.push_back(std::stoi(*p));
        }
        return path;
    }

    Sentence envToSentence(const runtime::Env& env) {
        Sentence s;
        s.push_back("[");
        s.push_back("env");
        for (auto& p : env) {
            s.push_back(p.first);
            s.push_back(std::to_string(p.second));
        }
        s.push_back("]");
        return s;
    }

    runtime::Env sentenceToEnv(const Sentence& s) {
        if (s.size() < 3) {  // shortest sentence is [ env ]
            utils::panic("serialization", "sentence is invalid to be converted to env");
        }
        runtime::Env env;
        for (auto p = s.begin() + groupHeaderLength; p + 1 != s.end(); p += 2) {
            env.push_back(std::make_pair(*p, std::stoi(*(p + 1))));
        }
        return env;
    }

    Sentence valueToSentence(const runtime::Value& v, const syntax::ExprNode* root) {
        if (std::holds_alternative<runtime::Void>(v)) {
            return {"(", "vval", ")"};
        }
        else if (std::holds_alternative<runtime::Integer>(v)) {
            auto i = std::get<runtime::Integer>(v);
            return {"(", "ival", std::to_string(i.value), ")"};
        }
        else if (std::holds_alternative<runtime::String>(v)) {
            auto s = std::get<runtime::String>(v);
            std::string quoted = syntax::quote(s.value);
            int len = quoted.size();
            return {"(", "sval", std::to_string(len), quoted, ")"};
        }
        else {
            auto c = std::get<runtime::Closure>(v);
            Sentence head = {"(", "cval"};
            Sentence tail = {")"};
            return (head, envToSentence(c.env), pathToSentence(encodeNodePath(c.fun, root)), tail);
        }
    }

    runtime::Value sentenceToValue(const Sentence& s, const syntax::ExprNode* root) {
        if (s.size() < 3) {  // shortest sentence is ( vval )
            utils::panic("serialization", "sentence is invalid to be converted to value");
        }
        if (s[1] == "vval") {
            return runtime::Void();
        }
        else if (s[1] == "ival") {
            return runtime::Integer(std::stoi(s[2]));
        }
        else if (s[1] == "sval") {
            return runtime::String(syntax::unquote(s[3]));
        }
        else if (s[1] == "cval") {
            int pathIndex = -1;
            int sentenceSize = s.size();
            // finding the start of path
            // should start from the back because "env" also starts with "["
            for (int i = sentenceSize - 1; i >= 0; i--) {
                if (s[i] == "[") {
                    pathIndex = i;
                    break;
                }
            }
            if (pathIndex == -1) {
                utils::panic("serialization", "didn't find the start of path");
            }
            auto env = sentenceToEnv(slice(s, groupHeaderLength, pathIndex));
            auto path = sentenceToPath(slice(s, pathIndex, s.size() - 1));
            return runtime::Closure(
                env, dynamic_cast<const syntax::LambdaNode*>(decodeNodePath(path, root)));
        } else {
            utils::panic("serialization", "invalid label in the sentence as a value");
            // unreachable
            return runtime::Value();
        }
    }

    Sentence layerToSentence(const runtime::Layer& layer, const syntax::ExprNode* root) {
        Sentence serializedLayer = {"<", "lay"};
        // bool frame;
        if (layer.frame) {
            serializedLayer.push_back("!fr");
        }
        else {
            serializedLayer.push_back("!nfr");
        }
        // std::shared_ptr<Env> env;
        if (layer.frame) {
            serializedLayer = (serializedLayer, envToSentence(layer.env));
        }
        // const syntax::ExprNode *expr;
        if (layer.expr != nullptr) {
            serializedLayer = (serializedLayer, pathToSentence(encodeNodePath(layer.expr, root)));
        }
        else {
            // the case of main frame
            Sentence dummyPathSentence = {"[", "pth", "-1", "]"};
            serializedLayer = (serializedLayer, dummyPathSentence);
        }
        // int pc;
        Sentence pcSentence = {"(", "pc", std::to_string(layer.pc), ")"};
        serializedLayer = (serializedLayer, pcSentence);
        // std::vector<Location> local;
        Sentence head = {"[", "lok"};
        Sentence body;
        for (auto& l : layer.local) {
            body.push_back(std::to_string(l));
        }
        Sentence tail = {"]"};
        Sentence end = {">"};
        serializedLayer = (serializedLayer, head, body, tail, end);
        return serializedLayer;
    }

    runtime::Layer sentenceToLayer(
        const Sentence& s, const syntax::ExprNode* root) {
        if (s.size() < 4) {  // shortest sentence is < lay *fr >
            utils::panic("serialization", "sentence is invalid to be converted to layer");
        }
        // bool frame;
        if (!(s[groupHeaderLength] == "!fr" || s[groupHeaderLength] == "!nfr")) {
            utils::panic("serialization", "didn't find a valid frame label");
        }
        bool frame = (s[groupHeaderLength] == "!fr");
        // Env env;
        runtime::Env env;
        int nextPos = groupHeaderLength + 1;
        if (frame) {
            int sentenceSize = s.size();
            int start = nextPos;
            for (int i = start; i < sentenceSize; i++) {
                if (s[i] == "]") {
                    nextPos = i + 1;
                    break;
                }
            }
            if (nextPos == groupHeaderLength + 1) {
                utils::panic("serialization", "didn't find valid env in frame layer");
            }
            env = sentenceToEnv(slice(s, start, nextPos));
        }
        // const syntax::ExprNode* expr;
        const syntax::ExprNode* expr = nullptr;
        // s[nextPos + groupHeaderLength] should at least be "]"
        if (nextPos + groupHeaderLength >= static_cast<int>(s.size())) {
            utils::panic("serialization", "didn't find valid path in layer");
        }
        if (s[nextPos + groupHeaderLength] != "-1") {
            // this includes the case of empty path ("]")
            int start = nextPos;
            int serSize = s.size();
            for (int i = nextPos; i < serSize; i++) {
                if (s[i] == "]") {
                    nextPos = i + 1;
                    break;
                }
            }
            if (nextPos == start) {
                utils::panic("serialization", "didn't find valid path end (\"]\") in layer");
            }
            expr = decodeNodePath(sentenceToPath(slice(s, start, nextPos)), root);
        } else {
            // dummy path (for the main frame)
            nextPos += (groupHeaderLength + 2);
        }
        // int pc;
        if (nextPos + groupHeaderLength >= static_cast<int>(s.size())) {
            utils::panic("serialization", "didn't find pc");
        }
        int pc = std::stoi(s[nextPos + groupHeaderLength]);
        nextPos += (groupHeaderLength + 2);
        // std::vector<syntax::Location> local;
        // s[nextPos + groupHeaderLength] should at least be "]"
        if (nextPos + groupHeaderLength >= static_cast<int>(s.size())) {
            utils::panic("serialization", "didn't find valid lok");
        }
        std::vector<syntax::Location> local;
        int sentenceSize = s.size();
        for (int i = nextPos + groupHeaderLength; i < sentenceSize; i++) {
            if (s[i] == "]") {
                break;
            }
            local.push_back(std::stoi(s[i]));
        }
        return runtime::Layer(std::move(env), expr, frame, pc, std::move(local));
    }

}  // namespace serialization

class State {
    void _populateStaticAnalysisResults(bool doPreAlloc = true) {
        std::function<void(syntax::ExprNode*)> checkDuplicate =
            [](syntax::ExprNode* e) -> void {
            if (auto lnode = dynamic_cast<syntax::LambdaNode*>(e)) {
                std::unordered_set<std::string> varNames;
                for (auto var : lnode->varList) {
                    if (varNames.contains(var->name)) {
                        utils::panic("sema", "duplicate parameter names", lnode->sl);
                    }
                    varNames.insert(var->name);
                }
            }
            else if (auto lnode = dynamic_cast<syntax::LetrecNode*>(e)) {
                std::unordered_set<std::string> varNames;
                for (const auto& ve : lnode->varExprList) {
                    if (varNames.contains(ve.first->name)) {
                        utils::panic("sema", "duplicate binding names", lnode->sl);
                    }
                    varNames.insert(ve.first->name);
                }
            }
        };
        expr->traverse(syntax::TraversalMode::TOP_DOWN, checkDuplicate);
        expr->computeFreeVars();
        expr->computeTail(false);
        if (doPreAlloc) {
            std::unordered_map<int, syntax::Location> integerLocationMap;
            std::unordered_map<std::string, syntax::Location> stringLocationMap;
            std::function<void(syntax::ExprNode*)> preAllocate =
                [this, &integerLocationMap, &stringLocationMap]
                (syntax::ExprNode* e) -> void {
                if (auto inode = dynamic_cast<syntax::IntegerNode*>(e)) {
                    // TODO: exceptions
                    int ival = std::stoi(inode->val);
                    if (integerLocationMap.contains(ival)) {
                        inode->loc = integerLocationMap.at(ival);
                    } else {
                        inode->loc = this->_new<runtime::Integer>(ival);
                        integerLocationMap[ival] = inode->loc;
                    }
                }
                else if (auto snode = dynamic_cast<syntax::StringNode*>(e)) {
                    std::string sval = syntax::unquote(snode->val);
                    if (stringLocationMap.contains(sval)) {
                        snode->loc = stringLocationMap.at(sval);
                    } else {
                        snode->loc = this->_new<runtime::String>(sval);
                        stringLocationMap[sval] = snode->loc;
                    }
                }
            };
            expr->traverse(syntax::TraversalMode::TOP_DOWN, preAllocate);
        }
        else {
            // the location for every different value is unique
            std::function<void(syntax::ExprNode*)> findPreAllocate =
                [this](syntax::ExprNode* e) -> void {
                if (auto inode = dynamic_cast<syntax::IntegerNode*>(e)) {
                    // TODO: exceptions
                    for (int i = 0; i < numLiterals; i++) {
                        if (std::holds_alternative<runtime::Integer>(heap[i]) &&
                            std::get<runtime::Integer>(heap[i]).value == std::stoi(inode->val)) {
                            inode->loc = i;
                            break;
                        }
                    }
                }
                else if (auto snode = dynamic_cast<syntax::StringNode*>(e)) {
                    for (int i = 0; i < numLiterals; i++) {
                        if (std::holds_alternative<runtime::String>(heap[i]) &&
                            std::get<runtime::String>(heap[i]).value ==
                            syntax::unquote(snode->val)) {
                            snode->loc = i;
                            break;
                        }
                    }
                }
            };
            expr->traverse(syntax::TraversalMode::TOP_DOWN, findPreAllocate);
        }
    }
public:
    State(std::string origin) {
        if (origin.size() == 0) {
            utils::panic("state constructor", "empty origin string");
        }
        else if (origin[0] != '|') {  // origin is source code
            // (1) copy source code
            source = std::move(origin);
            // (2) AST construction (parsing and static analysis) (TODO: exceptions?)
            expr = syntax::parse(syntax::lex(source));
            _populateStaticAnalysisResults();
            // (3) stack initialization
            // the main frame (which cannot be removed by TCO because it's marked as not tail)
            stack.push_back(runtime::Layer(runtime::Env(), nullptr, true));
            // the first expression
            stack.push_back(runtime::Layer(runtime::Env(), expr));
            // (4) heap initialization
            // skipped (_populateStaticAnalysisResults may have created some heap entries)
            // (5) literal boundary initialization
            numLiterals = heap.size();
            // (6) result location initialization (no result yet)
            resultLoc = -1;
        }
        else {  // origin is serialized state
            int originLen = origin.size();
            std::string sourceLenStr;
            for (int i = std::string("|{ src ").size(); i < originLen; i++) {
                if (std::isdigit(origin[i])) {
                    sourceLenStr += origin[i];
                }
                else {
                    break;
                }
            }
            // (1) copy source code
            int sourceStart = 0;  // starting from 0 is a conservative choice
            for (int i = 0; i < originLen; i++) {
                if (origin[i] == '^') {
                    sourceStart = i + 1;
                    break;
                }
            }
            if (sourceStart == 0) {
                utils::panic("serialization", "didn't find source code beginner");
            }
            source = origin.substr(sourceStart, std::stoi(sourceLenStr));
            // (2) AST construction (parsing and static analysis) (TODO: exceptions?)
            expr = syntax::parse(syntax::lex(source));
            // (3) stack initialization
            int stackStart = sourceStart + std::stoi(sourceLenStr) + std::string(" } ").size();
            if (stackStart >= static_cast<int>(origin.size())) {
                utils::panic("serialization", "didn't find stack beginner");
            }
            // split the rest of "origin" into a "Sentence"
            serialization::Sentence sentence;
            {
                std::istringstream sin(origin.substr(stackStart, origin.size()));
                std::string word;
                while (sin >> word) {
                    sentence.push_back(word);
                    // special treatment of string values
                    if (word == "sval" && sentence.size() > 1 && *(sentence.rbegin() + 1) == "(") {
                        sin >> word;
                        sentence.push_back(word);
                        int len = std::stoi(word);
                        while (true) {
                            if (sin.get() == '"') {
                                break;
                            }
                            if (sin.eof()) {
                                utils::panic("serialization", "didn't find start of str val");
                            }
                        }
                        word = "\"";
                        for (int i = 0; i < len - 1; i++) {
                            word.push_back(sin.get());
                            if (sin.eof()) {
                                utils::panic("serialization", "incomplete string");
                            }
                        }
                        if (word.back() != '"') {
                            utils::panic("serialization", "invalid string terminator");
                        }
                        sentence.push_back(word);
                    }
                }
            }
            auto stackSentence = serialization::takeTo(sentence, "}");
            if (stackSentence.size() < 3) {  // { stk }
                utils::panic("serialization", "invalid stack");
            }
            stackSentence.pop_front();  // "{"
            stackSentence.pop_front();  // "stk"
            while (stackSentence.size() > 0 && stackSentence.front() == "<") {
                auto layerSentence = serialization::takeTo(stackSentence, ">");
                stack.push_back(serialization::sentenceToLayer(layerSentence, expr));
            }
            if (stackSentence.empty() || stackSentence.front() != "}") {
                utils::panic("serialization", "invalid stack terminator");
            }
            stackSentence.pop_front();  // "}"; this isn't really necessary here
            // (4) heap initialization
            auto heapSentence = serialization::takeTo(sentence, "}");
            if (heapSentence.size() < 3) {  // { hp }
                utils::panic("serialization", "invalid heap");
            }
            heapSentence.pop_front();  // "{"
            heapSentence.pop_front();  // "hp"
            while (heapSentence.size() > 0 && heapSentence.front() == "(") {
                auto valueSentence = serialization::takeTo(heapSentence, ")");
                heap.push_back(serialization::sentenceToValue(
                    valueSentence, expr
                ));
            }
            if (heapSentence.empty() || heapSentence.front() != "}") {
                utils::panic("serialization", "invalid heap terminator");
            }
            heapSentence.pop_front();  // "}"; this isn't really necessary here
            // (5) literal boundary initialization
            if (sentence.size() < 4) {
                utils::panic("serialization", "incomplete literal boundary");
            }
            sentence.pop_front();  // "{"
            sentence.pop_front();  // "nLit"
            numLiterals = std::stoi(sentence.front());
            sentence.pop_front();  // <nLit>
            sentence.pop_front();  // "}"
            // (6) result location initialization
            if (sentence.size() < 4) {
                utils::panic("serialization", "incomplete result location");
            }
            sentence.pop_front();  // "{"
            sentence.pop_front();  // "rLoc"
            resultLoc = std::stoi(sentence.front());
            sentence.pop_front();  // <rLoc>
            sentence.pop_front();  // "}"; this isn't really necessary here
            // _populateStaticAnalysisResults don't do preAlloc
            // because heap will be reconstructed
            // instead, do findPreAlloc to find out previously allocated locations
            // this has to come after (5) literal boundary initialization
            // because it needs numLiterals
            _populateStaticAnalysisResults(false);
        }
    }
private:
    void _migrateAST(const State& state1, State& state2) {
        for (auto& layer : state2.stack) {
            if (layer.expr != nullptr) {
                layer.expr = serialization::migrateASTNode(state1.expr, state2.expr, layer.expr);
            }
        }
        for (auto& value : state2.heap) {
            if (std::holds_alternative<runtime::Closure>(value)) {
                auto& closure = std::get<runtime::Closure>(value);
                closure.fun = dynamic_cast<const syntax::LambdaNode*>(
                    serialization::migrateASTNode(state1.expr, state2.expr,
                        closure.fun)
                );
            }
        }
    }
public:
    State(const State& state) :
        source(state.source),
        expr(state.expr->clone()),
        stack(state.stack),
        heap(state.heap),
        numLiterals(state.numLiterals),
        resultLoc(state.resultLoc) {
        _migrateAST(state, *this);
    }
    State& operator=(const State& state) {
        if (this != &state) {
            source = state.source;
            delete expr;
            expr = state.expr->clone();
            stack = state.stack;
            heap = state.heap;
            numLiterals = state.numLiterals;
            resultLoc = state.resultLoc;
            _migrateAST(state, *this);
        }
        return *this;
    }
    State(State&& state) :
        source(std::move(state.source)),
        expr(state.expr),
        stack(std::move(state.stack)),
        heap(std::move(state.heap)),
        numLiterals(state.numLiterals),
        resultLoc(state.resultLoc) {
        state.expr = nullptr;
    }
    State& operator=(State&& state) {
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
private:
    template <typename... Alt>
        requires (true && ... &&
            (std::same_as<Alt, runtime::Value> || utils::isAlternativeOf<Alt, runtime::Value>))
    void _typecheck(utils::SourceLocation sl, const std::vector<syntax::Location>& args) {
        bool ok = args.size() == sizeof...(Alt);
        int i = -1;
        ok = ok && (true && ... && (
            i++,
            [&] {
                if constexpr (std::same_as<Alt, runtime::Value>) {
                    return true;
                }
                else {
                    return std::holds_alternative<Alt>(heap[args[i]]);
                }
            } ()
        ));
        if (!ok) {
            _errorStack();
            utils::panic("runtime", "type error on intrinsic call", sl);
        }
    }
    // intrinsic dispatch
    runtime::Value _callIntrinsic(utils::SourceLocation sl,
        const std::string& name, const std::vector<syntax::Location>& args) {
        if (name == ".void") {
            _typecheck<>(sl, args);
            return runtime::Void();
        }
        else if (name == ".+") {
            _typecheck<runtime::Integer, runtime::Integer>(sl, args);
            return runtime::Integer(std::get<runtime::Integer>(heap[args[0]]).value +
                std::get<runtime::Integer>(heap[args[1]]).value);
        }
        else if (name == ".-") {
            _typecheck<runtime::Integer, runtime::Integer>(sl, args);
            return runtime::Integer(std::get<runtime::Integer>(heap[args[0]]).value -
                std::get<runtime::Integer>(heap[args[1]]).value);
        }
        else if (name == ".*") {
            _typecheck<runtime::Integer, runtime::Integer>(sl, args);
            return runtime::Integer(std::get<runtime::Integer>(heap[args[0]]).value *
                std::get<runtime::Integer>(heap[args[1]]).value);
        }
        else if (name == "./") {
            _typecheck<runtime::Integer, runtime::Integer>(sl, args);
            int d = std::get<runtime::Integer>(heap[args[1]]).value;
            if (d == 0) {
                utils::panic("runtime", "division by zero", sl);
            }
            return runtime::Integer(std::get<runtime::Integer>(heap[args[0]]).value / d);
        }
        else if (name == ".%") {
            _typecheck<runtime::Integer, runtime::Integer>(sl, args);
            int d = std::get<runtime::Integer>(heap[args[1]]).value;
            if (d == 0) {
                utils::panic("runtime", "division by zero", sl);
            }
            return runtime::Integer(std::get<runtime::Integer>(heap[args[0]]).value % d);
        }
        else if (name == ".<") {
            _typecheck<runtime::Integer, runtime::Integer>(sl, args);
            return runtime::Integer(std::get<runtime::Integer>(heap[args[0]]).value <
                std::get<runtime::Integer>(heap[args[1]]).value ? 1 : 0);
        }
        else if (name == ".<=") {
            _typecheck<runtime::Integer, runtime::Integer>(sl, args);
            return runtime::Integer(std::get<runtime::Integer>(heap[args[0]]).value <=
                std::get<runtime::Integer>(heap[args[1]]).value ? 1 : 0);
        }
        else if (name == ".>") {
            _typecheck<runtime::Integer, runtime::Integer>(sl, args);
            return runtime::Integer(std::get<runtime::Integer>(heap[args[0]]).value >
                std::get<runtime::Integer>(heap[args[1]]).value ? 1 : 0);
        }
        else if (name == ".>=") {
            _typecheck<runtime::Integer, runtime::Integer>(sl, args);
            return runtime::Integer(std::get<runtime::Integer>(heap[args[0]]).value >=
                std::get<runtime::Integer>(heap[args[1]]).value ? 1 : 0);
        }
        else if (name == ".=") {
            _typecheck<runtime::Integer, runtime::Integer>(sl, args);
            return runtime::Integer(std::get<runtime::Integer>(heap[args[0]]).value ==
                std::get<runtime::Integer>(heap[args[1]]).value ? 1 : 0);
        }
        else if (name == "./=") {
            _typecheck<runtime::Integer, runtime::Integer>(sl, args);
            return runtime::Integer(std::get<runtime::Integer>(heap[args[0]]).value !=
                std::get<runtime::Integer>(heap[args[1]]).value ? 1 : 0);
        }
        else if (name == ".and") {
            _typecheck<runtime::Integer, runtime::Integer>(sl, args);
            return runtime::Integer(std::get<runtime::Integer>(heap[args[0]]).value &&
                std::get<runtime::Integer>(heap[args[1]]).value ? 1 : 0);
        }
        else if (name == ".or") {
            _typecheck<runtime::Integer, runtime::Integer>(sl, args);
            return runtime::Integer(std::get<runtime::Integer>(heap[args[0]]).value ||
                std::get<runtime::Integer>(heap[args[1]]).value ? 1 : 0);
        }
        else if (name == ".not") {
            _typecheck<runtime::Integer>(sl, args);
            return runtime::Integer(std::get<runtime::Integer>(heap[args[0]]).value ? 0 : 1);
        }
        else if (name == ".s+") {
            _typecheck<runtime::String, runtime::String>(sl, args);
            return runtime::String(std::get<runtime::String>(heap[args[0]]).value +
                std::get<runtime::String>(heap[args[1]]).value);
        }
        else if (name == ".s<") {
            _typecheck<runtime::String, runtime::String>(sl, args);
            return runtime::Integer(std::get<runtime::String>(heap[args[0]]).value <
                std::get<runtime::String>(heap[args[1]]).value ? 1 : 0);
        }
        else if (name == ".s<=") {
            _typecheck<runtime::String, runtime::String>(sl, args);
            return runtime::Integer(std::get<runtime::String>(heap[args[0]]).value <=
                std::get<runtime::String>(heap[args[1]]).value ? 1 : 0);
        }
        else if (name == ".s>") {
            _typecheck<runtime::String, runtime::String>(sl, args);
            return runtime::Integer(std::get<runtime::String>(heap[args[0]]).value >
                std::get<runtime::String>(heap[args[1]]).value ? 1 : 0);
        }
        else if (name == ".s>=") {
            _typecheck<runtime::String, runtime::String>(sl, args);
            return runtime::Integer(std::get<runtime::String>(heap[args[0]]).value >=
                std::get<runtime::String>(heap[args[1]]).value ? 1 : 0);
        }
        else if (name == ".s=") {
            _typecheck<runtime::String, runtime::String>(sl, args);
            return runtime::Integer(std::get<runtime::String>(heap[args[0]]).value ==
                std::get<runtime::String>(heap[args[1]]).value ? 1 : 0);
        }
        else if (name == ".s/=") {
            _typecheck<runtime::String, runtime::String>(sl, args);
            return runtime::Integer(std::get<runtime::String>(heap[args[0]]).value !=
                std::get<runtime::String>(heap[args[1]]).value ? 1 : 0);
        }
        else if (name == ".s||") {
            _typecheck<runtime::String>(sl, args);
            return runtime::Integer(std::get<runtime::String>(heap[args[0]]).value.size());
        }
        else if (name == ".s[]") {
            _typecheck<runtime::String, runtime::Integer, runtime::Integer>(sl, args);
            int n = std::get<runtime::String>(heap[args[0]]).value.size();
            int l = std::get<runtime::Integer>(heap[args[1]]).value;
            int r = std::get<runtime::Integer>(heap[args[2]]).value;
            if (!((0 <= l && l < n) && (0 <= r && r <= n) && (l <= r))) {
                utils::panic("runtime", "invalid substring range", sl);
            }
            return runtime::String(std::get<runtime::String>(heap[args[0]]).value.substr(l, r - l));
        }
        else if (name == ".quote") {
            _typecheck<runtime::String>(sl, args);
            return runtime::String(syntax::quote(std::get<runtime::String>(heap[args[0]]).value));
        }
        else if (name == ".unquote") {
            _typecheck<runtime::String>(sl, args);
            return runtime::String(syntax::unquote(std::get<runtime::String>(heap[args[0]]).value));
        }
        else if (name == ".s->i") {
            _typecheck<runtime::String>(sl, args);
            return runtime::Integer(std::stoi(std::get<runtime::String>(heap[args[0]]).value)
                /* TODO: exceptions */);
        }
        else if (name == ".i->s") {
            _typecheck<runtime::Integer>(sl, args);
            return runtime::String(std::to_string(std::get<runtime::Integer>(heap[args[0]]).value));
        }
        else if (name == ".type") {
            _typecheck<runtime::Value>(sl, args);
            int label = -1;
            if (std::holds_alternative<runtime::Void>(heap[args[0]])) {
                label = 0;
            }
            else if (std::holds_alternative<runtime::Integer>(heap[args[0]])) {
                label = 1;
            }
            else if (std::holds_alternative<runtime::String>(heap[args[0]])) {
                label = 2;
            }
            else {
                label = 3;
            }
            return runtime::Integer(label);
        }
        else if (name == ".eval") {
            // Note: evaluating a string as a program is one "step"
            // and thus currently cannot be divided by the serialization
            // in a fine-grained way.
            _typecheck<runtime::String>(sl, args);
            State state(std::get<runtime::String>(heap[args[0]]).value);
            state.execute();
            // cannot return Closure because it may
            // refer to a node in the AST dynamically
            // constructed from .eval's argument
            if (std::holds_alternative<runtime::Closure>(state.getResult())) {
                utils::panic("runtime", "returning a closure from .eval");
            }
            return state.getResult();  // this should be a copy
        }
        else if (name == ".forkstate") {
            _typecheck<>(sl, args);
            // simulate the action of intrinsic return of Void()
            // and record old values
            syntax::Location oldLoc = resultLoc;
            resultLoc = _new<runtime::Void>();
            runtime::Layer oldStackLayer = stack.back();
            stack.pop_back();
            // now the state is the same as the one after the return of Void()
            // serialize it and save it
            std::string serializedState = serialize();
            // revert the state back
            resultLoc = oldLoc;
            heap.pop_back();
            stack.push_back(oldStackLayer);
            // do the actual return of the serialized state
            return runtime::String(serializedState);
        }
        else if (name == ".getchar") {
            _typecheck<>(sl, args);
            auto c = std::cin.get();
            if (std::cin.eof()) {
                return runtime::Void();
            }
            else {
                std::string s;
                s.push_back(static_cast<char>(c));
                return runtime::String(s);
            }
        }
        else if (name == ".getint") {
            _typecheck<>(sl, args);
            int v;
            if (std::cin >> v) {
                return runtime::Integer(v);
            }
            else {
                return runtime::Void();
            }
        }
        else if (name == ".putstr") {
            _typecheck<runtime::String>(sl, args);
            std::cout << std::get<runtime::String>(heap[args[0]]).value;
            return runtime::Void();
        }
        else if (name == ".flush") {
            _typecheck<>(sl, args);
            std::cout << std::flush;
            return runtime::Void();
        }
        else {
            _errorStack();
            utils::panic("runtime", "unrecognized intrinsic call", sl);
            // unreachable
            return runtime::Void();
        }
    }
    // memory management
    template <typename V, typename... Args>
        requires utils::isAlternativeOf<V, runtime::Value>
    syntax::Location _new(Args&&... args) {
        heap.push_back(std::move(V(std::forward<Args>(args)...)));
        return heap.size() - 1;
    }
    syntax::Location _moveNew(runtime::Value v) {
        heap.push_back(std::move(v));
        return heap.size() - 1;
    }
    std::unordered_set<syntax::Location> _mark() {
        std::unordered_set<syntax::Location> visited;
        // for each traversed location, specifically handle the closure case
        std::function<void(syntax::Location)> traverseLocation =
            // "this" captures the current object by reference
            [this, &visited, &traverseLocation](syntax::Location loc) {
            if (!(visited.contains(loc))) {
                visited.insert(loc);
                if (std::holds_alternative<runtime::Closure>(heap[loc])) {
                    for (const auto& [_, l] : std::get<runtime::Closure>(heap[loc]).env) {
                        traverseLocation(l);
                    }
                }
            }
        };
        // traverse the stack
        for (const auto& layer : stack) {
            // only frames "own" the environments
            if (layer.frame) {
                for (const auto& [_, loc] : (layer.env)) {
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
    std::pair<int, std::unordered_map<syntax::Location, syntax::Location>>
        _sweepAndCompact(const std::unordered_set<syntax::Location>& visited) {
        std::unordered_map<syntax::Location, syntax::Location> relocation;
        syntax::Location n = heap.size();
        syntax::Location i = numLiterals, j = numLiterals;
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
    void _relocate(const std::unordered_map<syntax::Location, syntax::Location>& relocation) {
        auto reloc = [&relocation](syntax::Location& loc) -> void {
            if (relocation.contains(loc)) {
                loc = relocation.at(loc);
            }
        };
        // traverse the stack
        for (auto& layer : stack) {
            // only frames "own" the environments
            if (layer.frame) {
                for (auto& [_, loc] : (layer.env)) {
                    reloc(loc);
                }
            }
            // but each layer can still have locals
            for (auto& v : layer.local) {
                reloc(v);
            }
        }
        // traverse the resultLoc
        reloc(resultLoc);
        // traverse the closure values
        for (auto& v : heap) {
            if (std::holds_alternative<runtime::Closure>(v)) {
                auto& c = std::get<runtime::Closure>(v);
                for (auto& [_, loc] : c.env) {
                    reloc(loc);
                }
            }
        }
    }
    int _gc() {
        auto visited = _mark();
        const auto& [removed, relocation] = _sweepAndCompact(visited);
        _relocate(relocation);
        return removed;
    }
    std::vector<utils::SourceLocation> _getFrameSLs() {
        std::vector<utils::SourceLocation> frameSLs;
        for (const auto& l : stack) {
            if (l.frame) {
                if (l.expr == nullptr) {  // main frame
                    frameSLs.emplace_back(1, 1);
                }
                else {
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
    runtime::Env& _getEnv() {
        for (auto p = stack.rbegin(); p != stack.rend(); p++) {
            if (p->frame) {
                return p->env;
            }
        }
        // should be unreachable
        return stack.back().env;
    }
public:
    // returns true iff the step is completed without reaching the end of evaluation
    bool step() {
        // be careful! this reference may be invalidated after modifying the stack
        // so always keep stack change as the last operation(s)
        auto& layer = stack.back();
        // main frame; end of evaluation
        if (layer.expr == nullptr) {
            return false;
        }
        // evaluations for every case
        if (auto inode = dynamic_cast<const syntax::IntegerNode*>(layer.expr)) {
            resultLoc = inode->loc;
            stack.pop_back();
        }
        else if (auto snode = dynamic_cast<const syntax::StringNode*>(layer.expr)) {
            resultLoc = snode->loc;
            stack.pop_back();
        }
        else if (auto vnode = dynamic_cast<const syntax::VariableNode*>(layer.expr)) {
            auto varName = vnode->name;
            auto loc = runtime::lookup(varName, _getEnv());
            if (!loc.has_value()) {
                _errorStack();
                utils::panic("runtime", "undefined variable " + varName, layer.expr->sl);
            }
            resultLoc = loc.value();
            stack.pop_back();
        }
        else if (auto lnode = dynamic_cast<const syntax::LambdaNode*>(layer.expr)) {
            // copy the statically used part of the env into the closure
            runtime::Env savedEnv;
            // copy
            auto usedVars = lnode->freeVars;
            auto& env = _getEnv();
            for (auto ptr = env.rbegin(); ptr != env.rend(); ptr++) {
                if (usedVars.empty()) {
                    break;
                }
                if (usedVars.contains(ptr->first)) {
                    savedEnv.push_back(*ptr);
                    usedVars.erase(ptr->first);
                }
            }
            std::reverse(savedEnv.begin(), savedEnv.end());
            resultLoc = _new<runtime::Closure>(savedEnv, lnode);
            stack.pop_back();
        }
        else if (auto lnode = dynamic_cast<const syntax::LetrecNode*>(layer.expr)) {
            // unified argument recording
            if (layer.pc > 1 && layer.pc <= static_cast<int>(lnode->varExprList.size()) + 1) {
                auto varName = lnode->varExprList[layer.pc - 2].first->name;
                auto loc = runtime::lookup(varName, _getEnv());
                // this shouldn't happen since those variables are newly introduced by letrec
                if (!loc.has_value()) {
                    _errorStack();
                    utils::panic("runtime", "undefined variable " + varName, layer.expr->sl);
                }
                // copy (inherited resultLoc)
                heap[loc.value()] = heap[resultLoc];
            }
            // create all new locations
            if (layer.pc == 0) {
                layer.pc++;
                auto& env = _getEnv();
                for (const auto& [var, _] : lnode->varExprList) {
                    env.push_back(std::make_pair(var->name, _new<runtime::Void>()));
                }
            }
            // evaluate bindings
            else if (layer.pc <= static_cast<int>(lnode->varExprList.size())) {
                layer.pc++;
                // note: growing the stack might invalidate the reference "layer"
                //       but this is fine since next time "layer" will be re-bound
                stack.emplace_back(runtime::Env(), lnode->varExprList[layer.pc - 2].second);
            }
            // evaluate body
            else if (layer.pc == static_cast<int>(lnode->varExprList.size()) + 1) {
                layer.pc++;
                stack.emplace_back(runtime::Env(), lnode->expr);
            }
            // finish letrec
            else {
                int nParams = lnode->varExprList.size();
                auto& env = _getEnv();
                for (int i = 0; i < nParams; i++) {
                    env.pop_back();
                }
                // this layer cannot be optimized by TCO because we need nParams to revert env
                // no need to update resultLoc: inherited from body evaluation
                stack.pop_back();
            }
        }
        else if (auto inode = dynamic_cast<const syntax::IfNode*>(layer.expr)) {
            // evaluate condition
            if (layer.pc == 0) {
                layer.pc++;
                stack.emplace_back(runtime::Env(), inode->cond);
            }
            // evaluate one branch
            else if (layer.pc == 1) {
                layer.pc++;
                // inherited condition value
                if (!std::holds_alternative<runtime::Integer>(heap[resultLoc])) {
                    _errorStack();
                    utils::panic("runtime", "wrong cond type", layer.expr->sl);
                }
                if (std::get<runtime::Integer>(heap[resultLoc]).value) {
                    stack.emplace_back(runtime::Env(), inode->branch1);
                }
                else {
                    stack.emplace_back(runtime::Env(), inode->branch2);
                }
            }
            // finish if
            else {
                // no need to update resultLoc: inherited
                stack.pop_back();
            }
        }
        else if (auto snode = dynamic_cast<const syntax::SequenceNode*>(layer.expr)) {
            // evaluate one-by-one
            if (layer.pc < static_cast<int>(snode->exprList.size())) {
                layer.pc++;
                stack.emplace_back(runtime::Env(), snode->exprList[layer.pc - 1]);
            }
            // finish
            else {
                // sequence's value is the last expression's value
                // no need to update resultLoc: inherited
                stack.pop_back();
            }
        }
        else if (auto inode = dynamic_cast<const syntax::IntrinsicCallNode*>(layer.expr)) {
            // unified argument recording
            if (layer.pc > 0 && layer.pc <= static_cast<int>(inode->argList.size())) {
                layer.local.push_back(resultLoc);
            }
            // evaluate arguments
            if (layer.pc < static_cast<int>(inode->argList.size())) {
                layer.pc++;
                stack.emplace_back(runtime::Env(), inode->argList[layer.pc - 1]);
            }
            // intrinsic call doesn't grow the stack
            else {
                auto value = _callIntrinsic(layer.expr->sl, inode->intrinsic,
                    layer.local /* intrinsic call is pass by reference */);
                resultLoc = _moveNew(std::move(value));
                stack.pop_back();
            }
        }
        else if (auto enode = dynamic_cast<const syntax::ExprCallNode*>(layer.expr)) {
            // unified argument recording
            if (layer.pc > 2 && layer.pc <= static_cast<int>(enode->argList.size()) + 2) {
                layer.local.push_back(resultLoc);
            }
            // evaluate the callee
            if (layer.pc == 0) {
                layer.pc++;
                stack.emplace_back(runtime::Env(), enode->expr);
            }
            // initialization
            else if (layer.pc == 1) {
                layer.pc++;
                // inherited callee location
                layer.local.push_back(resultLoc);
            }
            // evaluate arguments
            else if (layer.pc <= static_cast<int>(enode->argList.size()) + 1) {
                layer.pc++;
                stack.emplace_back(runtime::Env(), enode->argList[layer.pc - 3]);
            }
            // call
            else if (layer.pc == static_cast<int>(enode->argList.size()) + 2) {
                layer.pc++;
                auto exprLoc = layer.local[0];
                if (!std::holds_alternative<runtime::Closure>(heap[exprLoc])) {
                    _errorStack();
                    utils::panic("runtime", "calling a non-callable", layer.expr->sl);
                }
                auto& closure = std::get<runtime::Closure>(heap[exprLoc]);
                // types will be checked inside the closure call
                if (static_cast<int>(layer.local.size()) - 1 !=
                    static_cast<int>(closure.fun->varList.size())) {
                    _errorStack();
                    utils::panic("runtime", "wrong number of arguments", layer.expr->sl);
                }
                int nArgs = static_cast<int>(closure.fun->varList.size());
                // lexical scope: copy the env from the closure definition place
                auto newEnv = closure.env;
                for (int i = 0; i < nArgs; i++) {
                    // closure call is pass by reference
                    newEnv.push_back(std::make_pair(closure.fun->varList[i]->name,
                        layer.local[i + 1]));
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
                stack.emplace_back(std::move(newEnv) /* new frame has new env */,
                    closure.fun->expr, true);
            }
            // finish
            else {
                // no need to update resultLoc: inherited
                stack.pop_back();
            }
        }
        else if (auto anode = dynamic_cast<const syntax::AtNode*>(layer.expr)) {
            // evaluate the expr
            if (layer.pc == 0) {
                layer.pc++;
                stack.emplace_back(runtime::Env(), anode->expr);
            }
            else {
                // inherited resultLoc
                if (!std::holds_alternative<runtime::Closure>(heap[resultLoc])) {
                    _errorStack();
                    utils::panic("runtime", "@ wrong type", layer.expr->sl);
                }
                auto varName = anode->var->name;
                auto loc = runtime::lookup(varName,
                    std::get<runtime::Closure>(heap[resultLoc]).env);
                if (!loc.has_value()) {
                    _errorStack();
                    utils::panic("runtime", "undefined variable " + varName, layer.expr->sl);
                }
                // "access by reference"
                resultLoc = loc.value();
                stack.pop_back();
            }
        }
        else {
            _errorStack();
            utils::panic("runtime", "unrecognized AST node", layer.expr->sl);
        }
        return true;
    }
    void execute() {
        // can choose different initial values here
        // Note: when the program state is recovered
        // from de-serialization, the value of gc_threshold
        // is not preserved and will re-start from this initial value
        // when calling execute().
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
    const runtime::Value& getResult() const {
        return heap[resultLoc];
    }
    const syntax::ExprNode* getExpr() const {
        return expr;
    }
    // TODO: record GC state?
    std::string serialize() const {
        // std::string source;
        std::string serializedSource =
            "{ src " + std::to_string(source.size()) + " ^" + source + " }";
        // syntax::ExprNode *expr;
        // (skipped, because source is more accurate)
        std::string serializedState;
        // std::vector<Layer> stack;
        serializedState += "{ stk";
        for (auto& layer : stack) {
            serializedState += " ";
            serializedState += serialization::join(serialization::layerToSentence(layer, expr));
        }
        serializedState += " }";
        // std::vector<Value> heap;
        serializedState += " { hp";
        for (auto& value : heap) {
            serializedState += " ";
            serializedState += serialization::join(serialization::valueToSentence(value, expr));
        }
        serializedState += " }";
        // int numLiterals;
        serializedState += " { nLit ";
        serializedState += std::to_string(numLiterals);
        serializedState += " }";
        // Location resultLoc;
        serializedState += " { rLoc ";
        serializedState += std::to_string(resultLoc);
        serializedState += " }";
        auto ret = "|" + serializedSource + " " + serializedState + "|";
        return ret;
    }
private:
    // states
    std::string source;
    syntax::ExprNode* expr;
    std::vector<runtime::Layer> stack;
    std::vector<runtime::Value> heap;
    int numLiterals;
    syntax::Location resultLoc;
};

// ------------------------------
// main
// ------------------------------

std::string readSource(const std::string& spath) {
    if (!std::filesystem::exists(spath)) {
        utils::panic("reader", spath + " does not exist.");
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

int main(int argc, char** argv) {
    if (argc != 2) {
        std::cerr << "Usage: " << argv[0] << " <source-path>\n";
        std::exit(EXIT_FAILURE);
    }
    try {
#ifndef DEBUG
        State state(readSource(argv[1]));
        state.execute();
        std::cout << "<end-of-stdout>\n"
            << serialization::join(
                serialization::valueToSentence(state.getResult(), state.getExpr())) << std::endl;
#else
        // test the copy / move assignment operators for State
        std::string source = readSource(argv[1]);
        State state1(source);
        {
            State state2(source);
            {
                State state3(source);
                state2 = state3;
                // destruct state3
            }
            state1 = std::move(state2);
            // destruct state2
        }
        state1.execute();
        std::cout << "<end-of-stdout>\n"
            << serialization::join(
                serialization::valueToSentence(state1.getResult(), state1.getExpr())) << std::endl;
#endif
    }
    catch (const std::runtime_error& e) {
        std::cerr << e.what() << std::endl;
        std::exit(EXIT_FAILURE);
    }
}

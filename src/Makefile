CXX = clang++
WARNING = -Wall -Wextra -pedantic
STD = -std=c++20
SRC = main.cpp
DST = -o ../bin/clo

debug: main.cpp
	$(CXX) $(WARNING) $(STD) -g -fsanitize=address,undefined -fno-omit-frame-pointer -fno-sanitize-recover=all $(SRC) $(DST)

release: main.cpp
	$(CXX) $(WARNING) $(STD) -O3 $(SRC) $(DST)

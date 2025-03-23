#include <string>
#include <iostream>

// maybe one variable length thing we'd consider adding at the toplevel is just "message"

// these are really inclusive of a null terminator
#define MAX_INT_CHARS 10
#define MAX_BOOL_CHARS 7

struct test_struct {
    int x = 0;
    int y = 0;
    bool b = false;

    test_struct(int X, int Y, bool B) : x{X}, y{Y}, b{B} { }

    static constexpr int x_start = 5;  // 2 + len(first member name) + 2
    static constexpr int y_start = 20; // x_start + MAX_INT_CHARS + 2 + len(second member name) + 2
    static constexpr int b_start = 35; // y_start + MAX_INT_CHARS + 2 + len(third member name) + 2

    static std::string empty()
    {
        return "{\"x\":          ,\"y\":          ,\"b\":       }";
    }

    void format(char* buf) const { 
        snprintf(buf + x_start, MAX_INT_CHARS, "%*d", MAX_INT_CHARS - 1, x);
        snprintf(buf + y_start, MAX_INT_CHARS, "%*d", MAX_INT_CHARS - 1, y);
        snprintf(buf + b_start, MAX_BOOL_CHARS, "%*s", MAX_BOOL_CHARS - 1, b ? "true" : "false");
    }
};

// let's do a nested example

struct outer_struct {
    int x = 0;
    test_struct inner;

    outer_struct(int X, int Y, bool B) : x{X}, inner{X,Y,B} { }

    static constexpr int x_start = 5;      // 2 + len(first member name) + 2
    static constexpr int inner_start = 24; // x_start + MAX_INT_CHARS + 2 + len(second member name) + 2

    // style 2 -- offsets from prior (so, exclude the offset for the prior member)
    static constexpr int x_offset = 5;      // 2 + len(first member name) + 2
    static constexpr int inner_offset = 19; // MAX_INT_CHARS + 2 + len(second member name) + 2

    // MAX_{PREV_MEMBER_TYPE}_CHARS + 2 + len(my name) + 2

    static std::string empty()
    {
        return std::string{"{\"x\":          "} + std::string{",\"inner\":"} + test_struct::empty() + std::string{"}"};
    }

    // guaranteed to only write within [buf, buf+empty().size())
    void format(char* buf) const { 
        auto head { buf };

        head += x_offset;
        snprintf(head, MAX_INT_CHARS, "%*d", MAX_INT_CHARS - 1, x);

        head += inner_offset;
        inner.format(head);
    }
};

template <typename TStruct>
class json_logger
{
    std::string _buf{TStruct::empty()};
public:
    json_logger() {}

    void log(const TStruct& t) {
        t.format(_buf.data());
        std::cout << _buf << std::endl;
    }
};
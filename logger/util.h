#include <dragonbox/dragonbox_to_chars.h>

template <typename T>
struct is_signed_int : std::false_type {};
template <> struct is_signed_int<int8_t> : std::true_type {};
template <> struct is_signed_int<int16_t> : std::true_type {};
template <> struct is_signed_int<int32_t> : std::true_type {};
template <> struct is_signed_int<int64_t> : std::true_type {};

// another stupid optimization to consider
// if char at head is already == pad after formatting the value,
// just return

// also for negatives, remove branching by indexing 0 or 1 into a fixed 
// size array for the - sign? other char is pad

template <typename T, 
typename = std::enable_if_t<is_signed_int<T>::value>,
typename = void> // differentiate from uints
void write_backwards(T value, char* buf, int count, char pad) 
{
    bool neg { value < 0 };
    value = std::abs(value);
    int iter = 0;
    if (value == 0) {
        *buf = '0';
        buf--;
        iter++;
    }

    while (value > 0)
    {
        *buf = "0123456789"[value % 10];
        value /= 10;
        buf--;
        iter++;
    }

    if (neg)
    {
        *buf = '-';
        buf--;
        iter++;
    }

    while (iter < count)
    {
        *buf = pad;
        buf--;
        iter++;
    }
}

template <typename T>
struct is_unsigned_int : std::false_type {};
template <> struct is_unsigned_int<uint8_t> : std::true_type {};
template <> struct is_unsigned_int<uint16_t> : std::true_type {};
template <> struct is_unsigned_int<uint32_t> : std::true_type {};
template <> struct is_unsigned_int<uint64_t> : std::true_type {};

template <typename T, typename = std::enable_if_t<is_unsigned_int<T>::value>>
void write_backwards(T value, char* buf, int count, char pad) 
{
    int iter = 0;
    if (value == 0) {
        *buf = '0';
        buf--;
        iter++;
    }

    while (value > 0)
    {
        *buf = "0123456789"[value % 10];
        value /= 10;
        buf--;
        iter++;
    }

    while (iter < count)
    {
        *buf = pad;
        buf--;
        iter++;
    }
}

template <typename T>
struct is_bool : std::false_type {};
template <> struct is_bool<bool> : std::true_type {};

template <typename T, 
typename = std::enable_if_t<is_bool<T>::value>,
typename = void,
typename = void>
void write_backwards(T value, char* buf, int count, char pad) 
{
    int remaining = 0;
    if (value)
    {
        *(buf--) = 'e';
        *(buf--) = 'u';
        *(buf--) = 'r';
        *(buf--) = 't';
        remaining = count - 4;
    }
    else
    {
        *(buf--) = 'e';
        *(buf--) = 's';
        *(buf--) = 'l';
        *(buf--) = 'a';
        *(buf--) = 'f';
        remaining = count - 5;
    }

    while (remaining > 0)
    {
        *(buf--) = pad;
        remaining--;
    }
}

// struct ieee754_binary32 {
//     static constexpr int total_bits = 32;
//     static constexpr int significand_bits = 23;
//     static constexpr int exponent_bits = 8;
//     static constexpr int min_exponent = -126;
//     static constexpr int max_exponent = 127;
//     static constexpr int exponent_bias = -127;
//     static constexpr int decimal_significand_digits = 9;
//     static constexpr int decimal_exponent_digits = 2;
// };
// struct ieee754_binary64 {
//     static constexpr int total_bits = 64;
//     static constexpr int significand_bits = 52;
//     static constexpr int exponent_bits = 11;
//     static constexpr int min_exponent = -1022;
//     static constexpr int max_exponent = 1023;
//     static constexpr int exponent_bias = -1023;
//     static constexpr int decimal_significand_digits = 17;
//     static constexpr int decimal_exponent_digits = 3;
// };

constexpr int max_double_length = jkj::dragonbox::max_output_string_length<jkj::dragonbox::ieee754_binary64>;
constexpr int max_float_length = jkj::dragonbox::max_output_string_length<jkj::dragonbox::ieee754_binary32>;

void write_backwards(float value, char* buf, int count, char pad) 
{
    // constexpr int buffer_length =
    //   jkj::dragonbox::max_output_string_length<jkj::dragonbox::ieee754_binary64>;
    // double x = 1.234;  // Also works for float
    // char buffer[buffer_length];

    // Does not null-terminate the buffer; returns the next-to-end pointer
    // buffer is now { '1', '.', '2', '3', '4', 'E', '0', (garbages) }
    // you can wrap the buffer with things like std::string_view
    jkj::dragonbox::to_chars_n(value, buf);
}
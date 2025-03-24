template <typename T>
struct is_signed_int : std::false_type {};
template <> struct is_signed_int<int8_t> : std::true_type {};
template <> struct is_signed_int<int16_t> : std::true_type {};
template <> struct is_signed_int<int32_t> : std::true_type {};
template <> struct is_signed_int<int64_t> : std::true_type {};

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
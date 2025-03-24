// add impls for other things
void write_backwards(int32_t value, char* buf, int count, char pad) 
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
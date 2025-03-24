template <typename TStruct>
class json_logger
{
    std::string _buf{TStruct::empty()};
public:
    json_logger() {}

    const char* log(const TStruct& t) {
        t.format(_buf.data());
        return _buf.c_str();
    }
};
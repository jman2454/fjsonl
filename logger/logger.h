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
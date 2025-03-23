#include "structs.h"
#include "logger.h"

int main()
{
    auto logger = json_logger<test_struct_orig>();
    logger.log({2232,313431,0});
    auto logger2 = json_logger<outer_struct>();
    logger2.log({2234,{2232,313431,0},1,{1,2,true},(static_cast<uint64_t>(1))<<63});
}
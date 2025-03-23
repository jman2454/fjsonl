#include "logger.h"

int main()
{
    auto logger = json_logger<test_struct>();
    logger.log({2232,313431,0});
    auto logger2 = json_logger<outer_struct>();
    logger2.log({2232,313431,0});
}
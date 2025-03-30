cmake_minimum_required(VERSION 3.10)
project(json_benchmark)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -O3")

find_package(nlohmann_json REQUIRED)
find_path(RAPIDJSON_INCLUDE_DIRS "rapidjson/document.h")
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../logger)

include(FetchContent)
FetchContent_Declare(
        dragonbox
        GIT_REPOSITORY https://github.com/jk-jeon/dragonbox
)
FetchContent_MakeAvailable(dragonbox)

add_library(dragonbox_impl STATIC ${dragonbox_SOURCE_DIR}/source/dragonbox_to_chars.cpp)
target_include_directories(dragonbox_impl PUBLIC ${dragonbox_SOURCE_DIR}/include)

add_executable(json_benchmark benchmark.cpp)
target_link_libraries(json_benchmark nlohmann_json::nlohmann_json dragonbox_impl)
target_include_directories(json_benchmark PRIVATE
 ${RAPIDJSON_INCLUDE_DIRS}
)
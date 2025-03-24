cmake_minimum_required(VERSION 3.10)
project(json_benchmark)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -O3")

find_package(nlohmann_json REQUIRED)
find_path(RAPIDJSON_INCLUDE_DIRS "rapidjson/document.h")
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../logger)

add_executable(json_benchmark benchmark.cpp)
target_link_libraries(json_benchmark PRIVATE nlohmann_json::nlohmann_json)
target_include_directories(json_benchmark PRIVATE ${RAPIDJSON_INCLUDE_DIRS})
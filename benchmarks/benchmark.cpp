#include <iostream>
#include <chrono>
#include <random>
#include <vector>
#include <cstdint>
#include <string>
#include <functional>
#include <fstream>

#include "logger.h"
#include "structs.h"

// Include JSON libraries
#include <nlohmann/json.hpp>
#include <rapidjson/document.h>
#include <rapidjson/writer.h>
#include <rapidjson/stringbuffer.h>

// Serialization functions for nlohmann::json
namespace nlohmann {
    template<>
    struct adl_serializer<test_struct_orig> {
        static void to_json(json& j, const test_struct_orig& obj) {
            j = json{
                {"x", obj.x},
                {"y", obj.y},
                {"b", obj.b}
            };
        }
    };

    template<>
    struct adl_serializer<outer_struct> {
        static void to_json(json& j, const outer_struct& obj) {
            j = json{
                {"x", obj.x},
                {"inner", obj.inner},
                {"b", obj.b},
                {"ff", obj.ff},
                {"z", obj.z},
                {"f", obj.f}
            };
        }
    };
}

// Serialization functions for RapidJSON
void serialize_test_struct_orig_rapid(rapidjson::Writer<rapidjson::StringBuffer>& writer, const test_struct_orig& obj) {
    writer.StartObject();
    writer.Key("x");
    writer.Int(obj.x);
    writer.Key("y");
    writer.Int(obj.y);
    writer.Key("b");
    writer.Bool(obj.b);
    writer.EndObject();
}

void serialize_outer_struct_rapid(rapidjson::Writer<rapidjson::StringBuffer>& writer, const outer_struct& obj) {
    writer.StartObject();
    writer.Key("x");
    writer.Int(obj.x);
    writer.Key("inner");
    serialize_test_struct_orig_rapid(writer, obj.inner);
    writer.Key("b");
    writer.Bool(obj.b);
    writer.Key("ff");
    writer.Double(obj.ff);
    writer.Key("z");
    serialize_test_struct_orig_rapid(writer, obj.z);
    writer.Key("f");
    writer.Uint64(obj.f);
    writer.EndObject();
}

// Function to generate random struct
void randomize_struct(outer_struct& obj, std::mt19937& rng) {
    std::uniform_int_distribution<int32_t> int_dist(std::numeric_limits<int32_t>::min(), std::numeric_limits<int32_t>::max());
    std::uniform_int_distribution<uint64_t> uint_dist(0, std::numeric_limits<uint64_t>::max());
    std::uniform_int_distribution<int> bool_dist(0, 1);
    
    std::uniform_real_distribution<float> float_dist(
        std::numeric_limits<float>::min(),
        std::numeric_limits<float>::max()        
    );

    obj.x = int_dist(rng);
    obj.inner.x = int_dist(rng);
    obj.inner.y = int_dist(rng);
    obj.inner.b = bool_dist(rng);
    obj.b = bool_dist(rng);
    obj.z.x = int_dist(rng);
    obj.z.y = int_dist(rng);
    obj.z.b = bool_dist(rng);
    obj.f = uint_dist(rng);
    obj.ff = float_dist(rng);
}

// Benchmark function
void run_benchmark(const std::string& library_name, 
                  std::function<std::string(const outer_struct&)> serialize_fn,
                  std::ofstream& results_file,
                  unsigned int seed
                ) {
    // Initialize random number generator
    std::random_device rd;
    std::mt19937 rng(seed);

    std::ofstream stream_file(library_name + ".txt");
    
    // Initialize struct
    outer_struct obj = {};
    
    // Variables for timing
    std::chrono::duration<double, std::milli> total_duration(0);
    std::chrono::duration<double, std::milli> total_flush_duration(0);
    int iterations = 0;
    int serializations = 0;
    
    std::cout << "Running benchmark for " << library_name << "...\n";
    
    auto start_time = std::chrono::steady_clock::now();
    auto end_time = start_time + std::chrono::seconds(5);
    int i =0;

    while (++i < 5000000) {
        // Update struct
        randomize_struct(obj, rng);
        iterations++;
        
        // Serialize every 20th update
        if (iterations % 20 == 0) {
            auto serialize_start = std::chrono::high_resolution_clock::now();
            auto s{ serialize_fn(obj) };
            auto serialize_end = std::chrono::high_resolution_clock::now();

            auto flush_start = std::chrono::high_resolution_clock::now();
            stream_file << s << std::endl;
            auto flush_end = std::chrono::high_resolution_clock::now();

            total_duration += serialize_end - serialize_start;
            total_flush_duration += flush_end - flush_start;
            serializations++;
        }
    }
    
    double avg_duration = total_duration.count() / serializations;    
    double avg_flush_duration = total_flush_duration.count() / serializations;    
    results_file << "Results for " << library_name << ":\n";
    results_file << "  Total updates: " << iterations << "\n";
    results_file << "  Total serializations: " << serializations << "\n";
    results_file << "  Average serialization time: " << avg_duration << " ms\n";
    results_file << "  Total serialization time: " << total_duration.count() << " ms\n\n";
    results_file << "  Average flush time: " << avg_flush_duration << " ms\n";
    results_file << "  Total flush time: " << total_flush_duration.count() << " ms\n\n";
}

int main() {
    std::ofstream results_file("benchmark_results.txt");

    unsigned int seed = 31454252;

    // Benchmark nlohmann::json
    run_benchmark("nlohmann::json", [](const outer_struct& obj) {
        nlohmann::json j = obj;
        return j.dump();
    }, results_file, seed);

    json_logger<outer_struct> logger{};
    run_benchmark("fjsonl", [&logger](const outer_struct& obj) {
        return logger.log(obj);
    }, results_file, seed);
    
    // Benchmark RapidJSON        
    rapidjson::StringBuffer buffer;

    run_benchmark("RapidJSON", [&buffer](const outer_struct& obj) {
        buffer.Clear();
        rapidjson::Writer<rapidjson::StringBuffer> writer(buffer);
        
        serialize_outer_struct_rapid(writer, obj);
        return buffer.GetString();
    }, results_file, seed);
    
    return 0;
}
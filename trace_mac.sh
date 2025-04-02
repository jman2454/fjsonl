rm -rf profile_results.trace
xcrun xctrace record --template "Time Profiler" --output profile_results.trace --launch -- ./benchmarks/json_benchmark
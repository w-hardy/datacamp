library(benchmarkme)

# Assign the variable ram to the amount of RAM on this machine
ram <- get_ram()
ram

# Assign the variable cpu to the cpu specs
cpu <- get_cpu()
cpu

# Run the io benchmark
res <- benchmark_io(runs = 5, size = 5)

# Plot the results
plot(res)

# run each benchmark 3 times
res <- benchmark_std(runs = 10)

# Plot the results
plot(res)

upload_results(res)

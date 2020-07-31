# Benchmark

This application uses [purescript-benchotron](https://github.com/hdgarrood/purescript-benchotron) for benchmarking the performance of various functions.

## Running the benchmarks
Run the command '`npm run benchmark`' in the repository folder. This will execute all the benchmarks and dump the output into the '`/tmp`' folder. You can also execute the individual benchmarks by building the application then running the command '`node benchmark/Run.js`'. This will start a CLI for picking which benchmark to run.

## Viewing the results
To view the benchmark results open [purescript-benchotron-svg-renderer](http://harry.garrood.me/purescript-benchotron-svg-renderer/) and open the results file from the '`/tmp`' folder. This will render the results as a `png` or `svg`.

## Creating a new benchmark
The first step to creating a new benchmark is to copy the definition ``
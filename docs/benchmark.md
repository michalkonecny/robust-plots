# Benchmark

This application uses [purescript-benchotron](https://github.com/hdgarrood/purescript-benchotron) for benchmarking the performance of various functions.

## Running the benchmarks
Run the command '`npm run benchmark`' in the repository folder. This will execute all the benchmarks and dump the output into the '`/tmp`' folder. You can also execute the individual benchmarks by building the application then running the command '`node benchmark/Run.js`'. This will start a CLI for picking which benchmark to run.

## Viewing the results
To view the benchmark results open [purescript-benchotron-svg-renderer](http://harry.garrood.me/purescript-benchotron-svg-renderer/) and open the results file from the '`/tmp`' folder. This will render the results as a `png` or `svg`.

## Creating a new benchmark
The first step to creating a new benchmark is to copy a definition `Benchmark` from `Benchotron.Core`. The parameters for the benchmark are given as record type and the attributes are as follows.

### `slug`
The name of the benchmark and the file name of the output `JSON` file.
### `title`
The text displays at the top of the input against time graph.
### `sizes`
The range of values that will be used as inputs to the benchmark functions.
### `sizeInterpretation`
This is the text label that will appear on the x axis of the output graph.
### `inputsPerSize`
The number of times that each input value will be run.
### `gen`
The function that converts the input size value into a set of input values.
### `functions`
The array of functions that will be benchmarked. These functions will receive the same inputs and be plotted against each other on the same axis. You construct these functions using the `benchFn` from `Benchotron.Core`. The first parameter for this function is the label for the function that will appear on the graph.

Not that only functions with a single variable between them should be plotted on the same benchmark. This is because a benchmark should only be used to assert the change in performance caused by that one aspect.
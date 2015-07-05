#!/usr/bin/env bash

set -e

# cheat to put Benchmark.Main in the pulp source path
rm -rf src/tmp
mkdir -p src/tmp
ln -s ../../benchmarks/Main.purs src/tmp/Benchmark.Main.purs
ln -s ../../benchmarks/Main.js src/tmp/Benchmark.Main.js

pulp build

mkdir -p tmp
NODE_PATH=./output browserify benchmarks/run.js --node -o tmp/benchmarks.js
node tmp/benchmarks.js

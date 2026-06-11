# JMH benchmarks for the Idris JVM backend

Numeric microbenchmarks (Sieve of Eratosthenes, matrix multiplication)
compiled with the Idris JVM backend and driven by
[JMH](https://github.com/openjdk/jmh). The primary purpose is measuring
**allocation rate** before and after compiler changes such as
monomorphization, via JMH's GC profiler (`gc.alloc.rate.norm`, bytes per
operation).

## Benchmarks

| Benchmark                  | Workload                                                                  |
|----------------------------|---------------------------------------------------------------------------|
| `BenchmarkMain.sievePrimes`| Functional Sieve of Eratosthenes over `List Int` up to 4096               |
| `BenchmarkMain.matMul`     | 50×50 `List (List Double)` matrix multiply via `map`/`zipWith`/`foldl`    |

Both workloads build their inputs inside the benchmark on purpose: list and
closure allocation is the thing being measured.

## Prerequisites

- JDK 21+
- Maven (or use the repo's `./mvnw`)
- An installed Idris JVM compiler (e.g. `~/bin/idris2-0.8.1-SNAPSHOT/exec/idris2`)

## Quick start

```shell
./run.sh mylabel /path/to/exec/idris2
```

This cleans previous Idris/Maven output, builds the benchmarks with the given
compiler (`IDRIS2_CG=jvm`), and runs JMH with `-prof gc`, writing
`results/mylabel.txt` (human-readable) and `results/mylabel.json`.

## Before/after allocation comparison

Build and install the compiler from the baseline branch and from the branch
under test (e.g. `main` vs `feature/monomorphization`), then run the suite
once with each and diff:

```shell
./run.sh baseline ~/bin/idris2-baseline/exec/idris2
./run.sh mono     ~/bin/idris2-0.8.1-SNAPSHOT/exec/idris2
./compare.sh results/baseline.json results/mono.json
```

The interesting numbers:

- `gc.alloc.rate.norm` — bytes allocated per benchmark op. This is the
  headline metric for monomorphization: fewer boxed values and erased-generic
  constructor cells means a lower B/op.
- `gc.alloc.rate` — MB/sec, a function of the above and throughput.
- The primary score (ops/s) — should improve or at least not regress.

`gc.alloc.rate.norm` comes from thread allocation counters, so it is exact
and stable even with short iterations.

## Where the allocations come from (JFR / async-profiler)

`-prof gc` tells you *how much* is allocated; to see *which call sites*
allocate, use one of:

JFR (built into the JDK, no extra install):

```shell
./run.sh mylabel /path/to/exec/idris2 -prof jfr
jfr print --events jdk.ObjectAllocationInNewTLAB <fork-dir>/profile.jfr | less
```

async-profiler (alloc mode, flame graph output):

```shell
./run.sh mylabel /path/to/exec/idris2 \
  -prof 'async:libPath=/path/to/libasyncProfiler.dylib;event=alloc;output=flamegraph'
```

## Manual runs

The shaded JAR is a regular JMH launcher, so all JMH options work:

```shell
mvn clean package -Didris2.executable=/path/to/exec/idris2
java -jar target/benchmark.jar -h                    # list options
java -jar target/benchmark.jar 'sievePrimes' -prof gc
```

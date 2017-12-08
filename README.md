Advent of Code 2017
===================

**Warning: Spoilers**

[Reflections and Benchmarks][RnB]
---------------------------------

[RnB]: https://github.com/mstksg/advent-of-code-2017/blob/master/reflections.md

I try to reflect on every day's puzzle, attempting to describe my thought
processes and how my solutions all work.  Benchmarks also included.

*   **[Day 1 Reflections][d1r]** *([code][d1c])* *([benchmarks][d1b])*
*   **[Day 2 Reflections][d2r]** *([code][d2c])* *([benchmarks][d2b])*
*   **[Day 3 Reflections][d3r]** *([code][d3c])* *([benchmarks][d3b])*
*   **[Day 4 Reflections][d4r]** *([code][d4c])* *([benchmarks][d4b])*
*   **[Day 5 Reflections][d5r]** *([code][d5c])* *([benchmarks][d5b])*
*   **[Day 6 Reflections][d6r]** *([code][d6c])* *([benchmarks][d6b])*
*   **[Day 7 Reflections][d7r]** *([code][d7c])* *([benchmarks][d7b])*
*   **[Day 8 Reflections][d8r]** *([code][d8c])* *([benchmarks][d8b])*

[d1r]: https://github.com/mstksg/advent-of-code-2017/blob/master/reflections.md#day-1
[d2r]: https://github.com/mstksg/advent-of-code-2017/blob/master/reflections.md#day-2
[d3r]: https://github.com/mstksg/advent-of-code-2017/blob/master/reflections.md#day-3
[d4r]: https://github.com/mstksg/advent-of-code-2017/blob/master/reflections.md#day-4
[d5r]: https://github.com/mstksg/advent-of-code-2017/blob/master/reflections.md#day-5
[d6r]: https://github.com/mstksg/advent-of-code-2017/blob/master/reflections.md#day-6
[d7r]: https://github.com/mstksg/advent-of-code-2017/blob/master/reflections.md#day-7
[d8r]: https://github.com/mstksg/advent-of-code-2017/blob/master/reflections.md#day-8

[d1c]: https://github.com/mstksg/advent-of-code-2017/blob/master/src/AOC2017/Day01.hs
[d2c]: https://github.com/mstksg/advent-of-code-2017/blob/master/src/AOC2017/Day02.hs
[d3c]: https://github.com/mstksg/advent-of-code-2017/blob/master/src/AOC2017/Day03.hs
[d4c]: https://github.com/mstksg/advent-of-code-2017/blob/master/src/AOC2017/Day04.hs
[d5c]: https://github.com/mstksg/advent-of-code-2017/blob/master/src/AOC2017/Day05.hs
[d6c]: https://github.com/mstksg/advent-of-code-2017/blob/master/src/AOC2017/Day06.hs
[d7c]: https://github.com/mstksg/advent-of-code-2017/blob/master/src/AOC2017/Day07.hs
[d8c]: https://github.com/mstksg/advent-of-code-2017/blob/master/src/AOC2017/Day08.hs

[d1b]: https://github.com/mstksg/advent-of-code-2017/blob/master/reflections.md#day-1-benchmarks
[d2b]: https://github.com/mstksg/advent-of-code-2017/blob/master/reflections.md#day-2-benchmarks
[d3b]: https://github.com/mstksg/advent-of-code-2017/blob/master/reflections.md#day-3-benchmarks
[d4b]: https://github.com/mstksg/advent-of-code-2017/blob/master/reflections.md#day-4-benchmarks
[d5b]: https://github.com/mstksg/advent-of-code-2017/blob/master/reflections.md#day-5-benchmarks
[d6b]: https://github.com/mstksg/advent-of-code-2017/blob/master/reflections.md#day-6-benchmarks
[d7b]: https://github.com/mstksg/advent-of-code-2017/blob/master/reflections.md#day-7-benchmarks
[d8b]: https://github.com/mstksg/advent-of-code-2017/blob/master/reflections.md#day-8-benchmarks

Executable
----------

Comes with test examples given in problems.

You can install using `stack`:

```bash
$ git clone https://github.com/mstksg/advent-of-code-2017
$ cd advent-of-code-2017
$ stack setup
$ stack install
```

The executable `aoc2017` includes a testing and benchmark suite

```
$ aoc2017 --help
aoc2017 - Advent of Code 2017 challenge runner

Usage: aoc2017 DAY [PART] [-t|--tests] [-b|--bench]
  Run challenges from Advent of Code 2017

Available options:
  DAY                      Day of challenge (1 - 25), or "all"
  PART                     Challenge part (a, b, c, etc.)
  -t,--tests               Run sample tests
  -b,--bench               Run benchmarks
  -h,--help                Show this help text

$ aoc2017 5 b
>> Day 05b
>> [✓] 27720699
```

Benchmarking is implemented using *criterion*

```
$ aoc2017 2 --bench
>> Day 02a
benchmarking...
time                 729.1 μs   (695.0 μs .. 784.2 μs)
                     0.967 R²   (0.926 R² .. 0.995 R²)
mean                 740.4 μs   (711.9 μs .. 783.6 μs)
std dev              116.8 μs   (70.44 μs .. 172.8 μs)
variance introduced by outliers: 89% (severely inflated)

>> Day 02b
benchmarking...
time                 782.4 μs   (761.3 μs .. 812.9 μs)
                     0.983 R²   (0.966 R² .. 0.998 R²)
mean                 786.7 μs   (764.1 μs .. 849.4 μs)
std dev              110.8 μs   (42.44 μs .. 228.5 μs)
variance introduced by outliers: 86% (severely inflated)
```

Test suites run the example problems given in the puzzle description, and
outputs are colorized in ANSI terminals.

```
$ aoc2017 1 --tests
[9] [!35732] $ aoc2017 1 --tests
>> Day 01a
[✓] (3)
[✓] (4)
[✓] (0)
[✓] (9)
[✓] Passed 4 out of 4 test(s)
[✓] 1097
>> Day 01b
[✓] (6)
[✓] (0)
[✓] (4)
[✓] (12)
[✓] (4)
[✓] Passed 5 out of 5 test(s)
[✓] 1188
```

This should only work if you're running `aoc2017` in the project directory.

**To run on actual inputs**, the executable expects inputs to be found in the
folder `data/XX.txt` in the directory you are running in.  That is, the input
for Day 7 will be expected at `data/07.txt`.

*aoc2017 will download missing input files*, but requires a session token.
This can be provided in `aoc2017-conf.yaml`:

```yaml
session:  [[ session token goes here ]]
```

To aid in regression testing, the executable will also automatically verify
that your current answers match the ones that have been previously submitted
and confirmed correct.  These are expected in `data/ans/XXpart.txt`.  That is,
the target output for Day 7 (Part 2, `b`) will be expected at
`data/ans/07b.txt`.


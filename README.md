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
*   **[Day 9 Reflections][d9r]** *([code][d9c])* *([benchmarks][d9b])* *([stream][d9s])*
*   **[Day 10 Reflections][d10r]** *([code][d10c])* *([benchmarks][d10b])* *([stream][d10s])*
*   **[Day 11 Reflections][d11r]** *([code][d11c])* *([benchmarks][d11b])*
*   **[Day 12 Reflections][d12r]** *([code][d12c])* *([benchmarks][d12b])*
*   **[Day 13 Reflections][d13r]** *([code][d13c])* *([benchmarks][d13b])*
*   **[Day 14 Reflections][d14r]** *([code][d14c])* *([benchmarks][d14b])*
*   **[Day 15 Reflections][d15r]** *([code][d15c])* *([benchmarks][d15b])*
*   **[Day 16 Reflections][d16r]** *([code][d16c])* *([benchmarks][d16b])*
*   **[Day 17 Reflections][d17r]** *([code][d17c])* *([benchmarks][d17b])*
*   **Day 18 Reflections** *([code][d18c])* *([benchmarks][d18b])*
*   **[Day 19 Reflections][d19r]** *([code][d19c])* *([benchmarks][d19b])*
*   **Day 20 Reflections** *([code][d20c])* *([benchmarks][d20b])*
*   **Day 21 Reflections** *([code][d21c])* *([benchmarks][d21b])*
*   **Day 22 Reflections** *([code][d22c])* *([benchmarks][d22b])*
*   **Day 23 Reflections** *([code][d23c])* *([benchmarks][d23b])*
*   **Day 24 Reflections** *([code][d24c])* *([benchmarks][d24b])*
*   **Day 25 Reflections** *([code][d25c])* *([benchmarks][d25b])*

[d1r]: https://github.com/mstksg/advent-of-code-2017/blob/master/reflections.md#day-1
[d2r]: https://github.com/mstksg/advent-of-code-2017/blob/master/reflections.md#day-2
[d3r]: https://github.com/mstksg/advent-of-code-2017/blob/master/reflections.md#day-3
[d4r]: https://github.com/mstksg/advent-of-code-2017/blob/master/reflections.md#day-4
[d5r]: https://github.com/mstksg/advent-of-code-2017/blob/master/reflections.md#day-5
[d6r]: https://github.com/mstksg/advent-of-code-2017/blob/master/reflections.md#day-6
[d7r]: https://github.com/mstksg/advent-of-code-2017/blob/master/reflections.md#day-7
[d8r]: https://github.com/mstksg/advent-of-code-2017/blob/master/reflections.md#day-8
[d9r]: https://github.com/mstksg/advent-of-code-2017/blob/master/reflections.md#day-9
[d10r]: https://github.com/mstksg/advent-of-code-2017/blob/master/reflections.md#day-10
[d11r]: https://github.com/mstksg/advent-of-code-2017/blob/master/reflections.md#day-11
[d12r]: https://github.com/mstksg/advent-of-code-2017/blob/master/reflections.md#day-12
[d13r]: https://github.com/mstksg/advent-of-code-2017/blob/master/reflections.md#day-13
[d14r]: https://github.com/mstksg/advent-of-code-2017/blob/master/reflections.md#day-14
[d15r]: https://github.com/mstksg/advent-of-code-2017/blob/master/reflections.md#day-15
[d16r]: https://github.com/mstksg/advent-of-code-2017/blob/master/reflections.md#day-16
[d17r]: https://github.com/mstksg/advent-of-code-2017/blob/master/reflections.md#day-17
[d18r]: https://github.com/mstksg/advent-of-code-2017/blob/master/reflections.md#day-18
[d19r]: https://github.com/mstksg/advent-of-code-2017/blob/master/reflections.md#day-19
[d20r]: https://github.com/mstksg/advent-of-code-2017/blob/master/reflections.md#day-20
[d21r]: https://github.com/mstksg/advent-of-code-2017/blob/master/reflections.md#day-21
[d22r]: https://github.com/mstksg/advent-of-code-2017/blob/master/reflections.md#day-22
[d23r]: https://github.com/mstksg/advent-of-code-2017/blob/master/reflections.md#day-23
[d24r]: https://github.com/mstksg/advent-of-code-2017/blob/master/reflections.md#day-24
[d25r]: https://github.com/mstksg/advent-of-code-2017/blob/master/reflections.md#day-25

[d1c]: https://github.com/mstksg/advent-of-code-2017/blob/master/src/AOC2017/Challenge/Day01.hs
[d2c]: https://github.com/mstksg/advent-of-code-2017/blob/master/src/AOC2017/Challenge/Day02.hs
[d3c]: https://github.com/mstksg/advent-of-code-2017/blob/master/src/AOC2017/Challenge/Day03.hs
[d4c]: https://github.com/mstksg/advent-of-code-2017/blob/master/src/AOC2017/Challenge/Day04.hs
[d5c]: https://github.com/mstksg/advent-of-code-2017/blob/master/src/AOC2017/Challenge/Day05.hs
[d6c]: https://github.com/mstksg/advent-of-code-2017/blob/master/src/AOC2017/Challenge/Day06.hs
[d7c]: https://github.com/mstksg/advent-of-code-2017/blob/master/src/AOC2017/Challenge/Day07.hs
[d8c]: https://github.com/mstksg/advent-of-code-2017/blob/master/src/AOC2017/Challenge/Day08.hs
[d9c]: https://github.com/mstksg/advent-of-code-2017/blob/master/src/AOC2017/Challenge/Day09.hs
[d10c]: https://github.com/mstksg/advent-of-code-2017/blob/master/src/AOC2017/Challenge/Day10.hs
[d11c]: https://github.com/mstksg/advent-of-code-2017/blob/master/src/AOC2017/Challenge/Day11.hs
[d12c]: https://github.com/mstksg/advent-of-code-2017/blob/master/src/AOC2017/Challenge/Day12.hs
[d13c]: https://github.com/mstksg/advent-of-code-2017/blob/master/src/AOC2017/Challenge/Day13.hs
[d14c]: https://github.com/mstksg/advent-of-code-2017/blob/master/src/AOC2017/Challenge/Day14.hs
[d15c]: https://github.com/mstksg/advent-of-code-2017/blob/master/src/AOC2017/Challenge/Day15.hs
[d16c]: https://github.com/mstksg/advent-of-code-2017/blob/master/src/AOC2017/Challenge/Day16.hs
[d17c]: https://github.com/mstksg/advent-of-code-2017/blob/master/src/AOC2017/Challenge/Day17.hs
[d18c]: https://github.com/mstksg/advent-of-code-2017/blob/master/src/AOC2017/Challenge/Day18.hs
[d19c]: https://github.com/mstksg/advent-of-code-2017/blob/master/src/AOC2017/Challenge/Day19.hs
[d20c]: https://github.com/mstksg/advent-of-code-2017/blob/master/src/AOC2017/Challenge/Day20.hs
[d21c]: https://github.com/mstksg/advent-of-code-2017/blob/master/src/AOC2017/Challenge/Day21.hs
[d22c]: https://github.com/mstksg/advent-of-code-2017/blob/master/src/AOC2017/Challenge/Day22.hs
[d23c]: https://github.com/mstksg/advent-of-code-2017/blob/master/src/AOC2017/Challenge/Day23.hs
[d24c]: https://github.com/mstksg/advent-of-code-2017/blob/master/src/AOC2017/Challenge/Day24.hs
[d25c]: https://github.com/mstksg/advent-of-code-2017/blob/master/src/AOC2017/Challenge/Day25.hs

[d1b]: https://github.com/mstksg/advent-of-code-2017/blob/master/reflections.md#day-1-benchmarks
[d2b]: https://github.com/mstksg/advent-of-code-2017/blob/master/reflections.md#day-2-benchmarks
[d3b]: https://github.com/mstksg/advent-of-code-2017/blob/master/reflections.md#day-3-benchmarks
[d4b]: https://github.com/mstksg/advent-of-code-2017/blob/master/reflections.md#day-4-benchmarks
[d5b]: https://github.com/mstksg/advent-of-code-2017/blob/master/reflections.md#day-5-benchmarks
[d6b]: https://github.com/mstksg/advent-of-code-2017/blob/master/reflections.md#day-6-benchmarks
[d7b]: https://github.com/mstksg/advent-of-code-2017/blob/master/reflections.md#day-7-benchmarks
[d8b]: https://github.com/mstksg/advent-of-code-2017/blob/master/reflections.md#day-8-benchmarks
[d9b]: https://github.com/mstksg/advent-of-code-2017/blob/master/reflections.md#day-9-benchmarks
[d10b]: https://github.com/mstksg/advent-of-code-2017/blob/master/reflections.md#day-10-benchmarks
[d11b]: https://github.com/mstksg/advent-of-code-2017/blob/master/reflections.md#day-11-benchmarks
[d12b]: https://github.com/mstksg/advent-of-code-2017/blob/master/reflections.md#day-12-benchmarks
[d13b]: https://github.com/mstksg/advent-of-code-2017/blob/master/reflections.md#day-13-benchmarks
[d14b]: https://github.com/mstksg/advent-of-code-2017/blob/master/reflections.md#day-14-benchmarks
[d15b]: https://github.com/mstksg/advent-of-code-2017/blob/master/reflections.md#day-15-benchmarks
[d16b]: https://github.com/mstksg/advent-of-code-2017/blob/master/reflections.md#day-16-benchmarks
[d17b]: https://github.com/mstksg/advent-of-code-2017/blob/master/reflections.md#day-17-benchmarks
[d18b]: https://github.com/mstksg/advent-of-code-2017/blob/master/reflections.md#day-18-benchmarks
[d19b]: https://github.com/mstksg/advent-of-code-2017/blob/master/reflections.md#day-19-benchmarks
[d20b]: https://github.com/mstksg/advent-of-code-2017/blob/master/reflections.md#day-20-benchmarks
[d21b]: https://github.com/mstksg/advent-of-code-2017/blob/master/reflections.md#day-21-benchmarks
[d22b]: https://github.com/mstksg/advent-of-code-2017/blob/master/reflections.md#day-22-benchmarks
[d23b]: https://github.com/mstksg/advent-of-code-2017/blob/master/reflections.md#day-23-benchmarks
[d24b]: https://github.com/mstksg/advent-of-code-2017/blob/master/reflections.md#day-24-benchmarks
[d25b]: https://github.com/mstksg/advent-of-code-2017/blob/master/reflections.md#day-25-benchmarks

[d9s]: https://www.twitch.tv/videos/207969022
[d10s]: https://www.twitch.tv/videos/208287550

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

You can "lock in" your current answers (telling the executable that those are
the correct answers) by passing in `--lock`.  This will lock in any final
puzzle solutions encountered as the verified official answers.  Later, if you
edit or modify your solutions, they will be checked on the locked-in answers.

These are store in `data/ans/XXpart.txt`.  That is, the target output for Day 7
(Part 2, `b`) will be expected at `data/ans/07b.txt`.  You can also manually
edit these files.


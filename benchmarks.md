# Benchmarks

## Running

Switch to directory steady_vector_bench/.
* Quick run: `mix run bench/bench.exs`
* Full run: `mix run bench/bench.exs full`

## Benchmarking Summary

Perf-wise, the sweet spot for `steady_vector` is scenarios when the vector needs to be built by repeatedly appending to the end **and** random-access (`get`/`set`  operations) are also used.

If *random-access* after building is not required, then building and reversing a `List` is more efficient.

If building speed is not important, but removal from the end happens often, then Erlang's `array` shows better performance.

`get`/`set` operations perform similar to `array`:
* `steady_vector:get/2` is slightly faster for *larger* collections compared to `:array`.
* `steady_vector:set/3` is slightly faster for *smaller* collections compared to `:array`.

`maps`, `gb_trees`, `dicts` and `orddicts` are added only for a baseline,
in a sense that if any of these was to outperform `steady_vector` then this library would not be needed.
This comparison is *not fair*, as all of them present much richer capabilities.

## Raw Benchmarking results for 1.0.0 (Erlang/OTP 19.3)

```none
Time per test: 10 sec
Using 2 inputs

#
# Build
#

##### With input 1'000 #####
Name                       ips        average  deviation         median         99th %
list                   41.01 K       24.38 μs    ±64.55%          24 μs          34 μs
steady_vector          12.90 K       77.51 μs     ±9.76%          76 μs         103 μs
PersistentVector        8.83 K      113.28 μs     ±7.34%         112 μs         146 μs
map                     7.82 K      127.81 μs    ±18.04%         124 μs         234 μs
array                   4.65 K      215.17 μs     ±9.13%         209 μs         281 μs
dict                    1.32 K      755.36 μs    ±10.98%         722 μs     1072.07 μs
gb_trees                0.70 K     1429.90 μs     ±7.80%        1398 μs        1802 μs

Comparison:
list                   41.01 K
steady_vector          12.90 K - 3.18x slower
PersistentVector        8.83 K - 4.65x slower
map                     7.82 K - 5.24x slower
array                   4.65 K - 8.82x slower
dict                    1.32 K - 30.98x slower
gb_trees                0.70 K - 58.64x slower

##### With input 1'000'000 #####
Name                       ips        average  deviation         median         99th %
list                     30.21       33.10 ms    ±10.60%       32.53 ms       46.95 ms
steady_vector             9.76      102.48 ms     ±2.21%      102.47 ms      108.95 ms
PersistentVector          7.20      138.96 ms     ±3.69%      138.86 ms      155.07 ms
array                     2.45      409.07 ms     ±1.59%      410.90 ms      421.59 ms
map                       1.13      882.59 ms     ±6.89%      877.84 ms      993.90 ms
gb_trees                  0.29     3417.94 ms     ±1.73%     3378.74 ms     3501.51 ms
dict                    0.0489    20467.57 ms     ±0.00%    20467.57 ms    20467.57 ms

Comparison:
list                     30.21
steady_vector             9.76 - 3.10x slower
PersistentVector          7.20 - 4.20x slower
array                     2.45 - 12.36x slower
map                       1.13 - 26.66x slower
gb_trees                  0.29 - 103.25x slower
dict                    0.0489 - 618.31x slower

#
# Shrink
#

##### With input 1'000 #####
Name                                   ips        average  deviation         median         99th %
array:resize                       15.71 K       63.64 μs   ±349.53%          61 μs          83 μs
steady_vector:remove_last           8.05 K      124.18 μs   ±254.89%         103 μs         435 μs
PersistentVector:remove_last        6.98 K      143.27 μs   ±253.54%         137 μs         189 μs
maps:remove                         6.90 K      144.84 μs   ±239.24%         117 μs         490 μs
gb_trees:delete                     6.30 K      158.81 μs   ±217.52%         153 μs         203 μs
gb_trees:take_largest               4.86 K      205.74 μs   ±187.64%         195 μs         290 μs

Comparison:
array:resize                       15.71 K
steady_vector:remove_last           8.05 K - 1.95x slower
PersistentVector:remove_last        6.98 K - 2.25x slower
maps:remove                         6.90 K - 2.28x slower
gb_trees:delete                     6.30 K - 2.50x slower
gb_trees:take_largest               4.86 K - 3.23x slower

##### With input 1'000'000 #####
Name                                   ips        average  deviation         median         99th %
array:resize                         14.70       68.05 ms    ±21.91%       66.72 ms      165.31 ms
steady_vector:remove_last             7.75      129.00 ms    ±22.23%      123.00 ms      277.58 ms
PersistentVector:remove_last          5.80      172.48 ms    ±22.59%      164.50 ms      340.46 ms
gb_trees:delete                       3.52      284.29 ms    ±22.76%      268.54 ms      626.65 ms
gb_trees:take_largest                 2.64      379.49 ms    ±33.08%      347.64 ms      980.82 ms
maps:remove                           1.55      646.73 ms     ±5.72%      638.01 ms      789.18 ms

Comparison:
array:resize                         14.70
steady_vector:remove_last             7.75 - 1.90x slower
PersistentVector:remove_last          5.80 - 2.53x slower
gb_trees:delete                       3.52 - 4.18x slower
gb_trees:take_largest                 2.64 - 5.58x slower
maps:remove                           1.55 - 9.50x slower

#
# Get
#

##### With input 1'000 #####
Name                           ips        average  deviation         median         99th %
maps:get                   13.79 K       72.54 μs     ±5.49%          72 μs          84 μs
gb_trees:get                7.02 K      142.36 μs   ±232.34%         138 μs         160 μs
steady_vector:get           6.36 K      157.37 μs     ±4.47%         155 μs         181 μs
array:get                   5.83 K      171.56 μs     ±2.07%         171 μs         181 μs
dict:fetch                  5.52 K      181.20 μs     ±4.65%         178 μs         210 μs
PersistentVector:get        4.53 K      220.75 μs    ±93.41%         217 μs         244 μs

Comparison:
maps:get                   13.79 K
gb_trees:get                7.02 K - 1.96x slower
steady_vector:get           6.36 K - 2.17x slower
array:get                   5.83 K - 2.36x slower
dict:fetch                  5.52 K - 2.50x slower
PersistentVector:get        4.53 K - 3.04x slower

##### With input 1'000'000 #####
Name                           ips        average  deviation         median         99th %
steady_vector:get             5.05      197.91 ms     ±0.60%      198.03 ms      200.52 ms
gb_trees:get                  4.65      214.94 ms     ±0.41%      214.88 ms      217.51 ms
PersistentVector:get          3.85      259.82 ms     ±1.15%      261.18 ms      264.06 ms
array:get                     3.48      287.49 ms     ±1.00%      287.03 ms      297.05 ms
maps:get                      2.79      358.44 ms     ±3.34%      359.64 ms      376.21 ms
dict:fetch                    1.61      621.53 ms     ±0.39%      621.62 ms      628.00 ms

Comparison:
steady_vector:get             5.05
gb_trees:get                  4.65 - 1.09x slower
PersistentVector:get          3.85 - 1.31x slower
array:get                     3.48 - 1.45x slower
maps:get                      2.79 - 1.81x slower
dict:fetch                    1.61 - 3.14x slower

#
# Set
#

##### With input 1'000 #####
Name                           ips        average  deviation         median         99th %
maps:put                    6.46 K      154.88 μs   ±225.92%         121 μs         539 μs
maps:update                 6.42 K      155.73 μs   ±226.34%         120 μs         542 μs
gb_trees:update             4.08 K      245.23 μs   ±178.76%         236 μs         306 μs
steady_vector:set           4.05 K      246.84 μs   ±188.37%         196 μs         655 μs
array:set                   3.45 K      289.78 μs   ±167.77%         251 μs         666 μs
PersistentVector:set        2.89 K      346.39 μs   ±166.68%         296 μs         756 μs

Comparison:
maps:put                    6.46 K
maps:update                 6.42 K - 1.01x slower
gb_trees:update             4.08 K - 1.58x slower
steady_vector:set           4.05 K - 1.59x slower
array:set                   3.45 K - 1.87x slower
PersistentVector:set        2.89 K - 2.24x slower

##### With input 1'000'000 #####
Name                           ips        average  deviation         median         99th %
steady_vector:set             2.38      420.56 ms     ±7.02%      414.42 ms      561.96 ms
array:set                     1.97      507.40 ms     ±6.79%      499.85 ms      656.68 ms
PersistentVector:set          1.92      521.05 ms     ±6.42%      513.43 ms      662.61 ms
gb_trees:update               1.72      581.67 ms    ±30.79%      528.13 ms     1279.81 ms
maps:put                      1.21      826.85 ms    ±11.13%      837.56 ms     1041.08 ms
maps:update                   1.13      887.50 ms    ±15.51%      847.86 ms     1343.26 ms

Comparison:
steady_vector:set             2.38
array:set                     1.97 - 1.21x slower
PersistentVector:set          1.92 - 1.24x slower
gb_trees:update               1.72 - 1.38x slower
maps:put                      1.21 - 1.97x slower
maps:update                   1.13 - 2.11x slower

#
# Fold
#

##### With input 1'000 #####
Name                                              ips        average  deviation         median         99th %
orddict:fold (pair)                           29.68 K       33.70 μs     ±7.16%          33 μs          41 μs
steady_vector:foldl (pair)                    24.14 K       41.42 μs   ±437.99%          39 μs          50 μs
dict:fold (pair)                              23.64 K       42.30 μs   ±498.51%          42 μs          47 μs
steady_vector:foldr (pair)                    22.42 K       44.60 μs    ±68.46%          41 μs         160 μs
array:foldl (pair)                            19.78 K       50.56 μs   ±456.41%          49 μs          58 μs
array:foldr (pair)                            19.69 K       50.80 μs   ±446.96%          50 μs          58 μs
maps:fold (pair)                              16.93 K       59.07 μs   ±364.29%          54 μs         180 μs
PersistentVector |> Enum.reduce (value)       13.03 K       76.73 μs   ±783.74%          73 μs          98 μs

Comparison:
orddict:fold (pair)                           29.68 K
steady_vector:foldl (pair)                    24.14 K - 1.23x slower
dict:fold (pair)                              23.64 K - 1.26x slower
steady_vector:foldr (pair)                    22.42 K - 1.32x slower
array:foldl (pair)                            19.78 K - 1.50x slower
array:foldr (pair)                            19.69 K - 1.51x slower
maps:fold (pair)                              16.93 K - 1.75x slower
PersistentVector |> Enum.reduce (value)       13.03 K - 2.28x slower

##### With input 1'000'000 #####
Name                                              ips        average  deviation         median         99th %
orddict:fold (pair)                             27.51       36.35 ms     ±0.83%       36.38 ms       37.06 ms
steady_vector:foldl (pair)                      23.88       41.87 ms    ±25.91%       41.17 ms       42.86 ms
dict:fold (pair)                                22.48       44.48 ms     ±2.00%       44.61 ms       46.55 ms
steady_vector:foldr (pair)                      22.14       45.16 ms    ±24.99%       44.40 ms       46.78 ms
array:foldl (pair)                              19.84       50.41 ms     ±0.51%       50.32 ms       50.96 ms
array:foldr (pair)                              19.81       50.49 ms     ±0.47%       50.41 ms       51.43 ms
maps:fold (pair)                                14.33       69.80 ms    ±27.44%       62.18 ms      187.25 ms
PersistentVector |> Enum.reduce (value)         13.18       75.86 ms    ±38.70%       73.01 ms      306.04 ms

Comparison:
orddict:fold (pair)                             27.51
steady_vector:foldl (pair)                      23.88 - 1.15x slower
dict:fold (pair)                                22.48 - 1.22x slower
steady_vector:foldr (pair)                      22.14 - 1.24x slower
array:foldl (pair)                              19.84 - 1.39x slower
array:foldr (pair)                              19.81 - 1.39x slower
maps:fold (pair)                                14.33 - 1.92x slower
PersistentVector |> Enum.reduce (value)         13.18 - 2.09x slower

#
# Map
#

##### With input 1'000 #####
Name                                           ips        average  deviation         median         99th %
orddict:map (pair)                         21.99 K       45.48 μs  ±1012.58%          43 μs          61 μs
dict:map (pair)                            20.35 K       49.14 μs   ±421.93%          48 μs          61 μs
steady_vector:map (pair)                   19.34 K       51.72 μs   ±770.00%          46 μs         168 μs
gb_trees:map (pair)                        18.77 K       53.28 μs   ±388.10%          52 μs          64 μs
array:map (pair)                           14.95 K       66.90 μs   ±344.23%          65 μs          84 μs
PersistentVector |> Enum.map (value)        9.73 K      102.74 μs   ±338.22%          99 μs         136 μs
maps:map (pair)                             7.97 K      125.55 μs   ±244.77%         114 μs         328 μs

Comparison:
orddict:map (pair)                         21.99 K
dict:map (pair)                            20.35 K - 1.08x slower
steady_vector:map (pair)                   19.34 K - 1.14x slower
gb_trees:map (pair)                        18.77 K - 1.17x slower
array:map (pair)                           14.95 K - 1.47x slower
PersistentVector |> Enum.map (value)        9.73 K - 2.26x slower
maps:map (pair)                             7.97 K - 2.76x slower

##### With input 1'000'000 #####
Name                                           ips        average  deviation         median         99th %
orddict:map (pair)                           15.73       63.58 ms    ±51.07%       59.70 ms      255.88 ms
steady_vector:map (pair)                     15.12       66.13 ms    ±56.57%       56.88 ms      303.32 ms
array:map (pair)                             14.12       70.83 ms    ±20.68%       69.07 ms      174.28 ms
gb_trees:map (pair)                          11.44       87.45 ms    ±48.99%       83.41 ms      480.40 ms
dict:map (pair)                              10.66       93.78 ms    ±44.90%       89.97 ms      496.65 ms
PersistentVector |> Enum.map (value)          9.18      108.94 ms    ±34.99%      104.95 ms      468.62 ms
maps:map (pair)                               3.92      255.45 ms    ±30.00%      234.39 ms      681.08 ms

Comparison:
orddict:map (pair)                           15.73
steady_vector:map (pair)                     15.12 - 1.04x slower
array:map (pair)                             14.12 - 1.11x slower
gb_trees:map (pair)                          11.44 - 1.38x slower
dict:map (pair)                              10.66 - 1.48x slower
PersistentVector |> Enum.map (value)          9.18 - 1.71x slower
maps:map (pair)                               3.92 - 4.02x slower

#
# Filter
#

##### With input 1'000 #####
Name                                              ips        average  deviation         median         99th %
orddict:filter (pair)                         23.42 K       42.70 μs   ±379.79%          42 μs          44 μs
dict:filter (pair)                            16.67 K       60.01 μs   ±317.38%          59 μs          63 μs
maps:filter (pair)                            10.52 K       95.09 μs   ±252.79%          89 μs         213 μs
steady_vector:filter (pair)                   10.51 K       95.13 μs   ±253.59%          87 μs         213 μs
PersistentVector |> Enum.filter (value)        8.27 K      120.87 μs   ±212.78%         117 μs         237 μs

Comparison:
orddict:filter (pair)                         23.42 K
dict:filter (pair)                            16.67 K - 1.41x slower
maps:filter (pair)                            10.52 K - 2.23x slower
steady_vector:filter (pair)                   10.51 K - 2.23x slower
PersistentVector |> Enum.filter (value)        8.27 K - 2.83x slower

##### With input 1'000'000 #####
Name                                              ips        average  deviation         median         99th %
orddict:filter (pair)                           20.19       49.52 ms    ±55.96%       46.88 ms       53.69 ms
dict:filter (pair)                              13.71       72.97 ms    ±18.81%       70.39 ms      173.33 ms
steady_vector:filter (pair)                      9.87      101.34 ms    ±14.31%      100.05 ms      243.48 ms
PersistentVector |> Enum.filter (value)          8.28      120.72 ms    ±13.96%      118.92 ms      271.87 ms
maps:filter (pair)                               7.55      132.45 ms    ±19.26%      132.33 ms      349.02 ms

Comparison:
orddict:filter (pair)                           20.19
dict:filter (pair)                              13.71 - 1.47x slower
steady_vector:filter (pair)                      9.87 - 2.05x slower
PersistentVector |> Enum.filter (value)          8.28 - 2.44x slower
maps:filter (pair)                               7.55 - 2.67x slower

#
# ConvertToList
#

##### With input 1'000 #####
Name                               ips        average  deviation         median         99th %
steady_vector:to_list         168.14 K        5.95 μs   ±333.61%        3.70 μs       18.93 μs
PersistentVector:to_list      149.02 K        6.71 μs  ±1222.81%           4 μs         127 μs
maps:to_list                   76.49 K       13.07 μs   ±727.75%          10 μs         134 μs
array:to_list                  75.12 K       13.31 μs   ±706.43%          13 μs          13 μs
gb_trees:to_list               57.50 K       17.39 μs   ±577.84%          17 μs          18 μs
dict:to_list                   30.67 K       32.61 μs   ±435.53%          32 μs          35 μs

Comparison:
steady_vector:to_list         168.14 K
PersistentVector:to_list      149.02 K - 1.13x slower
maps:to_list                   76.49 K - 2.20x slower
array:to_list                  75.12 K - 2.24x slower
gb_trees:to_list               57.50 K - 2.92x slower
dict:to_list                   30.67 K - 5.48x slower

##### With input 1'000'000 #####
Name                               ips        average  deviation         median         99th %
steady_vector:to_list           105.34        9.49 ms    ±59.87%        8.58 ms       14.12 ms
array:to_list                    69.46       14.40 ms    ±42.28%       13.21 ms       16.78 ms
PersistentVector:to_list         55.35       18.07 ms    ±95.73%       16.61 ms       29.03 ms
maps:to_list                     44.57       22.44 ms    ±50.90%       16.20 ms       34.46 ms
gb_trees:to_list                 33.17       30.15 ms    ±27.30%       29.73 ms       31.01 ms
dict:to_list                     20.84       47.99 ms    ±59.50%       45.70 ms       69.45 ms

Comparison:
steady_vector:to_list           105.34
array:to_list                    69.46 - 1.52x slower
PersistentVector:to_list         55.35 - 1.90x slower
maps:to_list                     44.57 - 2.36x slower
gb_trees:to_list                 33.17 - 3.18x slower
dict:to_list                     20.84 - 5.06x slower
```

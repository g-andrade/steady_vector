only =
  case System.argv do
    ["--only", x | rest] -> System.argv(rest); String.upcase(x)
    _ -> nil
  end

full  = Enum.member?(System.argv, "full")
quick = Enum.member?(System.argv, "quick")
quickest = Enum.member?(System.argv, "quickest")
parallel = Enum.member?(System.argv, "parallel")

defmodule Runner do
  @print_opts [benchmarking: false, fast_warning: false, configuration: false]

  @opts  [
    warmup: 2,
    time: (if quickest, do: 0.1, else: (if quick, do: 3, else: 10)),
    print: @print_opts,
    parallel: (if parallel, do: :erlang.system_info(:schedulers_online), else: 1),
    formatters: [
      &Benchee.Formatters.HTML.output/1,
      &Benchee.Formatters.Console.output/1
    ]
    #formatter_options: [html: [file: "html/bench.html"]],
  ]

  IO.puts :stderr, "Time per test: #{Keyword.get(@opts, :time)} sec"

  @only only

  def bench(name, tests, inputs \\ nil) do
    if should_run?(@only, name) do
      IO.puts ""
      IO.puts "#"
      IO.puts "# #{name}"
      IO.puts "#"

      opts =
        if inputs do
          @opts ++ [inputs: inputs]
        else
          @opts
        end

      report_path = :io_lib.format("html/~ts/~ts.html", [name, name])
      opts = opts ++ [formatter_options: [html: [file: report_path]]]
      Benchee.run(tests, opts)
    end
  end

  defp should_run?(nil, _), do: true
  defp should_run?(only, this), do: only == String.upcase(this)
end

inputs =
  cond do
    full ->
      %{
               "10" => 0 ..        10,
              "100" => 0 ..       100,
            "1'000" => 0 ..     1_000,
           "10'000" => 0 ..    10_000,
          "100'000" => 0 ..   100_000,
        "1'000'000" => 0 .. 1_000_000,
      }
    quickest ->
      %{
              "100" => 0 .. 100
      }
    true ->
      %{
            "1'000" => 0 ..     1_000,
        "1'000'000" => 0 .. 1_000_000,
      }
  end

IO.puts :stderr, "Using #{Enum.count(inputs)} inputs"

Runner.bench("Build",
  %{
    "steady_vector"    => fn range -> Enum.reduce(range, :steady_vector.new(), &:steady_vector.append/2) end,
    "PersistentVector" => fn range -> Enum.reduce(range, PersistentVector.new(), &PersistentVector.append(&2, &1)) end,
    "array"            => fn range -> Enum.reduce(range, :array.new(), &:array.set(&1, &1, &2)) end,
    "list"             => fn range -> Enum.reduce(range, [], &[&1 | &2]) |> :lists.reverse() end,
    "map"              => fn range -> Enum.reduce(range, %{}, &:maps.put(&1, &1, &2)) end,
    "gb_trees"         => fn range -> Enum.reduce(range, :gb_trees.empty(), &:gb_trees.insert(&1, &1, &2)) end,
    "dict"             => fn range -> Enum.reduce(range, :dict.new(), &:dict.store(&1, &1, &2)) end,
  },
  inputs)

data_inputs =
  inputs
  |> Enum.map(
      fn {text, range} ->
        vec = Enum.reduce(range, :steady_vector.new(), &:steady_vector.append/2)
        if vec |> :steady_vector.size != range.last+1, do: raise "steady_vector size didn't match"

        prv = Enum.reduce(range, PersistentVector.new(), &PersistentVector.append(&2, &1))
        if prv |> PersistentVector.count != range.last+1, do: raise "PersistentVector size didn't match"

        arr = Enum.reduce(range, :array.new(), &:array.set(&1, &1, &2))
        if arr |> :array.size != range.last+1, do: raise "array size didn't match"

        map = Enum.reduce(range, %{}, &:maps.put(&1, &1, &2))
        if map |> Enum.count != range.last+1, do: raise "map size didn't match"

        gbt = Enum.reduce(range, :gb_trees.empty(), &:gb_trees.insert(&1, &1, &2))
        if gbt |> :gb_trees.size != range.last+1, do: raise "gb_tree size didn't match"

        dic = Enum.reduce(range, :dict.new(), &:dict.store(&1, &1, &2))
        if dic |> :dict.size != range.last+1, do: raise "dict size didn't match"

        {text, %{range: range, vec: vec, prv: prv, arr: arr, map: map, gbt: gbt, dic: dic}}
      end)
  |> Enum.into(%{})

Runner.bench("Shrink",
  %{
    "steady_vector:remove_last"    => fn %{range: range, vec: vec} -> Enum.reduce(Enum.reverse(range), vec, fn _, vec -> vec |> :steady_vector.remove_last() end) end,
    "PersistentVector:remove_last" => fn %{range: range, prv: prv} -> Enum.reduce(Enum.reverse(range), prv, fn _, prv -> prv |> PersistentVector.remove_last() end) end,
    "array:resize"                 => fn %{range: range, arr: arr} -> Enum.reduce(Enum.reverse(range), arr, &:array.resize/2) end,
    "maps:remove"                  => fn %{range: range, map: map} -> Enum.reduce(Enum.reverse(range), map, &:maps.remove/2) end,
    "gb_trees:delete"              => fn %{range: range, gbt: gbt} -> Enum.reduce(Enum.reverse(range), gbt, &:gb_trees.delete/2) end,
    "gb_trees:take_largest"        => fn %{range: range, gbt: gbt} -> Enum.reduce(Enum.reverse(range), gbt, fn _, gbt -> {_key, _value, gbt} = :gb_trees.take_largest(gbt); gbt end) end,
    "dict:erase"                   => fn %{range: range, dic: dic} -> Enum.reduce(Enum.reverse(range), dic, &:dict.erase/2) end,
  },
  data_inputs)

Runner.bench("Get",
  %{
    "steady_vector:get"    => fn %{range: range, vec: vec} -> Enum.each(range, &:steady_vector.get(&1, vec)) end,
    "PersistentVector:get" => fn %{range: range, prv: prv} -> Enum.each(range, &PersistentVector.get(prv, &1)) end,
    "array:get"            => fn %{range: range, arr: arr} -> Enum.each(range, &:array.get(&1, arr)) end,
    "maps:get"             => fn %{range: range, map: map} -> Enum.each(range, &:maps.get(&1, map)) end,
    "gb_trees:get"         => fn %{range: range, gbt: gbt} -> Enum.each(range, &:gb_trees.get(&1, gbt)) end,
    "dict:fetch"           => fn %{range: range, dic: dic} -> Enum.each(range, &:dict.fetch(&1, dic)) end,
  },
  data_inputs)

Runner.bench("Set",
  %{
    "steady_vector:set"    => fn %{range: range, vec: vec} -> Enum.reduce(range, vec, &:steady_vector.set(&1, &1 + 1, &2)) end,
    "PersistentVector:set" => fn %{range: range, prv: prv} -> Enum.reduce(range, prv, &PersistentVector.set(&2, &1, &1 + 1)) end,
    "array:set"            => fn %{range: range, arr: arr} -> Enum.reduce(range, arr, &:array.set(&1, &1 + 1, &2)) end,
    "maps:put"             => fn %{range: range, map: map} -> Enum.reduce(range, map, &:maps.put(&1, &1 + 1, &2)) end,
    "maps:update"          => fn %{range: range, map: map} -> Enum.reduce(range, map, &:maps.update(&1, &1 + 1, &2)) end,
    "gb_trees:update"      => fn %{range: range, gbt: gbt} -> Enum.reduce(range, gbt, &:gb_trees.update(&1, &1 + 1, &2)) end,
    "dict:store"           => fn %{range: range, dic: dic} -> Enum.reduce(range, dic, &:dict.store(&1, &1 + 1, &2)) end,
  },
  data_inputs)

valuefold_fun = fn (value, acc) -> rem(acc + value, 42) end
pairfold_fun = fn (_index, value, acc) -> rem(acc + value, 42) end
Runner.bench("Fold",
  %{
    "steady_vector:foldl (value)"             => fn %{vec: vec} -> :steady_vector.foldl(valuefold_fun, 0, vec) end,
    "steady_vector:foldr (value)"             => fn %{vec: vec} -> :steady_vector.foldr(valuefold_fun, 0, vec) end,
    "steady_vector:foldl (pair)"              => fn %{vec: vec} -> :steady_vector.foldl(pairfold_fun, 0, vec) end,
    "steady_vector:foldr (pair)"              => fn %{vec: vec} -> :steady_vector.foldr(pairfold_fun, 0, vec) end,
    "PersistentVector |> Enum.reduce (value)" => fn %{prv: prv} -> Enum.reduce(prv, 0, valuefold_fun) end,
    "array:foldl (pair)"                      => fn %{arr: arr} -> :array.foldl(pairfold_fun, 0, arr) end,
    "array:foldr (pair)"                      => fn %{arr: arr} -> :array.foldr(pairfold_fun, 0, arr) end,
    "maps:fold (pair)"                        => fn %{map: map} -> :maps.fold(pairfold_fun, 0, map) end,
    "dict:fold (pair)"                        => fn %{dic: dic} -> :dict.fold(pairfold_fun, 0, dic) end,
  },
  data_inputs)

valuemap_fun = fn (value) -> value * 2 end
pairmap_fun = fn (_index, value) -> value * 2 end
Runner.bench("Map",
  %{
    "steady_vector:map (value)"            => fn %{vec: vec} -> :steady_vector.map(valuemap_fun, vec) end,
    "steady_vector:map (pair)"             => fn %{vec: vec} -> :steady_vector.map(pairmap_fun, vec) end,
    "PersistentVector |> Enum.map (value)" => fn %{prv: prv} -> Enum.map(prv, valuemap_fun) end,
    "array:map (pair)"                     => fn %{arr: arr} -> :array.map(pairmap_fun, arr) end,
    "maps:map (pair)"                      => fn %{map: map} -> :maps.map(pairmap_fun, map) end,
    "gb_trees:map (pair)"                  => fn %{gbt: gbt} -> :gb_trees.map(pairmap_fun, gbt) end,
    "dict:map (pair)"                      => fn %{dic: dic} -> :dict.map(pairmap_fun, dic) end,
  },
  data_inputs)

Runner.bench("ConvertToList",
  %{
  "steady_vector:to_list"    => fn %{vec: vec} -> vec |> :steady_vector.to_list() end,
  "PersistentVector:to_list" => fn %{prv: prv} -> prv |> PersistentVector.to_list() end,
  "array:to_list"            => fn %{arr: arr} -> arr |> :array.to_list() end,
  "maps:to_list"             => fn %{map: map} -> map |> :maps.to_list() end,
  "gb_trees:to_list"         => fn %{gbt: gbt} -> gbt |> :gb_trees.to_list() end,
  "dict:to_list"             => fn %{dic: dic} -> dic |> :dict.to_list() end,
  },
  data_inputs)

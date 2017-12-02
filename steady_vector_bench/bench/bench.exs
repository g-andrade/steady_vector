only =
  case System.argv do
    ["--only", x | rest] -> System.argv(rest); String.upcase(x)
    _ -> nil
  end

full  = Enum.member?(System.argv, "full")
quick = Enum.member?(System.argv, "quick")
parallel = Enum.member?(System.argv, "parallel")

defmodule Runner do
  @print_opts [benchmarking: false, fast_warning: false, configuration: false]

  @opts  [
    warmup: 2,
    time: (if quick, do: 3, else: 10),
    print: @print_opts,
    parallel: (if parallel, do: :erlang.system_info(:schedulers_online), else: 1)
    # formatters: [
    #   &Benchee.Formatters.HTML.output/1,
    #   &Benchee.Formatters.Console.output/1
    # ],
    # formatter_options: [html: [file: "html/bench.html"]],
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

      Benchee.run(tests, opts)
    end
  end

  defp should_run?(nil, _), do: true
  defp should_run?(only, this), do: only == String.upcase(this)
end

inputs =
  if full do
    %{
      "       10" => 0 ..        10,
      "      100" => 0 ..       100,
      "    1'000" => 0 ..     1_000,
      "   10'000" => 0 ..    10_000,
      "  100'000" => 0 ..   100_000,
      "1'000'000" => 0 .. 1_000_000,
    }
  else
    %{
      "    1'000" => 0 ..     1_000,
      "1'000'000" => 0 .. 1_000_000,
    }
  end

IO.puts :stderr, "Using #{Enum.count(inputs)} inputs"

Runner.bench("Build",
  %{
  "steady_vector build" => fn range -> Enum.reduce(range, :steady_vector.new(), &:steady_vector.append(&1, &2)) end,
  "array         build" => fn range -> Enum.reduce(range, :array.new(), &:array.set(&1, &1, &2)) end,
  "list          build" => fn range -> Enum.reduce(range, [], &[&1 | &2]) |> :lists.reverse() end,
  "map           build" => fn range -> Enum.reduce(range, %{}, &:maps.put(&1, &1, &2)) end,
  },
  inputs)

data_inputs =
  inputs
  |> Enum.map(
      fn {text, range} ->
        vec = Enum.reduce(range, :steady_vector.new(), &:steady_vector.append(&1, &2))
        if vec |> :steady_vector.size != range.last+1, do: raise "steady_vector size didn't match"

        arr = Enum.reduce(range, :array.new(), &:array.set(&1, &1, &2))
        if arr |> :array.size != range.last+1, do: raise "array size didn't match"

        map = Enum.reduce(range, %{}, &:maps.put(&1, &1, &2))
        if map |> Enum.count != range.last+1, do: raise "map size didn't match"

        {text, %{range: range, vec: vec, arr: arr, map: map}}
      end)
  |> Enum.into(%{})

Runner.bench("Shrink",
  %{
  "steady_vector remove_last" => fn %{range: range, vec: vec} -> Enum.reduce(range, vec, fn _, vec -> vec |> :steady_vector.remove_last() end) end,
  "array         resize     " => fn %{range: range, arr: arr} -> Enum.reduce(range, arr, fn _, arr -> :array.resize(:array.size(arr) - 1, arr) end) end,
  "map           remove     " => fn %{range: range, map: map} -> Enum.reduce(range, map, fn _, map -> :maps.remove(Enum.count(map) - 1, map) end) end,
  },
  data_inputs)

Runner.bench("Get",
  %{
  "steady_vector get" => fn %{range: range, vec: vec} -> Enum.each(range, &:steady_vector.get(&1, vec)) end,
  "array         get" => fn %{range: range, arr: arr} -> Enum.each(range, &:array.get(&1, arr)) end,
  "map           get" => fn %{range: range, map: map} -> Enum.each(range, &:maps.get(&1, map)) end,
  },
  data_inputs)

Runner.bench("Set",
  %{
  "steady_vector set" => fn %{range: range, vec: vec} -> Enum.reduce(range, vec, &:steady_vector.set(&1, &1 + 1, &2)) end,
  "array         set" => fn %{range: range, arr: arr} -> Enum.reduce(range, arr, &:array.set(&1, &1 + 1, &2)) end,
  "map           put" => fn %{range: range, map: map} -> Enum.reduce(range, map, &:maps.put(&1, &1 + 1, &2)) end,
  },
  data_inputs)

valuefold_fun = fn (value, acc) -> rem(acc + value, 42) end
pairfold_fun = fn (_index, value, acc) -> rem(acc + value, 42) end
Runner.bench("Fold",
  %{
  "steady_vector foldl (value)" => fn %{vec: vec} -> :steady_vector.foldl(valuefold_fun, 0, vec) end,
  "steady_vector foldr (value)" => fn %{vec: vec} -> :steady_vector.foldr(valuefold_fun, 0, vec) end,
  "steady_vector foldl (pair)"  => fn %{vec: vec} -> :steady_vector.foldl(pairfold_fun, 0, vec) end,
  "steady_vector foldr (pair)"  => fn %{vec: vec} -> :steady_vector.foldr(pairfold_fun, 0, vec) end,
  "array         foldl (pair)"  => fn %{arr: arr} -> :array.foldl(pairfold_fun, 0, arr) end,
  "array         foldr (pair)"  => fn %{arr: arr} -> :array.foldr(pairfold_fun, 0, arr) end,
  "map           fold  (pair)"  => fn %{map: map} -> :maps.fold(pairfold_fun, 0, map) end
  },
  data_inputs)

valuemap_fun = fn (value) -> value * 2 end
pairmap_fun = fn (_index, value) -> value * 2 end
Runner.bench("Map",
  %{
  "steady_vector map (value)" => fn %{vec: vec} -> :steady_vector.map(valuemap_fun, vec) end,
  "steady_vector map (pair)"  => fn %{vec: vec} -> :steady_vector.map(pairmap_fun, vec) end,
  "array         map (pair)"  => fn %{arr: arr} -> :array.map(pairmap_fun, arr) end,
  "map           map (pair)"  => fn %{map: map} -> :maps.map(pairmap_fun, map) end
  },
  data_inputs)

Runner.bench("Convert To List",
  %{
  "steady_vector to_list" => fn %{vec: vec} -> vec |> :steady_vector.to_list() end,
  "array         to_list" => fn %{arr: arr} -> arr |> :array.to_list() end,
  "map           to_list" => fn %{map: map} -> map |> :maps.to_list() end,
  },
  data_inputs)

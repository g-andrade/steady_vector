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

  #Runner.bench("Enumerate",
  #  %{
  #  "Vector Enumerate" => fn %{vec: vec} -> Enum.each(vec, &(&1)) end,
  #  "Map    Enumerate" => fn %{map: map} -> Enum.each(map, &(&1)) end,
  #  },
  #  data_inputs)

Runner.bench("Convert To List",
  %{
  "steady_vector to_list" => fn %{vec: vec} -> vec |> :steady_vector.to_list() end,
  "array         to_list" => fn %{arr: arr} -> arr |> :array.to_list() end,
  "map           to_list" => fn %{map: map} -> map |> :maps.to_list() end,
  },
  data_inputs)

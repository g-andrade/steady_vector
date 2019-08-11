

# steady_vector #

[![Hex pm](http://img.shields.io/hexpm/v/steady_vector.svg?style=flat)](https://hex.pm/packages/steady_vector)
[![Travis CI Build Status](https://travis-ci.org/g-andrade/steady_vector.png?branch=master)](https://travis-ci.org/g-andrade/steady_vector)
[![Circle CI Build Status](https://circleci.com/gh/g-andrade/steady_vector/tree/master.svg?style=svg)](https://circleci.com/gh/g-andrade/steady_vector/tree/master)


### <a name="steady_vector_-_Persistent_Vector_for_Erlang_and_Elixir">steady_vector - Persistent Vector for Erlang and Elixir</a> ###


#### <a name="Description">Description</a> ####

`steady_vector` is an array-like collection of values optimized for tail growth and shrinkage. It's heavily based on Dmitry Kakurin's [PersistentVector](https://github.com/Dimagog/persistent_vector) implementation for Elixir and, other than for some idiomatic changes and a few new functions, `steady_vector`'s interface is conceptually very similar.

`steady_vector` optimizes the following operations:
* Get element count
* Lookup element by index (0-based)
* Update element by index (0-based)
* Adding new element to the end
* Removing element from the end
* Enumeration
* Mapping
* Folding (left and right)

Get count operation is `O(1)`, most others are `O(log32(N))`.

`steady_vector` is implemented as a tree with 32-way branching at each level and uses *structural sharing* for updates.
All ideas are borrowed directly from `PersistentVector`, which in turn borrowed them from [Clojure](http://hypirion.com/musings/understanding-persistent-vector-pt-1).


#### <a name="Installation_(Erlang)">Installation (Erlang)</a> ####

Add `steady_vector` to your list of dependencies in `rebar.config`:

```erlang

{deps,
 [{steady_vector, "1.0.1"}
 ]}.

```

And then run `rebar3 compile`


#### <a name="Installation_(Elixir)">Installation (Elixir)</a> ####

Add `steady_vector` to your list of dependencies in `mix.exs`:

```elixir

def deps do
[
  {:steady_vector, "1.0.1"}
]
end

```

And then run `mix deps.get`


#### <a name="Requirements">Requirements</a> ####

The library has been tested on Erlang/OTP versions 17.5, 18.3, 19.{0..3}, and 20.{0..1}. The supported build tool is `rebar3`.


#### <a name="More_info">More info</a> ####

See [benchmarks](https://github.com/g-andrade/steady_vector/blob/master/benchmarks.md).

See API reference indexed below.


## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="steady_vector.md" class="module">steady_vector</a></td></tr></table>


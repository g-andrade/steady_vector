# steady_vector

[![Hex version](https://img.shields.io/hexpm/v/steady_vector.svg?style=flat)](https://hex.pm/packages/steady_vector)
[![CI status](https://github.com/g-andrade/steady_vector/actions/workflows/ci.yml/badge.svg)](https://github.com/g-andrade/steady_vector/actions/workflows/ci.yml)
[![Supported Erlang/OTP versions](https://img.shields.io/badge/Supported%20Erlang%2FOTP-24%20to%2029-blue)](https://www.erlang.org)
[![API reference](https://img.shields.io/badge/hex-docs-lightgreen.svg)](https://hexdocs.pm/steady_vector/)

`steady_vector` is a persistent vector for Erlang and Elixir: an array-like
collection of values optimized for tail growth and shrinkage. It's heavily based
on Dmitry Kakurin's
[PersistentVector](https://github.com/Dimagog/persistent_vector) implementation
for Elixir and, other than some idiomatic changes and a few new functions, its
interface is conceptually very similar.

`steady_vector` optimizes the following operations:

- getting the element count
- looking up an element by its 0-based index
- updating an element by its 0-based index
- appending a new element to the end
- removing the last element
- enumeration
- mapping
- folding (left and right)

Getting the element count is `O(1)`; most other operations are `O(log32(N))`.

It is implemented as a tree with 32-way branching at each level and uses
*structural sharing* for updates. All ideas are borrowed directly from
`PersistentVector`, which in turn borrowed them from
[Clojure](https://hypirion.com/musings/understanding-persistent-vector-pt-1).

## Installation

### Erlang

Add `steady_vector` to your list of dependencies in `rebar.config`:

``` erlang
{deps, [
    {steady_vector, "~> 1.1"}
]}.
```

And then run `rebar3 compile`.

### Elixir

Add `steady_vector` to your list of dependencies in `mix.exs`:

``` elixir
def deps do
    [
        {:steady_vector, "~> 1.1"}
    ]
end
```

And then run `mix deps.get`.

## Documentation

The API reference is available on [HexDocs](https://hexdocs.pm/steady_vector/).

## Requirements

`steady_vector` is tested against Erlang/OTP 24 to 29. The supported build tool
is `rebar3`.

## License

`steady_vector` is licensed under the MIT license. See the [LICENSE](LICENSE.md)
file for details.

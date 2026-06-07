# steady_vector

Erlang/OTP (and Elixir) library providing a **persistent vector**: an array-like
collection of values optimized for growth and shrinkage at the tail. It is a
single-module, dependency-free pure data structure, ported from Dmitry Kakurin's
Elixir [PersistentVector](https://github.com/Dimagog/persistent_vector) and
designed after [Clojure's persistent vectors](https://hypirion.com/musings/understanding-persistent-vector-pt-1).

## Build, test, check

```bash
make compile         # compile
make test            # eunit + CT (+ coverage)
make check           # check-fast + check-slow
make check-fast      # format check (erlfmt) + xref + dead-code (hank) + lint (elvis)
make check-slow      # dialyzer
make format          # auto-format source with erlfmt
make eunit           # unit tests only (currently none; see Tests)
make ct              # common test suite + coverage
make dialyzer        # type analysis
make doc             # build ExDoc HTML docs (OTP 27+; see Docs)
make shell           # interactive REPL with the app started
make benchmark       # run the Elixir benchmark project (steady_vector_bench/)
```

All checks run sequentially (`.NOTPARALLEL`). CI runs `make check-fast`,
`make test`, and `make check-slow` on OTP 24–29, Linux only.

## Compiler flags

Always on: `debug_info`, `warn_export_vars`, `warn_missing_spec`,
`warn_unused_import`, `warnings_as_errors`. Every exported function must have a
`-spec`. The `test` profile relaxes `warn_missing_spec` and `warnings_as_errors`.
The `E48` macro is platform-defined on OTP 27+ to enable EEP-48 doc attributes.

## Architecture

There is no process tree — everything lives in one module, `steady_vector`.

The vector is a tree with **32-way branching** at each level plus a separate
**tail** holding the most recently appended block, giving `O(1)` count and
`O(log32 N)` indexing/append/update. Updates use **structural sharing** (only the
path from root to the changed leaf is copied).

The record is:

```erlang
-record(steady_vector, {count, shift, root, tail}).
```

- `count` — number of elements (`size/1` returns it directly).
- `shift` — current tree height, in bits; the branching factor is `1 bsl ?shift`.
- `root` — the tree of full leaf blocks.
- `tail` — the trailing, not-yet-pushed-down block (kept for fast append).

`?shift` is **5** in normal builds (32-way) but **2** under the `test` profile
(4-way), so the suite exercises multiple tree levels without building huge
vectors. See `?block_size`/`?mask` derived from it.

### Public API

| Function | Role |
|---|---|
| `new/0`, `from_list/1` | construct (empty / from a list) |
| `append/2`, `remove_last/1` | grow/shrink at the tail |
| `get/2`, `get/3`, `find/2`, `last/1`, `last/2` | index/last lookup (raising, defaulting, or `{ok,_}`/`error`) |
| `set/3` | update by index (appends when `Index =:= size`) |
| `size/1`, `is_empty/1`, `is_steady_vector/1` | introspection |
| `to_list/1` | convert to a list |
| `foldl/3`, `foldr/3`, `foreach/2`, `map/2`, `filter/2` | enumeration/transformation (all index-aware) |

Invalid vectors raise `{badvec, V}`; bad indices/arguments raise `badarg`;
operations on an empty vector raise `emptyvec`.

## Code conventions

- Single module: `src/steady_vector.erl`. The `#steady_vector{}` record and all
  macros are inline (no header — internals are not exposed to consumers).
- `-export` and `-ignore_xref` are each declared as a **single consolidated
  block** (in `f/n` form), not interleaved per function, so `erlfmt` keeps them
  tidy. Every public function is in `-ignore_xref` because a library's API is
  "unused" from xref's point of view.
- Macros are intentionally **lowercase**: guard-style helpers (`?is_vector`,
  `?is_index`, …), constants (`?shift`, `?block_size`, `?mask`) and error helpers
  (`?arg_error`, `?vec_error`, `?empty_vec_error`). `macro_naming_convention` is
  relaxed for `src/` in `elvis.config` to allow this.
- Prefer `case` over `if`; the module uses `case Bool of true -> …; _ -> … end`.
- Code is formatted with `erlfmt`; run `make format` before committing. The pure
  reformat commit is listed in `.git-blame-ignore-revs`.
- Other elvis/hank exceptions are documented inline in `elvis.config`
  (`dont_repeat_yourself` is disabled for `test/`).

## Documentation

Docs are **EEP-48 native** and rendered by **ExDoc**, not legacy edoc.

- Public docs are `-moduledoc` / `-doc` attributes guarded by `-ifdef(E48).`
  (OTP 27+). Each function block is laid out as
  `-ifdef(E48). -doc """…""". -endif.` → blank → `-spec … when` → blank →
  function, with a lone `%%%` line separating consecutive blocks (the deigma
  pattern). `@spec`/`@doc`/`@see` edoc tags are **not** used.
- `make doc` runs `rebar3 edoc` (which, via the top-level `edoc_opts` with
  `{doclet, edoc_doclet_chunks}` + `{preprocess, true}`, emits EEP-48 chunks into
  `_build/docs/lib/steady_vector/ebin`) and then the pinned `ex_doc` escript over
  that ebin, configured by `ex_doc.config`. Do **not** use a `docs` rebar3
  profile or `rebar3 as docs` — the top-level `edoc_opts` is enough.
- `README.md` is the docs' main page; `CHANGELOG.md` and `LICENSE.md` are extras.
- Internal helper functions are not exported, so they never appear in the docs —
  no `-doc false` is needed.

## Tests

The suite is Common Test: `test/steady_vector_SUITE.erl`. `all/0` auto-discovers
every exported `*_test/1` function. The suite drives only the public API (plus
the EUnit assert macros from `stdlib/include/assert.hrl`).

The `test` profile defines `TEST`, so `steady_vector` compiles with the shallow
branching factor (`?shift = 2`) and the cases exercise real multi-level trees.
`make eunit` currently runs no tests (there are no in-module EUnit cases); it is
kept in `make test` for parity with the house style.

## OTP version support

Supported on **OTP 24–29** (`rebar.config` declares a low `minimum_otp_vsn` with
a "but only 24+ is supported" note). `rebar.config.script` drops dev plugins that
don't work on older releases: erlfmt + hank + elvis on OTP ≤ 25, and erlfmt alone
on OTP ≤ 26 (its tooling chokes on `-doc` triple-quoted strings there).

## Dependencies

None at runtime. Dev plugins: `erlfmt` (formatter), `rebar3_hank` (dead code),
`rebar3_lint` (Elvis), `rebar3_hex` (publishing).

## Releasing

`make publish` runs `rebar3 hex publish`. Versioning follows SemVer; the
changelog is `CHANGELOG.md` (Keep a Changelog format). The application version is
`{vsn, "git"}`, resolved by rebar3 from the git tags.

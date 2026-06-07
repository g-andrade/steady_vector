%% Copyright (c) 2017 Dmitry Kakurin
%% Copyright (c) 2017 Guilherme Andrade <steady_vector@gandrade.net>
%%
%% Permission is hereby granted, free of charge, to any person obtaining a
%% copy  of this software and associated documentation files (the "Software"),
%% to deal in the Software without restriction, including without limitation
%% the rights to use, copy, modify, merge, publish, distribute, sublicense,
%% and/or sell copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
%% DEALINGS IN THE SOFTWARE.

-module(steady_vector).

-compile({no_auto_import,[{size,1}]}).
-compile(inline_list_funcs).

-ifdef(E48).
-moduledoc """
A persistent vector: an array-like collection of values optimized for growth
and shrinkage at the tail.

`steady_vector` optimizes the following operations:

- getting the element count (`size/1`) — `O(1)`;
- looking up an element by its 0-based index (`get/2`, `get/3`, `find/2`);
- updating an element by its 0-based index (`set/3`);
- appending a new element to the end (`append/2`);
- removing the last element (`remove_last/1`);
- enumeration (`foldl/3`, `foldr/3`, `foreach/2`);
- mapping (`map/2`) and filtering (`filter/2`).

Every operation other than `size/1` is `O(log32(N))`.

It is implemented as a tree with 32-way branching at each level and uses
structural sharing for updates, following the same design as
[Clojure's persistent vectors](https://hypirion.com/musings/understanding-persistent-vector-pt-1).
""".
-endif.

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export(
    [
        append/2,
        get/2,
        get/3,
        filter/2,
        find/2,
        foldl/3,
        foldr/3,
        foreach/2,
        from_list/1,
        is_empty/1,
        is_steady_vector/1,
        last/1,
        last/2,
        map/2,
        new/0,
        remove_last/1,
        set/3,
        size/1,
        to_list/1
    ]
).

-ignore_xref(
    [
        append/2,
        get/2,
        get/3,
        filter/2,
        find/2,
        foldl/3,
        foldr/3,
        foreach/2,
        from_list/1,
        is_empty/1,
        is_steady_vector/1,
        last/1,
        last/2,
        map/2,
        new/0,
        remove_last/1,
        set/3,
        size/1,
        to_list/1
    ]
).

%% ------------------------------------------------------------------
%% Macro Definitions
%% ------------------------------------------------------------------

-ifdef(TEST).
-define(shift, 2).
-else.
-define(shift, 5).
-endif.

-define(block_size, (1 bsl ?shift)).
-define(mask, (?block_size - 1)).

-define(is_index(I), (is_integer((I)) andalso (I) >= 0)).
-define(is_existing_index(I, V), (?is_index((I)) andalso (I) < (V)#steady_vector.count)).
-define(is_next_index(I, V), ((I) =:= (V)#steady_vector.count)).
-define(is_vector(V), (is_record(V, steady_vector))).

-define(arg_error, (error(badarg))).
-define(vec_error(Vector), (error({badvec,Vector}))).
-define(empty_vec_error, (error(emptyvec))).

%% ------------------------------------------------------------------
%% Record and Type Definitions
%% ------------------------------------------------------------------

-type shift() :: pos_integer().

-ifdef(E48).
-doc "A 0-based index into a vector.".
-endif.
-type index() :: non_neg_integer().
-export_type([index/0]).

-record(steady_vector, {
          count = 0 :: index(),
          shift = ?shift :: shift(),
          root = {} :: tuple(),
          tail = {} :: tuple()
         }).

-ifdef(E48).
-doc "A persistent vector, as returned by `new/0`.".
-endif.
-opaque t() :: #steady_vector{}.
-export_type([t/0]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-ifdef(E48).
-doc """
Appends `Value` to the end of `Vector`, returning the modified vector.

`Vector` must be a valid vector or a `{badvec, Vector}` error is raised.

See also `set/3`.
""".
-endif.

-spec append(Value, Vector1) -> Vector2
            when Value :: term(),
                 Vector1 :: t(),
                 Vector2 :: t().

append(Value, #steady_vector{ tail = Tail } = Vector) when tuple_size(Tail) < ?block_size ->
    Vector#steady_vector{
      count = Vector#steady_vector.count + 1,
      tail = tuple_append(Value, Tail)
     };
append(NewValue, #steady_vector{} = Vector) ->
    #steady_vector{ count = Count, shift = Shift, root = Root, tail = Tail } = Vector,
    NewCount = Count + 1,
    NewTail = {NewValue},
    case append_recur(Root, Shift, Tail) of
        {ok, NewRoot} ->
            Vector#steady_vector{ count = NewCount, root = NewRoot, tail = NewTail };
        {overflow, TailPath} ->
            NewShift = Shift + ?shift,
            NewRoot = {Root, TailPath},
            Vector#steady_vector{ count = NewCount, shift = NewShift, root = NewRoot, tail = NewTail }
    end;
append(_NewValue, Vector) ->
    ?vec_error(Vector).

%%%

-ifdef(E48).
-doc """
Returns the value of the element in `Vector` at 0-based `Index`.

`Index` must be an integer satisfying `0 =< Index < size(Vector)` or a `badarg`
error is raised. `Vector` must be a valid vector or a `{badvec, Vector}` error
is raised.

See also `get/3` and `find/2`.
""".
-endif.

-spec get(Index, Vector) -> Value | no_return()
            when Index :: index(),
                 Vector :: t(),
                 Value :: term().

get(Index, Vector) when ?is_existing_index(Index, Vector) ->
    fast_get(Index, Vector);
get(_Index, Vector) when ?is_vector(Vector) ->
    ?arg_error;
get(_Index, Vector) ->
    ?vec_error(Vector).

%%%

-ifdef(E48).
-doc """
Returns the value of the element in `Vector` at 0-based `Index`, or `Default` if
`Index >= size(Vector)`.

`Index` must be a non-negative integer or a `badarg` error is raised. `Vector`
must be a valid vector or a `{badvec, Vector}` error is raised.

See also `get/2` and `find/2`.
""".
-endif.

-spec get(Index, Vector, Default) -> Value | Default
            when Index :: index(),
                 Vector :: t(),
                 Default :: term(),
                 Value :: term().

get(Index, Vector, Default) when ?is_index(Index), ?is_vector(Vector) ->
    case Index < Vector#steady_vector.count of
        true -> fast_get(Index, Vector);
        _ -> Default
    end;
get(_Index, Vector, _Default) when ?is_vector(Vector) ->
    ?arg_error;
get(_Index, Vector, _Default) ->
    ?vec_error(Vector).

%%%

-ifdef(E48).
-doc """
Filters the elements of `Vector` using the predicate `Fun`, returning a new
vector with the elements for which `Fun(Index, Value)` returned `true`.

`Fun` must be an arity-2 function or a `badarg` error is raised. `Vector` must
be a valid vector or a `{badvec, Vector}` error is raised.
""".
-endif.

-spec filter(Fun, Vector1) -> Vector2
            when Fun :: fun((Index, Value) -> boolean()),
                 Index :: index(),
                 Value :: term(),
                 Vector1 :: t(),
                 Vector2 :: t().

filter(Fun, Vector) when is_function(Fun, 2) ->
    foldl(
      fun (Index, Value, Acc) ->
              case Fun(Index, Value) of
                  true -> append(Value, Acc);
                  false -> Acc
              end
      end,
      new(), Vector);
filter(_Fun, Vector) when ?is_vector(Vector) ->
    ?arg_error;
filter(_Fun, Vector) ->
    ?vec_error(Vector).

%%%

-ifdef(E48).
-doc """
Returns `{ok, Value}` for the element in `Vector` at 0-based `Index`, or `error`
if the index is too large.

`Index` must be a non-negative integer or a `badarg` error is raised. `Vector`
must be a valid vector or a `{badvec, Vector}` error is raised.

See also `get/2` and `get/3`.
""".
-endif.

-spec find(Index, Vector) -> {ok, Value} | error
            when Index :: index(),
                 Vector :: t(),
                 Value :: term().

find(Index, Vector) when ?is_index(Index), ?is_vector(Vector) ->
    case Index < Vector#steady_vector.count of
        true -> {ok, fast_get(Index, Vector)};
        _ -> error
    end;
find(_Index, Vector) when ?is_vector(Vector) ->
    ?arg_error;
find(_Index, Vector) ->
    ?vec_error(Vector).

%%%

-ifdef(E48).
-doc """
Calls `Fun(Index, Value, AccIn)` on successive elements of `Vector`, starting
with `AccIn =:= Acc0`.

`Fun/3` must return a new accumulator, which is passed to the next call. The
final value of the accumulator is returned; `Acc0` is returned if the vector is
empty.

`Fun` must be an arity-3 function or a `badarg` error is raised. `Vector` must
be a valid vector or a `{badvec, Vector}` error is raised.

See also `foldr/3`.
""".
-endif.

-spec foldl(Fun, Acc0, Vector) -> AccN
            when Fun :: fun((Index, Value, AccIn) -> AccOut),
                 Index :: index(),
                 Value :: term(),
                 AccIn :: Acc0 | AccOut,
                 AccOut :: term() | AccN,
                 Acc0 :: term(),
                 Vector :: t(),
                 AccN :: term().

foldl(Fun, Acc0, Vector) when is_function(Fun, 3), ?is_vector(Vector) ->
    countfoldl_leaves(Fun, Acc0, Vector);
foldl(_Fun, _Acc0, Vector) when ?is_vector(Vector) ->
    ?arg_error;
foldl(_Fun, _Acc0, Vector) ->
    ?vec_error(Vector).

%%%

-ifdef(E48).
-doc """
Like `foldl/3`, but `Vector` is traversed from right to left.

See also `foldl/3`.
""".
-endif.

-spec foldr(Fun, Acc0, Vector) -> AccN
            when Fun :: fun((Index, Value, Acc1) -> Acc2),
                 Index :: index(),
                 Value :: term(),
                 Acc1 :: Acc0 | Acc2,
                 Acc2 :: term() | AccN,
                 Acc0 :: term(),
                 Vector :: t(),
                 AccN :: term().

foldr(Fun, Acc0, Vector) when is_function(Fun, 3), ?is_vector(Vector) ->
    countfoldr_leaves(Fun, Acc0, Vector);
foldr(_Fun, _Acc0, Vector) when ?is_vector(Vector) ->
    ?arg_error;
foldr(_Fun, _Acc0, Vector) ->
    ?vec_error(Vector).

%%%

-ifdef(E48).
-doc """
Calls `Fun(Index, Value)` for each `Value` in `Vector`.

This function is used for its side effects, and the evaluation order is the same
as the order of the elements in the vector.

`Fun` must be an arity-2 function or a `badarg` error is raised. `Vector` must
be a valid vector or a `{badvec, Vector}` error is raised.
""".
-endif.

-spec foreach(Fun, Vector) -> ok
            when Fun :: fun((Index, Value) -> term()),
                 Index :: index(),
                 Value :: term(),
                 Vector :: t().

foreach(Fun, Vector) when is_function(Fun, 2) ->
    counteach_leaves(Fun, Vector);
foreach(_Fun, Vector) when ?is_vector(Vector) ->
    ?arg_error;
foreach(_Fun, Vector) ->
    ?vec_error(Vector).

%%%

-ifdef(E48).
-doc """
Converts a proper list into a vector containing the same elements in the same
order.

`List` must be a list or a `badarg` error is raised.

See also `to_list/1`.
""".
-endif.

-spec from_list(List) -> Vector
            when List :: list(),
                 Vector :: t().

from_list(List) when is_list(List) ->
    lists:foldl(fun append/2, new(), List);
from_list(_List) ->
    ?arg_error.

%%%

-ifdef(E48).
-doc """
Returns `true` if `Vector` is empty, `false` otherwise.

`Vector` must be a valid vector or a `{badvec, Vector}` error is raised.

See also `size/1`.
""".
-endif.

-spec is_empty(Vector) -> boolean()
            when Vector :: t().

is_empty(Vector) when ?is_vector(Vector) ->
    Vector#steady_vector.count =:= 0;
is_empty(Vector) ->
    ?vec_error(Vector).

%%%

-ifdef(E48).
-doc """
Returns `true` if `Term` is a `steady_vector`, `false` otherwise.
""".
-endif.

-spec is_steady_vector(Term) -> boolean()
            when Term :: term().

is_steady_vector(Term) ->
    ?is_vector(Term).

%%%

-ifdef(E48).
-doc """
Returns the last value of a non-empty `Vector`.

If `Vector` is empty, an `emptyvec` error is raised. `Vector` must be a valid
vector or a `{badvec, Vector}` error is raised.

See also `last/2`.
""".
-endif.

-spec last(Vector) -> Value | no_return()
            when Vector :: t(),
                 Value :: term().

last(#steady_vector{ count = Count } = Vector) ->
    case Count > 0 of
        true -> fast_get(Count - 1, Vector);
        _ -> ?empty_vec_error(Vector)
    end;
last(Vector) ->
    ?vec_error(Vector).

%%%

-ifdef(E48).
-doc """
Returns the last value of `Vector` if it is not empty, `Default` otherwise.

`Vector` must be a valid vector or a `{badvec, Vector}` error is raised.

See also `last/1`.
""".
-endif.

-spec last(Vector, Default) -> Value | Default
            when Vector :: t(),
                 Default :: term(),
                 Value :: term().

last(#steady_vector{ count = Count } = Vector, Default) ->
    case Count > 0 of
        true -> fast_get(Count - 1, Vector);
        _ -> Default
    end;
last(Vector, _Default) ->
    ?vec_error(Vector).

%%%

-ifdef(E48).
-doc """
Maps `Fun(Index, Value)` over every value of `Vector`, returning a new vector
containing the mapped values.

`Fun` must be an arity-2 function or a `badarg` error is raised. `Vector` must
be a valid vector or a `{badvec, Vector}` error is raised.
""".
-endif.

-spec map(Fun, Vector1) -> Vector2
            when Fun :: fun((Index, Value1) -> Value2),
                 Index :: index(),
                 Value1 :: term(),
                 Value2 :: term(),
                 Vector1 :: t(),
                 Vector2 :: t().

map(Fun, Vector) when is_function(Fun, 2) ->
    countmap_leaves(Fun, Vector);
map(_Fun, Vector) when ?is_vector(Vector) ->
    ?arg_error;
map(_Fun, Vector) ->
    ?vec_error(Vector).

%%%

-ifdef(E48).
-doc """
Returns a new, empty vector.
""".
-endif.

-spec new() -> Vector
            when Vector :: t().

new() ->
    #steady_vector{}.

%%%

-ifdef(E48).
-doc """
Removes the last value of the non-empty vector `Vector1` and returns `Vector2`
with that element removed.

If the vector is empty, an `emptyvec` error is raised. `Vector1` must be a valid
vector or a `{badvec, Vector1}` error is raised.
""".
-endif.

-spec remove_last(Vector1) -> Vector2 | no_return()
            when Vector1 :: t(),
                 Vector2 :: t().

remove_last(#steady_vector{ tail = Tail } = Vector) when tuple_size(Tail) > 1 ->
    NewTail = tuple_delete_last(Tail),
    Vector#steady_vector{
      count = Vector#steady_vector.count - 1,
      tail = NewTail };
remove_last(#steady_vector{ count = Count } = Vector) when Count > 1 ->
    #steady_vector{ shift = Shift, root = Root } = Vector,
    NewCount = Count - 1,
    {NewRoot, NewTail} = remove_last_recur(Root, Shift),
    if tuple_size(NewRoot) =:= 1 andalso Shift > ?shift ->
           NewShift = Shift - ?shift,
           {InnerNewRoot} = NewRoot, % remove topmost tree level
           Vector#steady_vector{ count = NewCount, root = InnerNewRoot,
                                 shift = NewShift, tail = NewTail };
       true ->
           Vector#steady_vector{ count = NewCount, root = NewRoot, tail = NewTail }
    end;
remove_last(#steady_vector{ count = 1 }) ->
    new();
remove_last(Vector) when ?is_vector(Vector) ->
    ?empty_vec_error;
remove_last(Vector) ->
    ?vec_error(Vector).

%%%

-ifdef(E48).
-doc """
Returns `Vector2`, a copy of `Vector1` with the element at 0-based `Index` set to
`Value`.

`Index` must be an integer satisfying `0 =< Index < size(Vector)` or a `badarg`
error is raised. If `Index` equals `size(Vector)`, this function behaves like
`append/2`. `Vector1` must be a valid vector or a `{badvec, Vector1}` error is
raised.

See also `append/2`.
""".
-endif.

-spec set(Index, Value, Vector1) -> Vector2 | no_return()
            when Index :: index(),
                 Value :: term(),
                 Vector1 :: t(),
                 Vector2 :: t().

set(Index, Value, Vector) when ?is_existing_index(Index, Vector) ->
    case Index >= tail_start(Vector) of
        true ->
            Tail = Vector#steady_vector.tail,
            ValueIndex = Index band ?mask,
            NewTail = tuple_set(ValueIndex, Value, Tail),
            Vector#steady_vector{ tail = NewTail };
        _ ->
            Root = Vector#steady_vector.root,
            NewRoot = set_recur(Root, Vector#steady_vector.shift, Index, Value),
            Vector#steady_vector{ root = NewRoot }
    end;
set(Index, Value, Vector) when ?is_next_index(Index, Vector) ->
    append(Value, Vector);
set(_Index, _Value, Vector) when ?is_vector(Vector) ->
    ?arg_error;
set(_Index, _Value, Vector) ->
    ?vec_error(Vector).

%%%

-ifdef(E48).
-doc """
Returns the number of elements in `Vector`.

`Vector` must be a valid vector or a `{badvec, Vector}` error is raised.

See also `is_empty/1`.
""".
-endif.

-spec size(Vector) -> non_neg_integer()
            when Vector :: t().

size(#steady_vector{ count = Count }) ->
    Count;
size(Vector) ->
    ?vec_error(Vector).

%%%

-ifdef(E48).
-doc """
Returns a list with the elements of `Vector` in the same order.

`Vector` must be a valid vector or a `{badvec, Vector}` error is raised.

See also `from_list/1`.
""".
-endif.

-spec to_list(Vector) -> list()
            when Vector :: t().

to_list(Vector) ->
    foldr_leaf_blocks(
      fun (Block, Acc) -> tuple_to_list(Block) ++ Acc end,
      [], Vector).

%% ------------------------------------------------------------------
%% Internal Function Definitions - Appending
%% ------------------------------------------------------------------

append_recur(Node, Level, Tail) when Level > ?shift ->
    LastChildIndex = tuple_size(Node) - 1,
    LastChild = tuple_get(LastChildIndex, Node),
    case append_recur(LastChild, Level - ?shift, Tail) of
        {ok, NewChild} ->
            {ok, tuple_set(LastChildIndex, NewChild, Node)};
        {overflow, TailPath} ->
            append_here(Node, TailPath)
    end;
append_recur(Node, _Level, Tail) ->
    append_here(Node, Tail).

append_here(Node, TailPath) ->
    case tuple_size(Node) < ?block_size of
        true ->
            {ok, tuple_append(TailPath, Node)};
        _ ->
            {overflow, {TailPath}}
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions - Fetching
%% ------------------------------------------------------------------

fast_get(Index, Vector) ->
    ValueIndex = Index band ?mask,
    case Index >= tail_start(Vector) of
        true ->
            tuple_get(ValueIndex, Vector#steady_vector.tail);
        _ ->
            Node = get_recur(Vector#steady_vector.root, Vector#steady_vector.shift, Index),
            tuple_get(ValueIndex, Node)
    end.

get_recur(Node, Level, Index) when Level > 0 ->
    ChildIndex = (Index bsr Level) band ?mask,
    Child = tuple_get(ChildIndex, Node),
    get_recur(Child, Level - ?shift, Index);
get_recur(Leaf, _Level, _Index) ->
    Leaf.

%% ------------------------------------------------------------------
%% Internal Function Definitions - Folding Left
%% ------------------------------------------------------------------

countfoldl_leaves(Fun, Acc1, Vector) ->
    #steady_vector{ shift = Shift, root = Root, tail = Tail } = Vector,
    Counter1 = 0,
    {Counter2, Acc2} = countfoldl_descendant_leaf_blocks(Fun, Counter1, Acc1, Root, Shift),
    {_Counter3, Acc3} = countfoldl_node_leaves(Fun, Counter2, Acc2, Tail),
    Acc3.

countfoldl_descendant_leaf_blocks(Fun, Counter, Acc, Node, Level) when Level =:= 0 ->
    % leaf node
    countfoldl_node_leaves(Fun, Counter, Acc, Node);
countfoldl_descendant_leaf_blocks(Fun, Counter0, Acc0, Node, Level) ->
    ChildrenLevel = Level - ?shift,
    tuple_foldl(
      fun (Child, Counter, Acc) ->
              countfoldl_descendant_leaf_blocks(Fun, Counter, Acc, Child, ChildrenLevel)
      end,
      Counter0, Acc0, Node).

countfoldl_node_leaves(Fun, Counter, Acc, Node) ->
    tuple_countfoldl(Fun, Counter, Acc, Node).

%% ------------------------------------------------------------------
%% Internal Function Definitions - Folding Right
%% ------------------------------------------------------------------

foldr_leaf_blocks(Fun, Acc1, Vector) ->
    #steady_vector{ shift = Shift, root = Root, tail = Tail } = Vector,
    Acc2 = Fun(Tail, Acc1),
    foldr_descendant_leaf_blocks(Fun, Acc2, Root, Shift).

foldr_descendant_leaf_blocks(Fun, Acc, Node, Level) when Level =:= ?shift ->
    % leaf node
    tuple_foldr(Fun, Acc, Node);
foldr_descendant_leaf_blocks(Fun, Acc0, Node, Level) ->
    ChildrenLevel = Level - ?shift,
    tuple_foldr(
      fun (Child, Acc) ->
              foldr_descendant_leaf_blocks(Fun, Acc, Child, ChildrenLevel)
      end,
      Acc0, Node).

countfoldr_leaves(Fun, Acc1, Vector) ->
    #steady_vector{ count = Size, shift = Shift, root = Root, tail = Tail } = Vector,
    Counter1 = Size - 1,
    {Counter2, Acc2} = countfoldr_node_leaves(Fun, Counter1, Acc1, Tail),
    {_Counter3, Acc3} = countfoldr_descendant_leaf_blocks(Fun, Counter2, Acc2, Root, Shift),
    Acc3.

countfoldr_descendant_leaf_blocks(Fun, Counter, Acc, Node, Level) when Level =:= 0 ->
    % leaf node
    countfoldr_node_leaves(Fun, Counter, Acc, Node);
countfoldr_descendant_leaf_blocks(Fun, Counter0, Acc0, Node, Level) ->
    ChildrenLevel = Level - ?shift,
    tuple_foldr(
      fun (Child, Counter, Acc) ->
              countfoldr_descendant_leaf_blocks(Fun, Counter, Acc, Child, ChildrenLevel)
      end,
      Counter0, Acc0, Node).

countfoldr_node_leaves(Fun, Counter, Acc, Node) ->
    tuple_countfoldr(Fun, Counter, Acc, Node).

%% ------------------------------------------------------------------
%% Internal Function Definitions - Iterating
%% ------------------------------------------------------------------

counteach_leaves(Fun, Vector) ->
    #steady_vector{ shift = Shift, root = Root, tail = Tail } = Vector,
    Counter1 = 0,
    Counter2 = counteach_descendant_leaf_blocks(Fun, Counter1, Root, Shift),
    _Counter3 = counteach_node_leaves(Fun, Counter2, Tail),
    ok.

counteach_descendant_leaf_blocks(Fun, Counter, Node, Level) when Level =:= 0 ->
    % leaf node
    counteach_node_leaves(Fun, Counter, Node);
counteach_descendant_leaf_blocks(Fun, Counter0, Node, Level) ->
    ChildrenLevel = Level - ?shift,
    tuple_foldl(
      fun (Child, Counter) ->
              counteach_descendant_leaf_blocks(Fun, Counter, Child, ChildrenLevel)
      end,
      Counter0, Node).

counteach_node_leaves(Fun, Counter0, Node) ->
    tuple_foldl(
      fun (Value, Counter) ->
              _ = Fun(Counter, Value),
              Counter + 1
      end,
      Counter0, Node).

%% ------------------------------------------------------------------
%% Internal Function Definitions - Mapping
%% ------------------------------------------------------------------

countmap_leaves(Fun, Vector) ->
    #steady_vector{ shift = Shift, root = Root1, tail = Tail1 } = Vector,
    Counter1 = 0,
    {Counter2, Root2} = countmap_descendant_leaf_blocks(Fun, Counter1, Root1, Shift),
    {_Counter3, Tail2} = countmap_node_leaves(Fun, Counter2, Tail1),
    Vector#steady_vector{ root = Root2, tail = Tail2 }.

countmap_descendant_leaf_blocks(Fun, Counter, Node, Level) when Level =:= 0 ->
    % leaf node
    countmap_node_leaves(Fun, Counter, Node);
countmap_descendant_leaf_blocks(Fun, Counter0, Node, Level) ->
    ChildrenLevel = Level - ?shift,
    tuple_map(
      fun (Counter, Child) ->
              countmap_descendant_leaf_blocks(Fun, Counter, Child, ChildrenLevel)
      end,
      Counter0, Node).

countmap_node_leaves(Fun, Counter0, Node) ->
    tuple_countmap(Fun, Counter0, Node).

%% ------------------------------------------------------------------
%% Internal Function Definitions - Removing
%% ------------------------------------------------------------------

remove_last_recur(Node, Level) when Level > ?shift ->
    ChildIndex = tuple_size(Node) - 1,
    Child = tuple_get(ChildIndex, Node),
    case remove_last_recur(Child, Level - ?shift) of
        {{}, LastBlock} ->
            NewNode = tuple_delete(ChildIndex, Node),
            {NewNode, LastBlock};
        {NewChild, LastBlock} ->
            NewNode = tuple_set(ChildIndex, NewChild, Node),
            {NewNode, LastBlock}
    end;
remove_last_recur(Node, _Level) ->
    ChildIndex = tuple_size(Node) - 1,
    Child = tuple_get(ChildIndex, Node),
    NewNode = tuple_delete(ChildIndex, Node),
    {NewNode, Child}.

%% ------------------------------------------------------------------
%% Internal Function Definitions - Setting
%% ------------------------------------------------------------------

set_recur(Node, Level, Index, Val) when Level > 0 ->
    ChildIndex = (Index bsr Level) band ?mask,
    Child = tuple_get(ChildIndex, Node),
    NewChild = set_recur(Child, Level - ?shift, Index, Val),
    tuple_set(ChildIndex, NewChild, Node);
set_recur(Leaf, _Level, Index, Val) ->
    ValueIndex = Index band ?mask,
    tuple_set(ValueIndex, Val, Leaf).

%% ------------------------------------------------------------------
%% Internal Function Definitions - Utilities
%% ------------------------------------------------------------------

tail_start(#steady_vector{} = Vector) ->
    Vector#steady_vector.count - tuple_size(Vector#steady_vector.tail).

-compile({inline,{tuple_append,2}}).
tuple_append(Value, Tuple) ->
    erlang:append_element(Tuple, Value).

-compile({inline,{tuple_delete,2}}).
tuple_delete(Index, Tuple) ->
    erlang:delete_element(Index + 1, Tuple).

-compile({inline,{tuple_delete_last,1}}).
tuple_delete_last(Tuple) ->
    erlang:delete_element(tuple_size(Tuple), Tuple).

-compile({inline,{tuple_get,2}}).
tuple_get(Index, Tuple) ->
    element(Index + 1, Tuple).

-compile({inline,{tuple_foldl,3}}).
tuple_foldl(Fun, Acc, Tuple) ->
    lists:foldl(Fun, Acc, tuple_to_list(Tuple)).

-compile({inline,{tuple_foldl,4}}).
tuple_foldl(Fun, Counter, Acc, Tuple) ->
    List = tuple_to_list(Tuple),
    tuple_foldl_recur(Fun, Counter, Acc, List).

-compile({inline,{tuple_foldl_recur,4}}).
tuple_foldl_recur(_Fun, Counter, Acc, []) ->
    {Counter, Acc};
tuple_foldl_recur(Fun, Counter1, Acc1, [H|T]) ->
    {Counter2, Acc2} = Fun(H, Counter1, Acc1),
    tuple_foldl_recur(Fun, Counter2, Acc2, T).

-compile({inline,{tuple_countfoldl,4}}).
tuple_countfoldl(Fun, Counter, Acc, Tuple) ->
    List = tuple_to_list(Tuple),
    tuple_countfoldl_recur(Fun, Counter, Acc, List).

-compile({inline,{tuple_countfoldl_recur,4}}).
tuple_countfoldl_recur(_Fun, Counter, Acc, []) ->
    {Counter, Acc};
tuple_countfoldl_recur(Fun, Counter, Acc1, [H|T]) ->
    Acc2 = Fun(Counter, H, Acc1),
    tuple_countfoldl_recur(Fun, Counter + 1, Acc2, T).

-compile({inline,{tuple_foldr,3}}).
tuple_foldr(Fun, Acc, Tuple) ->
    lists:foldr(Fun, Acc, tuple_to_list(Tuple)).

-compile({inline,{tuple_foldr,4}}).
tuple_foldr(Fun, Counter, Acc, Tuple) ->
    List = lists:reverse(tuple_to_list(Tuple)),
    tuple_foldr_recur(Fun, Counter, Acc, List).

-compile({inline,{tuple_foldr_recur,4}}).
tuple_foldr_recur(_Fun, Counter, Acc, []) ->
    {Counter, Acc};
tuple_foldr_recur(Fun, Counter1, Acc1, [H|T]) ->
    {Counter2, Acc2} = Fun(H, Counter1, Acc1),
    tuple_foldr_recur(Fun, Counter2, Acc2, T).

-compile({inline,{tuple_countfoldr,4}}).
tuple_countfoldr(Fun, Counter, Acc, Tuple) ->
    List = lists:reverse(tuple_to_list(Tuple)),
    tuple_countfoldr_recur(Fun, Counter, Acc, List).

-compile({inline,{tuple_countfoldr_recur,4}}).
tuple_countfoldr_recur(_Fun, Counter, Acc, []) ->
    {Counter, Acc};
tuple_countfoldr_recur(Fun, Counter, Acc1, [H|T]) ->
    Acc2 = Fun(Counter, H, Acc1),
    tuple_countfoldr_recur(Fun, Counter - 1, Acc2, T).

-compile({inline,{tuple_map,3}}).
tuple_map(Fun, Counter, Tuple) ->
    List = tuple_to_list(Tuple),
    tuple_map_recur(Fun, Counter, List, []).

-compile({inline,{tuple_map_recur,4}}).
tuple_map_recur(_Fun, Counter, [], Acc) ->
    Tuple = list_to_tuple(lists:reverse(Acc)),
    {Counter, Tuple};
tuple_map_recur(Fun, Counter1, [Value1 | T], Acc) ->
    {Counter2, Value2} = Fun(Counter1, Value1),
    tuple_map_recur(Fun, Counter2, T, [Value2 | Acc]).

-compile({inline,{tuple_countmap,3}}).
tuple_countmap(Fun, Counter, Tuple) ->
    List = tuple_to_list(Tuple),
    tuple_countmap_recur(Fun, Counter, List, []).

-compile({inline,{tuple_countmap_recur,4}}).
tuple_countmap_recur(_Fun, Counter, [], Acc) ->
    {Counter, list_to_tuple(lists:reverse(Acc))};
tuple_countmap_recur(Fun, Counter1, [Value1 | T], Acc) ->
    Value2 = Fun(Counter1, Value1),
    Counter2 = Counter1 + 1,
    tuple_countmap_recur(Fun, Counter2, T, [Value2 | Acc]).

-compile({inline,{tuple_set,3}}).
tuple_set(Index, Value, Tuple) ->
    setelement(Index + 1, Tuple, Value).


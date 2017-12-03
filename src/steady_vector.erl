-module(steady_vector).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-compile({no_auto_import,[{size,1}]}).
-compile(inline_list_funcs).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([append/2]).                -ignore_xref({append,2}).
-export([get/2]).                   -ignore_xref({get,2}).
-export([get/3]).                   -ignore_xref({get,3}).
-export([filter/2]).                -ignore_xref({filter,2}).
-export([find/2]).                  -ignore_xref({find,2}).
-export([foldl/3]).                 -ignore_xref({foldl,3}).
-export([foldr/3]).                 -ignore_xref({foldr,3}).
-export([foreach/2]).               -ignore_xref({foreach,2}).
-export([from_list/1]).             -ignore_xref({from_list,1}).
-export([is_empty/1]).              -ignore_xref({is_empty,1}).
-export([is_steady_vector/1]).      -ignore_xref({is_steady_vector,1}).
-export([last/1]).                  -ignore_xref({last,1}).
-export([last/2]).                  -ignore_xref({last,2}).
-export([map/2]).                   -ignore_xref({map,2}).
-export([new/0]).                   -ignore_xref({new,0}).
-export([remove_last/1]).           -ignore_xref({remove_last,1}).
-export([set/3]).                   -ignore_xref({set,3}).
-export([size/1]).                  -ignore_xref({size,1}).
-export([to_list/1]).               -ignore_xref({to_list,1}).

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

-type index() :: non_neg_integer().
-export_type([index/0]).

-record(steady_vector, {
          count = 0 :: index(),
          shift = ?shift :: shift(),
          root = {} :: tuple(),
          tail = {} :: tuple()
         }).
-opaque t() :: #steady_vector{}.
-export_type([t/0]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec append(Value, Vector1) -> Vector2
            when Value :: term(),
                 Vector1 :: t(),
                 Vector2 :: t().
%% @doc Appends `Value' to the end of `Vector'
%% `Vector' must be a valid vector or a `{badvec,Vector}' error will be raised.
%% @see set/3
%% @returns Modified `Vector'
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

-spec get(Index, Vector) -> Value | no_return()
            when Index :: index(),
                 Vector :: t(),
                 Value :: term().
%% @doc Returns value of element in `Vector' at `0'-based `Index'.
%% `Index' must be an integer and satisfy condition `0 =< Index =< size(Vector)' or a `badarg' error will be raised.
%% `Vector' must be a valid vector or a `{badvec,Vector}' error will be raised.
%% @see get/3
%% @see find/2
get(Index, Vector) when ?is_existing_index(Index, Vector) ->
    fast_get(Index, Vector);
get(_Index, Vector) when ?is_vector(Vector) ->
    ?arg_error;
get(_Index, Vector) ->
    ?vec_error(Vector).

%% @doc Returns value of element in `Vector' at `0'-based `Index' or `Default' if `Index >= size(Vector)'.
%% `Index' must be a non-negative integer or a `badarg' error will be raised.
%% `Vector' must be a valid vector or a `{badvec,Vector}' error will be raised.
%% @see get/2
%% @see find/2
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

%% @doc Filters `Vector' elements using predicate `Fun'.
%% `Fun' must be an arity-2 function or a `badarg' error will be raised.
%% `Vector' must be a valid vector or a `{badvec,Vector}' error will be raised.
%% @returns The modified vector containing the filtered elements.
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

%% @doc Returns success-wrapped value of element in `Vector' at `0'-based `Index',
%% or `error' if the index is too large.
%% `Index' must be a non-negative integer or a `badarg' error will be raised.
%% `Vector' must be a valid vector or a `{badvec,Vector}' error will be raised.
%% @see get/2
%% @see get/3
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

%% @doc Calls `Fun(Index, Value, AccIn)' on successive elements of `Vector', starting with AccIn == Acc0.
%% `Fun/3' must return a new accumulator, which is passed to the next call.
%% The function returns the final value of the accumulator. `Acc0' is returned if the vector is empty.
%% `Fun' must be an arity-3 function or a `badarg' error will be raised.
%% `Vector' must be a valid vector or a `{badvec,Vector}' error will be raised.
%% @see foldr/3
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

%% @doc Like `foldl/3' but `Vector' is traversed from right to left.
%% @see foldl/3
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

%% @doc Calls `Fun(Index, Value)' for each `Value' in `Vector'.
%% This function is used for its side effects and the evaluation order
%% is defined to be the same as the order of the elements in the vector.
%% `Fun' must be an arity-2 function or a `badarg' error will be raised.
%% `Vector' must be a valid vector or a `{badvec,Vector}' error will be raised.
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

%% @doc Converts a proper `List' of elements to a `Vector' containing them in the same order.
%% `List' must be a list or a `{badvec,Vector}' error will be raised.
%% @see to_list/1
-spec from_list(List) -> Vector
            when List :: list(),
                 Vector :: t().
from_list(List) when is_list(List) ->
    lists:foldl(fun append/2, new(), List);
from_list(_List) ->
    ?arg_error.

%% @doc Returns `true' if `Vector' is empty, `false' otherwise.
%% `Vector' must be a valid vector or a `{badvec,Vector}' error will be raised.
%% @see size/1
-spec is_empty(Vector) -> boolean()
            when Vector :: t().
is_empty(Vector) when ?is_vector(Vector) ->
    Vector#steady_vector.count =:= 0;
is_empty(Vector) ->
    ?vec_error(Vector).

%% @doc Returns `true' if `Term' is a `steady_vector', `false' otherwise.
-spec is_steady_vector(Term) -> boolean()
            when Term :: term().
is_steady_vector(Term) ->
    ?is_vector(Term).

%% @doc Returns the last value of a non-empty `Vector'.
%% If `Vector' is empty, an `emptyvec' error will be raised.
%% `Vector' must be a valid vector or a `{badvec,Vector}' error will be raised.
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

%% @doc Returns the last value `Vector' if it's not empty, `Default' otherwise.
%% `Vector' must be a valid vector or a `{badvec,Vector}' error will be raised.
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

%% @doc Maps function `Fun(Index, Value)' to all values of vector `Vector'.
%% Returns a new vector containing the mapped values.
%% `Vector' must be a valid vector or a `{badvec,Vector}' error will be raised.
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

%% @doc Returns an empty vector.
-spec new() -> Vector
            when Vector :: t().
new() ->
    #steady_vector{}.

%% @doc Removes the last value of non-empty `Vector1' and returns `Vector2' with the element removed.
%% If `Vector' is empty, an `emptyvec' error will be raised.
%% `Vector' must be a valid vector or a `{badvec,Vector}' error will be raised.
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

%% @doc Returns updated `Vector2' with element at `0'-based `Index' set to `Value'.
%% `Index' must be an integer and satisfy condition `0 =< Index < size(Vector)'
%% or a `badarg' error will be raised. If `Index' equals `size(Vector)', this
%% function will behave like append/2.
%% `Vector' must be a valid vector or a `{badvec,Vector}' error will be raised.
%% @see append/2
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

%% @doc Returns number of elements in `Vector'.
%% `Vector' must be a valid vector or a `{badvec,Vector}' error will be raised.
%% @see is_empty/1
-spec size(Vector) -> non_neg_integer()
            when Vector :: t().
size(#steady_vector{ count = Count }) ->
    Count;
size(Vector) ->
    ?vec_error(Vector).

%% @doc Returns list with `Vector''s elements in the same order.
%% `Vector' must be a valid vector or a `{badvec,Vector}' error will be raised.
%% @see from_list/1
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

%% ------------------------------------------------------------------
%% EUnit Definitions
%% ------------------------------------------------------------------
-ifdef(TEST).

empty_test() ->
    Vec = ?MODULE:new(),
    ?assert(?MODULE:is_empty(Vec)),
    ?assertEqual(0, ?MODULE:size(Vec)),
    ?assertEqual(it_is_empty, ?MODULE:last(Vec, it_is_empty)),
    ?assertError(badarg, ?MODULE:get(0, Vec)),
    ?assertError(emptyvec, ?MODULE:last(Vec)),
    ?assertEqual(not_found, ?MODULE:get(1, Vec, not_found)),
    ?assertEqual(error, ?MODULE:find(1, Vec)).

brute_get_test() ->
    Vec = #steady_vector{ count = 5, root = {{0,1,2}, {4}} },
    ?assertEqual(0, ?MODULE:get(0, Vec)),
    ?assertEqual(1, ?MODULE:get(1, Vec)),
    ?assertEqual(2, ?MODULE:get(2, Vec)),
    ?assertEqual(4, ?MODULE:get(4, Vec)).

append_to_tail_test() ->
    Vec1 = ?MODULE:append(0, ?MODULE:new()),
    ?assertEqual(1, ?MODULE:size(Vec1)),
    ?assertNot(?MODULE:is_empty(Vec1)),
    ?assertEqual(0, ?MODULE:get(0, Vec1)),

    Vec2 = ?MODULE:append(1, Vec1),
    ?assertEqual(2, ?MODULE:size(Vec2)),
    ?assertEqual(0, ?MODULE:get(0, Vec2)),
    ?assertEqual(1, ?MODULE:get(1, Vec2)).

append_to_root_test() ->
    C = 68,
    Vec1 =
        lists:foldl(
          fun append_and_assert_element_identity/2,
          ?MODULE:new(), lists:seq(0, C - 1)),
    ?assertEqual(C, ?MODULE:size(Vec1)),

    Vec2 = assert_element_identity( ?MODULE:append(C, Vec1) ),
    ?assertEqual(C + 1, ?MODULE:size(Vec2)),
    ?assertEqual(C, ?MODULE:get(C, Vec2)),

    ?assertError(badarg, ?MODULE:get(C + 1, Vec2)),
    ?assertError(badarg, ?MODULE:get("hello", Vec2)),
    ?assertError(badarg, ?MODULE:get({1}, Vec2)).

remove_last_tail_test() ->
    Vec1 = ?MODULE:append(0, ?MODULE:new()),
    Vec1_R = ?MODULE:remove_last(Vec1),
    ?assertEqual(0, ?MODULE:size(Vec1_R)),

    Vec2 = ?MODULE:append(1, Vec1),
    Vec2_R = ?MODULE:remove_last(Vec2),
    ?assertEqual(1, ?MODULE:size(Vec2_R)),
    ?assertEqual(0, ?MODULE:get(0, Vec2_R)).

remove_last_root_test() ->
    C = 20,
    Vec1 = lists:foldl(
             fun ?MODULE:append/2,
             ?MODULE:new(), lists:seq(0, C - 1)),
    Vec2 = lists:foldl(
             fun (Index, Acc) ->
                     assert_element_identity(Acc),
                     ?assertEqual(Index + 1, ?MODULE:size(Acc)),
                     ?MODULE:remove_last(Acc)
             end,
             Vec1, lists:seq(C - 1, 0, -1)),

    ?assertEqual(0, ?MODULE:size(Vec2)),
    ?assertError({badvec,Vec2}, ?MODULE:remove_last(Vec2)).

set_tail_test() ->
    Vec1 = assert_element_identity(
             ?MODULE:set(1, 1, ?MODULE:set(0, 0, ?MODULE:new())) ),
    ?assertEqual(2, ?MODULE:size(Vec1)),

    C = 4,
    IndexSeq = lists:seq(0, C - 1),
    Vec2A = lists:foldl(
              fun ?MODULE:append/2,
              ?MODULE:new(), IndexSeq),
    Vec2B = lists:foldl(
              fun (Index, Acc) ->
                      ?MODULE:set(Index, Index + 10, Acc)
              end,
              Vec1, IndexSeq),

    ?assertEqual(?MODULE:size(Vec2A), ?MODULE:size(Vec2B)),
    lists:foreach(
      fun (Index) ->
              ?assertEqual(?MODULE:get(Index, Vec2A), ?MODULE:get(Index, Vec2B) - 10)
      end,
      IndexSeq).

set_root_test() ->
    C = 20,
    IndexSeq = lists:seq(0, C - 1),
    Vec1 = lists:foldl(
              fun ?MODULE:append/2,
              ?MODULE:new(), IndexSeq),
    Vec2 = lists:foldl(
              fun (Index, Acc) ->
                      ?MODULE:set(Index, Index + 10, Acc)
              end,
              Vec1, IndexSeq),

    ?assertEqual(?MODULE:size(Vec1), ?MODULE:size(Vec2)),
    lists:foreach(
      fun (Index) ->
              ?assertEqual(?MODULE:get(Index, Vec1), ?MODULE:get(Index, Vec2) - 10)
      end,
      IndexSeq),
    ?assertError(badarg, ?MODULE:set(1, 1, ?MODULE:new())),
    ?assertError(badarg, ?MODULE:set("bla", 1, ?MODULE:new())).

from_and_to_list_test() ->
    C = 1000,
    List = [{rand:uniform(), Index} || Index <- lists:seq(1, C)],

    % convert from
    Vec = ?MODULE:from_list(List),
    ?assertEqual(length(List), ?MODULE:size(Vec)),

    % convert to
    List2 = ?MODULE:to_list(Vec),
    ?assertEqual(?MODULE:size(Vec), length(List2)),

    % all elements were kept
    _ = lists:zipwith(
          fun (Left, Right) ->
                  ?assertEqual(Left, Right)
          end,
          List, List2).

filter_test() ->
    C = 1000,
    ListFilterFun = fun ({V, _Index}) -> V band 1 =:= 0 end,
    VecFilterFun = fun (Index, {_, OrigIndex} = Value) ->
                        ?assertEqual(Index, OrigIndex - 1),
                        ListFilterFun(Value)
                   end,
    List = [{rand:uniform(1000), Index} || Index <- lists:seq(1, C)],
    Vec = ?MODULE:from_list(List),
    FilteredList = lists:filter(ListFilterFun, List),
    FilteredVec = ?MODULE:filter(VecFilterFun, Vec),
    ?MODULE:foldl(
       fun (_Index, VecValue, [ListValue | Next]) ->
               ?assertEqual(VecValue, ListValue),
               Next
       end,
       FilteredList, FilteredVec).

foldl_test() ->
    C = 1000,
    List = [{rand:uniform(), Index} || Index <- lists:seq(1, C)],
    Vec = ?MODULE:from_list(List),
    ?MODULE:foldl(
       fun (VecIndex, {VecValue, OrigVecIndex},
            [{ListValue, OrigListIndex} | Next]) ->
               ?assertEqual(VecValue, ListValue),
               ?assertEqual(VecIndex, OrigVecIndex - 1),
               ?assertEqual(VecIndex, OrigListIndex - 1),
               Next
       end,
       List, Vec).

foldr_test() ->
    C = 1000,
    List = [{rand:uniform(), Index} || Index <- lists:seq(1, C)],
    Vec = ?MODULE:from_list(List),
    RevList = lists:reverse(List),
    ?MODULE:foldr(
       fun (VecIndex, {VecValue, OrigVecIndex},
            [{ListValue, OrigListIndex} | Next]) ->
               ?assertEqual(VecValue, ListValue),
               ?assertEqual(VecIndex, OrigVecIndex - 1),
               ?assertEqual(VecIndex, OrigListIndex - 1),
               Next
       end,
       RevList, Vec).

foreach_test() ->
    C = 1000,
    List = [{rand:uniform(), Index} || Index <- lists:seq(1, C)],
    Vec = ?MODULE:from_list(List),
    ProcDicKey = make_ref(),
    undefined = put(ProcDicKey, List),
    ?MODULE:foreach(
       fun (Index, {Value, OrigIndex}) ->
               [{ListValue, ListOrigIndex} | Next] = get(ProcDicKey),
               ?assertEqual(Value, ListValue),
               ?assertEqual(Index, OrigIndex - 1),
               ?assertEqual(Index, ListOrigIndex - 1),
               put(ProcDicKey, Next)
       end,
       Vec),
    erlang:put(ProcDicKey, undefined).

map_test() ->
    C = 1000,
    ListMapFun = fun ({V, Index}) -> {V * 2, Index} end,
    VecMapFun = fun (Index, {_, OrigIndex} = Value) ->
                        ?assertEqual(Index, OrigIndex - 1),
                        ListMapFun(Value)
                end,
    List = [{rand:uniform(), Index} || Index <- lists:seq(1, C)],
    Vec = ?MODULE:from_list(List),
    MappedList = lists:map(ListMapFun, List),
    MappedVec = ?MODULE:map(VecMapFun, Vec),
    ?MODULE:foldl(
       fun (_Index, VecValue, [ListValue | Next]) ->
               ?assertEqual(VecValue, ListValue),
               Next
       end,
       MappedList, MappedVec).

append_and_assert_element_identity(Value, Vec1) ->
    Vec2 = ?MODULE:append(Value, Vec1),
    assert_element_identity(Vec2).

assert_element_identity(Vec) ->
    C = ?MODULE:size(Vec),

    % "randomly" use different getters
    ValidationFun =
        case C rem 3 of
            0 -> fun (Index) -> ?assertEqual(Index, ?MODULE:get(Index, Vec)) end;
            1 -> fun (Index) -> ?assertEqual(Index, ?MODULE:get(Index, Vec, not_found)) end;
            2 -> fun (Index) -> ?assertEqual({ok, Index}, ?MODULE:find(Index, Vec)) end
        end,

    lists:foreach(ValidationFun, lists:seq(0, C - 1)),

    if C > 0 ->
           ?assertEqual(C - 1, ?MODULE:last(Vec, empty)),
           ?assertEqual(C - 1, ?MODULE:last(Vec));
       true ->
           ?assertEqual(empty, ?MODULE:last(Vec, empty)),
           ?assertError(badarg, ?MODULE:last(Vec))
    end,
    Vec.

-endif.

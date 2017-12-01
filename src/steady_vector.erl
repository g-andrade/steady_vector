-module(steady_vector).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-compile({no_auto_import,[{size,1}]}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([append/2]).                -ignore_xref({append,2}).
-export([get/2]).                   -ignore_xref({get,2}).
-export([get/3]).                   -ignore_xref({get,3}).
-export([find/2]).                  -ignore_xref({find,2}).
-export([foldl/3]).                 -ignore_xref({foldl,3}).
-export([foldr/3]).                 -ignore_xref({foldr,3}).
-export([from_list/1]).             -ignore_xref({from_list,1}).
-export([is_empty/1]).              -ignore_xref({is_empty,1}).
-export([last/1]).                  -ignore_xref({last,1}).
-export([last/2]).                  -ignore_xref({last,2}).
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

-define(block, (1 bsl ?shift)).
-define(mask, (?block - 1)).

-define(is_existing_index(I, V), (is_integer((I)) andalso (I) >= 0 andalso (I) < (V)#steady_vector.count)).
-define(is_new_index(I, V), ((I)=:= (V)#steady_vector.count)).

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
-type t() :: #steady_vector{}.
-export_type([t/0]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec append(Value, Vector1) -> Vector2
            when Value :: term(),
                 Vector1 :: t(),
                 Vector2 :: t().
append(Value, #steady_vector{ tail = Tail } = Vector) when tuple_size(Tail) < ?block ->
    Vector#steady_vector{
      count = Vector#steady_vector.count + 1,
      tail = tuple_append(Value, Tail)
     };
append(NewValue, Vector) ->
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
    end.

-spec get(Index, Vector) -> Value | no_return()
            when Index :: index(),
                 Vector :: t(),
                 Value :: term().
get(Index, Vector) when ?is_existing_index(Index, Vector) ->
    fast_get(Index, Vector);
get(_Index, _Vec) ->
    error(badarg).

-spec get(Index, Vector, Default) -> Value | Default
            when Index :: index(),
                 Vector :: t(),
                 Default :: term(),
                 Value :: term().
get(Index, Vector, _Default) when ?is_existing_index(Index, Vector) ->
    fast_get(Index, Vector);
get(_Index, _Vec, Default) ->
    Default.

-spec find(Index, Vector) -> {ok, Value} | error
            when Index :: index(),
                 Vector :: t(),
                 Value :: term().
find(Index, Vector) when ?is_existing_index(Index, Vector) ->
    {ok, fast_get(Index, Vector)};
find(_Index, _Vec) ->
    error.

-spec foldl(Fun, Acc0, Vector) -> AccN
            when Fun :: fun((Value, Acc1) -> Acc2),
                 Value :: term(),
                 Acc1 :: Acc0 | Acc2,
                 Acc2 :: term() | AccN,
                 Acc0 :: term(),
                 Vector :: t(),
                 AccN :: term().
foldl(Fun, Acc0, Vector) ->
    #steady_vector{ shift = Shift, root = Root, tail = Tail } = Vector,
    foldl_root(Fun, Acc0, Root, Tail, Shift, 0).

-spec foldr(Fun, Acc0, Vector) -> AccN
            when Fun :: fun((Value, Acc1) -> Acc2),
                 Value :: term(),
                 Acc1 :: Acc0 | Acc2,
                 Acc2 :: term() | AccN,
                 Acc0 :: term(),
                 Vector :: t(),
                 AccN :: term().
foldr(Fun, Acc0, Vector) ->
    #steady_vector{ shift = Shift, root = Root, tail = Tail } = Vector,
    foldr_tail(Fun, Acc0, Tail, Root, Shift).

-spec from_list(List) -> Vector
            when List :: list(),
                 Vector :: t().
from_list(List) ->
    lists:foldl(fun append/2, new(), List).

-spec is_empty(Vector) -> boolean()
            when Vector :: t().
is_empty(Vector) ->
    Vector#steady_vector.count =:= 0.

-spec last(Vector) -> Value | no_return()
            when Vector :: t(),
                 Value :: term().
last(#steady_vector{ count = Count } = Vector) when Count > 0 ->
    fast_get(Count - 1, Vector);
last(_Vec) ->
    error(badarg).

-spec last(Vector, Default) -> Value | Default
            when Vector :: t(),
                 Default :: term(),
                 Value :: term().
last(#steady_vector{ count = Count } = Vector, _Default) when Count > 0 ->
    fast_get(Count - 1, Vector);
last(_Vec, Default) ->
    Default.

-spec new() -> Vector
            when Vector :: t().
new() ->
    #steady_vector{}.

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
remove_last(_Vec) ->
    error(badarg).

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
        false ->
            Root = Vector#steady_vector.root,
            NewRoot = set_recur(Root, Vector#steady_vector.shift, Index, Value),
            Vector#steady_vector{ root = NewRoot }
    end;
set(Index, Value, Vector) when ?is_new_index(Index, Vector) ->
    append(Value, Vector);
set(_Index, _Value, _Vec) ->
    error(badarg).

-spec size(Vector) -> non_neg_integer()
            when Vector :: t().
size(Vector) ->
    Vector#steady_vector.count.

-spec to_list(Vector) -> list()
            when Vector :: t().
to_list(Vector) ->
    foldr(
      fun (Value, Acc) -> [Value | Acc] end,
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
    case tuple_size(Node) < ?block of
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

foldl_node(Fun, Acc1, Node, Level, Index) when Level > 0, Index < tuple_size(Node) ->
    Child = tuple_get(Index, Node),
    Acc2 = foldl_node(Fun, Acc1, Child, Level - ?shift, 0),
    foldl_node(Fun, Acc2, Node, Level, Index + 1);
foldl_node(Fun, Acc1, Node, Level, Index) when Index < tuple_size(Node) ->
    Value = tuple_get(Index, Node),
    Acc2 = Fun(Value, Acc1),
    foldl_node(Fun, Acc2, Node, Level, Index + 1);
foldl_node(_Fun, Acc, _Node, _Level, _Index) ->
    Acc.

foldl_root(Fun, Acc1, Root, Tail, Level, Index) when Index < tuple_size(Root) ->
    Child = tuple_get(Index, Root),
    Acc2 = foldl_node(Fun, Acc1, Child, Level - ?shift, 0),
    foldl_root(Fun, Acc2, Root, Tail, Level, Index + 1);
foldl_root(Fun, Acc, _Root, Tail, _Level, _Index) ->
    foldl_tail(Fun, Acc, Tail, 0).

foldl_tail(Fun, Acc1, Tail, Index) when Index < tuple_size(Tail) ->
    Value = tuple_get(Index, Tail),
    Acc2 = Fun(Value, Acc1),
    foldl_tail(Fun, Acc2, Tail, Index + 1);
foldl_tail(_Fun, AccN, _Tail, _Index) ->
    AccN.

%% ------------------------------------------------------------------
%% Internal Function Definitions - Folding Right
%% ------------------------------------------------------------------

foldr_node(Fun, Acc, Node, Level) ->
    foldr_node(Fun, Acc, Node, Level, tuple_size(Node) - 1).

foldr_node(Fun, Acc1, Node, Level, Index) when Level > 0, Index >= 0 ->
    Child = tuple_get(Index, Node),
    Acc2 = foldr_node(Fun, Acc1, Child, Level - ?shift),
    foldr_node(Fun, Acc2, Node, Level, Index - 1);
foldr_node(Fun, Acc1, Node, Level, Index) when Index >= 0 ->
    Value = tuple_get(Index, Node),
    Acc2 = Fun(Value, Acc1),
    foldr_node(Fun, Acc2, Node, Level, Index - 1);
foldr_node(_Fun, Acc, _Node, _Level, _Index) ->
    Acc.

foldr_root(Fun, Acc, Root, Level) ->
    foldr_root(Fun, Acc, Root, Level, tuple_size(Root) - 1).

foldr_root(Fun, Acc1, Root, Level, Index) when Index >= 0 ->
    Child = tuple_get(Index, Root),
    Acc2 = foldr_node(Fun, Acc1, Child, Level - ?shift),
    foldr_root(Fun, Acc2, Root, Level, Index - 1);
foldr_root(_Fun, Acc, _Root, _Level, _Index) ->
    Acc.

foldr_tail(Fun, Acc, Tail, Root, Shift) ->
    foldr_tail(Fun, Acc, Tail, Root, Shift, tuple_size(Tail) - 1).

foldr_tail(Fun, Acc1, Tail, Root, Shift, Index) when Index >= 0 ->
    Value = tuple_get(Index, Tail),
    Acc2 = Fun(Value, Acc1),
    foldr_tail(Fun, Acc2, Tail, Root, Shift, Index - 1);
foldr_tail(Fun, Acc, _Tail, Root, Shift, _Index) ->
    foldr_root(Fun, Acc, Root, Shift).

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

tuple_append(Value, Tuple) ->
    erlang:append_element(Tuple, Value).

tuple_delete(Index, Tuple) ->
    erlang:delete_element(Index + 1, Tuple).

tuple_delete_last(Tuple) ->
    erlang:delete_element(tuple_size(Tuple), Tuple).

tuple_get(Index, Tuple) ->
    element(Index + 1, Tuple).

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
    ?assertError(badarg, ?MODULE:last(Vec)),
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
    ?assertError(badarg, ?MODULE:remove_last(Vec2)).

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

foldl_test() ->
    C = 1000,
    List = [{rand:uniform(), Index} || Index <- lists:seq(1, C)],
    Vec = ?MODULE:from_list(List),
    ?MODULE:foldl(
       fun (VecValue, [ListValue | Next]) ->
               ?assertEqual(VecValue, ListValue),
               Next
       end,
       List, Vec).

foldr_test() ->
    C = 1000,
    List = [{rand:uniform(), Index} || Index <- lists:seq(1, C)],
    Vec = ?MODULE:from_list(List),
    RevList = lists:reverse(List),
    ?MODULE:foldr(
       fun (VecValue, [ListValue | Next]) ->
               ?assertEqual(VecValue, ListValue),
               Next
       end,
       RevList, Vec).

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

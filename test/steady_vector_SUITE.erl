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

-module(steady_vector_SUITE).
-compile(export_all).

-include_lib("stdlib/include/assert.hrl").

%% ------------------------------------------------------------------
%% Setup
%% ------------------------------------------------------------------

all() ->
    [
        Function
     || {Function, 1} <- ?MODULE:module_info(exports),
        lists:suffix("_test", atom_to_list(Function))
    ].

%% ------------------------------------------------------------------
%% Test Cases
%% ------------------------------------------------------------------

empty_test(_Config) ->
    Vec = steady_vector:new(),
    ?assert(steady_vector:is_empty(Vec)),
    ?assertEqual(0, steady_vector:size(Vec)),
    ?assertEqual(it_is_empty, steady_vector:last(Vec, it_is_empty)),
    ?assertError(badarg, steady_vector:get(0, Vec)),
    ?assertError(emptyvec, steady_vector:last(Vec)),
    ?assertEqual(not_found, steady_vector:get(1, Vec, not_found)),
    ?assertEqual(error, steady_vector:find(1, Vec)).

append_to_tail_test(_Config) ->
    Vec1 = steady_vector:append(0, steady_vector:new()),
    ?assertEqual(1, steady_vector:size(Vec1)),
    ?assertNot(steady_vector:is_empty(Vec1)),
    ?assertEqual(0, steady_vector:get(0, Vec1)),

    Vec2 = steady_vector:append(1, Vec1),
    ?assertEqual(2, steady_vector:size(Vec2)),
    ?assertEqual(0, steady_vector:get(0, Vec2)),
    ?assertEqual(1, steady_vector:get(1, Vec2)).

append_to_root_test(_Config) ->
    C = 68,
    Vec1 =
        lists:foldl(
            fun append_and_assert_element_identity/2,
            steady_vector:new(),
            lists:seq(0, C - 1)
        ),
    ?assertEqual(C, steady_vector:size(Vec1)),

    Vec2 = assert_element_identity(steady_vector:append(C, Vec1)),
    ?assertEqual(C + 1, steady_vector:size(Vec2)),
    ?assertEqual(C, steady_vector:get(C, Vec2)),

    ?assertError(badarg, steady_vector:get(C + 1, Vec2)),
    ?assertError(badarg, steady_vector:get("hello", Vec2)),
    ?assertError(badarg, steady_vector:get({1}, Vec2)).

remove_all_test(_Config) ->
    C = 1000,
    List = lists:seq(1, C),
    Vec = steady_vector:from_list(List),
    EmptyVec =
        lists:foldr(
            fun(Index, VecAcc) ->
                NewVecAcc = steady_vector:remove_last(VecAcc),
                ?assertEqual(steady_vector:to_list(NewVecAcc), lists:seq(1, Index - 1)),
                NewVecAcc
            end,
            Vec,
            List
        ),
    ?assertError(emptyvec, steady_vector:remove_last(EmptyVec)).

remove_last_tail_test(_Config) ->
    Vec1 = steady_vector:append(0, steady_vector:new()),
    Vec1_R = steady_vector:remove_last(Vec1),
    ?assertEqual(0, steady_vector:size(Vec1_R)),

    Vec2 = steady_vector:append(1, Vec1),
    Vec2_R = steady_vector:remove_last(Vec2),
    ?assertEqual(1, steady_vector:size(Vec2_R)),
    ?assertEqual(0, steady_vector:get(0, Vec2_R)).

remove_last_root_test(_Config) ->
    C = 20,
    Vec1 = lists:foldl(
        fun steady_vector:append/2,
        steady_vector:new(),
        lists:seq(0, C - 1)
    ),
    Vec2 = lists:foldl(
        fun(Index, Acc) ->
            assert_element_identity(Acc),
            ?assertEqual(Index + 1, steady_vector:size(Acc)),
            steady_vector:remove_last(Acc)
        end,
        Vec1,
        lists:seq(C - 1, 0, -1)
    ),

    ?assertEqual(0, steady_vector:size(Vec2)),
    ?assertError(emptyvec, steady_vector:remove_last(Vec2)).

set_tail_test(_Config) ->
    Vec1 = assert_element_identity(
        steady_vector:set(1, 1, steady_vector:set(0, 0, steady_vector:new()))
    ),
    ?assertEqual(2, steady_vector:size(Vec1)),

    C = 4,
    IndexSeq = lists:seq(0, C - 1),
    Vec2A = lists:foldl(
        fun steady_vector:append/2,
        steady_vector:new(),
        IndexSeq
    ),
    Vec2B = lists:foldl(
        fun(Index, Acc) ->
            steady_vector:set(Index, Index + 10, Acc)
        end,
        Vec1,
        IndexSeq
    ),

    ?assertEqual(steady_vector:size(Vec2A), steady_vector:size(Vec2B)),
    lists:foreach(
        fun(Index) ->
            ?assertEqual(steady_vector:get(Index, Vec2A), steady_vector:get(Index, Vec2B) - 10)
        end,
        IndexSeq
    ).

set_root_test(_Config) ->
    C = 20,
    IndexSeq = lists:seq(0, C - 1),
    Vec1 = lists:foldl(
        fun steady_vector:append/2,
        steady_vector:new(),
        IndexSeq
    ),
    Vec2 = lists:foldl(
        fun(Index, Acc) ->
            steady_vector:set(Index, Index + 10, Acc)
        end,
        Vec1,
        IndexSeq
    ),

    ?assertEqual(steady_vector:size(Vec1), steady_vector:size(Vec2)),
    lists:foreach(
        fun(Index) ->
            ?assertEqual(steady_vector:get(Index, Vec1), steady_vector:get(Index, Vec2) - 10)
        end,
        IndexSeq
    ),
    ?assertError(badarg, steady_vector:set(1, 1, steady_vector:new())),
    ?assertError(badarg, steady_vector:set("bla", 1, steady_vector:new())).

from_and_to_list_test(_Config) ->
    C = 1000,
    List = [{rand:uniform(), Index} || Index <- lists:seq(1, C)],

    % convert from
    Vec = steady_vector:from_list(List),
    ?assertEqual(length(List), steady_vector:size(Vec)),

    % convert to
    List2 = steady_vector:to_list(Vec),
    ?assertEqual(steady_vector:size(Vec), length(List2)),

    % all elements were kept
    _ = lists:zipwith(
        fun(Left, Right) ->
            ?assertEqual(Left, Right)
        end,
        List,
        List2
    ).

filter_test(_Config) ->
    C = 1000,
    ListFilterFun = fun({V, _Index}) -> V band 1 =:= 0 end,
    VecFilterFun = fun(Index, {_, OrigIndex} = Value) ->
        ?assertEqual(Index, OrigIndex - 1),
        ListFilterFun(Value)
    end,
    List = [{rand:uniform(1000), Index} || Index <- lists:seq(1, C)],
    Vec = steady_vector:from_list(List),
    FilteredList = lists:filter(ListFilterFun, List),
    FilteredVec = steady_vector:filter(VecFilterFun, Vec),
    steady_vector:foldl(
        fun(_Index, VecValue, [ListValue | Next]) ->
            ?assertEqual(VecValue, ListValue),
            Next
        end,
        FilteredList,
        FilteredVec
    ).

foldl_test(_Config) ->
    C = 1000,
    List = [{rand:uniform(), Index} || Index <- lists:seq(1, C)],
    Vec = steady_vector:from_list(List),
    steady_vector:foldl(
        fun(
            VecIndex,
            {VecValue, OrigVecIndex},
            [{ListValue, OrigListIndex} | Next]
        ) ->
            ?assertEqual(VecValue, ListValue),
            ?assertEqual(VecIndex, OrigVecIndex - 1),
            ?assertEqual(VecIndex, OrigListIndex - 1),
            Next
        end,
        List,
        Vec
    ).

foldr_test(_Config) ->
    C = 1000,
    List = [{rand:uniform(), Index} || Index <- lists:seq(1, C)],
    Vec = steady_vector:from_list(List),
    RevList = lists:reverse(List),
    steady_vector:foldr(
        fun(
            VecIndex,
            {VecValue, OrigVecIndex},
            [{ListValue, OrigListIndex} | Next]
        ) ->
            ?assertEqual(VecValue, ListValue),
            ?assertEqual(VecIndex, OrigVecIndex - 1),
            ?assertEqual(VecIndex, OrigListIndex - 1),
            Next
        end,
        RevList,
        Vec
    ).

foreach_test(_Config) ->
    C = 1000,
    List = [{rand:uniform(), Index} || Index <- lists:seq(1, C)],
    Vec = steady_vector:from_list(List),
    ProcDicKey = make_ref(),
    undefined = put(ProcDicKey, List),
    steady_vector:foreach(
        fun(Index, {Value, OrigIndex}) ->
            [{ListValue, ListOrigIndex} | Next] = get(ProcDicKey),
            ?assertEqual(Value, ListValue),
            ?assertEqual(Index, OrigIndex - 1),
            ?assertEqual(Index, ListOrigIndex - 1),
            put(ProcDicKey, Next)
        end,
        Vec
    ),
    erlang:put(ProcDicKey, undefined).

map_test(_Config) ->
    C = 1000,
    ListMapFun = fun({V, Index}) -> {V * 2, Index} end,
    VecMapFun = fun(Index, {_, OrigIndex} = Value) ->
        ?assertEqual(Index, OrigIndex - 1),
        ListMapFun(Value)
    end,
    List = [{rand:uniform(), Index} || Index <- lists:seq(1, C)],
    Vec = steady_vector:from_list(List),
    MappedList = lists:map(ListMapFun, List),
    MappedVec = steady_vector:map(VecMapFun, Vec),
    steady_vector:foldl(
        fun(_Index, VecValue, [ListValue | Next]) ->
            ?assertEqual(VecValue, ListValue),
            Next
        end,
        MappedList,
        MappedVec
    ).

%% ------------------------------------------------------------------
%% Internal Test Helpers
%% ------------------------------------------------------------------

append_and_assert_element_identity(Value, Vec1) ->
    Vec2 = steady_vector:append(Value, Vec1),
    assert_element_identity(Vec2).

assert_element_identity(Vec) ->
    C = steady_vector:size(Vec),

    % "randomly" use different getters
    ValidationFun =
        case C rem 3 of
            0 -> fun(Index) -> ?assertEqual(Index, steady_vector:get(Index, Vec)) end;
            1 -> fun(Index) -> ?assertEqual(Index, steady_vector:get(Index, Vec, not_found)) end;
            2 -> fun(Index) -> ?assertEqual({ok, Index}, steady_vector:find(Index, Vec)) end
        end,

    lists:foreach(ValidationFun, lists:seq(0, C - 1)),

    if
        C > 0 ->
            ?assertEqual(C - 1, steady_vector:last(Vec, empty)),
            ?assertEqual(C - 1, steady_vector:last(Vec));
        true ->
            ?assertEqual(empty, steady_vector:last(Vec, empty)),
            ?assertError(badarg, steady_vector:last(Vec))
    end,
    Vec.

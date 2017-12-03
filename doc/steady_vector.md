

# Module steady_vector #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-index">index()</a> ###


<pre><code>
index() = non_neg_integer()
</code></pre>




### <a name="type-t">t()</a> ###


__abstract datatype__: `t()`

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#append-2">append/2</a></td><td>Appends <code>Value</code> to the end of <code>Vector</code>
<code>Vector</code> must be a valid vector or a <code>{badvec,Vector}</code> error will be raised.</td></tr><tr><td valign="top"><a href="#filter-2">filter/2</a></td><td>Filters <code>Vector</code> elements using predicate <code>Fun</code>.</td></tr><tr><td valign="top"><a href="#find-2">find/2</a></td><td>Returns success-wrapped value of element in <code>Vector</code> at <code>0</code>-based <code>Index</code>,
or <code>error</code> if the index is too large.</td></tr><tr><td valign="top"><a href="#foldl-3">foldl/3</a></td><td>Calls <code>Fun(Index, Value, AccIn)</code> on successive elements of <code>Vector</code>, starting with AccIn == Acc0.</td></tr><tr><td valign="top"><a href="#foldr-3">foldr/3</a></td><td>Like <code>foldl/3</code> but <code>Vector</code> is traversed from right to left.</td></tr><tr><td valign="top"><a href="#foreach-2">foreach/2</a></td><td>Calls <code>Fun(Index, Value)</code> for each <code>Value</code> in <code>Vector</code>.</td></tr><tr><td valign="top"><a href="#from_list-1">from_list/1</a></td><td>Converts a proper <code>List</code> of elements to a <code>Vector</code> containing them in the same order.</td></tr><tr><td valign="top"><a href="#get-2">get/2</a></td><td>Returns value of element in <code>Vector</code> at <code>0</code>-based <code>Index</code>.</td></tr><tr><td valign="top"><a href="#get-3">get/3</a></td><td>Returns value of element in <code>Vector</code> at <code>0</code>-based <code>Index</code> or <code>Default</code> if <code>Index >= size(Vector)</code>.</td></tr><tr><td valign="top"><a href="#is_empty-1">is_empty/1</a></td><td>Returns <code>true</code> if <code>Vector</code> is empty, <code>false</code> otherwise.</td></tr><tr><td valign="top"><a href="#is_steady_vector-1">is_steady_vector/1</a></td><td>Returns <code>true</code> if <code>Term</code> is a <code>steady_vector</code>, <code>false</code> otherwise.</td></tr><tr><td valign="top"><a href="#last-1">last/1</a></td><td>Returns the last value of a non-empty <code>Vector</code>.</td></tr><tr><td valign="top"><a href="#last-2">last/2</a></td><td>Returns the last value <code>Vector</code> if it's not empty, <code>Default</code> otherwise.</td></tr><tr><td valign="top"><a href="#map-2">map/2</a></td><td>Maps function <code>Fun(Index, Value)</code> to all values of vector <code>Vector</code>.</td></tr><tr><td valign="top"><a href="#new-0">new/0</a></td><td>Returns an empty vector.</td></tr><tr><td valign="top"><a href="#remove_last-1">remove_last/1</a></td><td>Removes the last value of non-empty <code>Vector1</code> and returns <code>Vector2</code> with the element removed.</td></tr><tr><td valign="top"><a href="#set-3">set/3</a></td><td>Returns updated <code>Vector2</code> with element at <code>0</code>-based <code>Index</code> set to <code>Value</code>.</td></tr><tr><td valign="top"><a href="#size-1">size/1</a></td><td>Returns number of elements in <code>Vector</code>.</td></tr><tr><td valign="top"><a href="#to_list-1">to_list/1</a></td><td>Returns list with <code>Vector</code>'s elements in the same order.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="append-2"></a>

### append/2 ###

<pre><code>
append(Value, Vector1) -&gt; Vector2
</code></pre>

<ul class="definitions"><li><code>Value = term()</code></li><li><code>Vector1 = <a href="#type-t">t()</a></code></li><li><code>Vector2 = <a href="#type-t">t()</a></code></li></ul>

returns: Modified `Vector`

Appends `Value` to the end of `Vector`
`Vector` must be a valid vector or a `{badvec,Vector}` error will be raised.

__See also:__ [set/3](#set-3).

<a name="filter-2"></a>

### filter/2 ###

<pre><code>
filter(Fun, Vector1) -&gt; Vector2
</code></pre>

<ul class="definitions"><li><code>Fun = fun((Index, Value) -&gt; boolean())</code></li><li><code>Index = <a href="#type-index">index()</a></code></li><li><code>Value = term()</code></li><li><code>Vector1 = <a href="#type-t">t()</a></code></li><li><code>Vector2 = <a href="#type-t">t()</a></code></li></ul>

returns: The modified vector containing the filtered elements.

Filters `Vector` elements using predicate `Fun`.
`Fun` must be an arity-2 function or a `badarg` error will be raised.
`Vector` must be a valid vector or a `{badvec,Vector}` error will be raised.

<a name="find-2"></a>

### find/2 ###

<pre><code>
find(Index, Vector) -&gt; {ok, Value} | error
</code></pre>

<ul class="definitions"><li><code>Index = <a href="#type-index">index()</a></code></li><li><code>Vector = <a href="#type-t">t()</a></code></li><li><code>Value = term()</code></li></ul>

Returns success-wrapped value of element in `Vector` at `0`-based `Index`,
or `error` if the index is too large.
`Index` must be a non-negative integer or a `badarg` error will be raised.
`Vector` must be a valid vector or a `{badvec,Vector}` error will be raised.

__See also:__ [get/2](#get-2), [get/3](#get-3).

<a name="foldl-3"></a>

### foldl/3 ###

<pre><code>
foldl(Fun, Acc0, Vector) -&gt; AccN
</code></pre>

<ul class="definitions"><li><code>Fun = fun((Index, Value, AccIn) -&gt; AccOut)</code></li><li><code>Index = <a href="#type-index">index()</a></code></li><li><code>Value = term()</code></li><li><code>AccIn = Acc0 | AccOut</code></li><li><code>AccOut = term() | AccN</code></li><li><code>Acc0 = term()</code></li><li><code>Vector = <a href="#type-t">t()</a></code></li><li><code>AccN = term()</code></li></ul>

Calls `Fun(Index, Value, AccIn)` on successive elements of `Vector`, starting with AccIn == Acc0.
`Fun/3` must return a new accumulator, which is passed to the next call.
The function returns the final value of the accumulator. `Acc0` is returned if the vector is empty.
`Fun` must be an arity-3 function or a `badarg` error will be raised.
`Vector` must be a valid vector or a `{badvec,Vector}` error will be raised.

__See also:__ [foldr/3](#foldr-3).

<a name="foldr-3"></a>

### foldr/3 ###

<pre><code>
foldr(Fun, Acc0, Vector) -&gt; AccN
</code></pre>

<ul class="definitions"><li><code>Fun = fun((Index, Value, Acc1) -&gt; Acc2)</code></li><li><code>Index = <a href="#type-index">index()</a></code></li><li><code>Value = term()</code></li><li><code>Acc1 = Acc0 | Acc2</code></li><li><code>Acc2 = term() | AccN</code></li><li><code>Acc0 = term()</code></li><li><code>Vector = <a href="#type-t">t()</a></code></li><li><code>AccN = term()</code></li></ul>

Like `foldl/3` but `Vector` is traversed from right to left.

__See also:__ [foldl/3](#foldl-3).

<a name="foreach-2"></a>

### foreach/2 ###

<pre><code>
foreach(Fun, Vector) -&gt; ok
</code></pre>

<ul class="definitions"><li><code>Fun = fun((Index, Value) -&gt; term())</code></li><li><code>Index = <a href="#type-index">index()</a></code></li><li><code>Value = term()</code></li><li><code>Vector = <a href="#type-t">t()</a></code></li></ul>

Calls `Fun(Index, Value)` for each `Value` in `Vector`.
This function is used for its side effects and the evaluation order
is defined to be the same as the order of the elements in the vector.
`Fun` must be an arity-2 function or a `badarg` error will be raised.
`Vector` must be a valid vector or a `{badvec,Vector}` error will be raised.

<a name="from_list-1"></a>

### from_list/1 ###

<pre><code>
from_list(List) -&gt; Vector
</code></pre>

<ul class="definitions"><li><code>List = list()</code></li><li><code>Vector = <a href="#type-t">t()</a></code></li></ul>

Converts a proper `List` of elements to a `Vector` containing them in the same order.
`List` must be a list or a `{badvec,Vector}` error will be raised.

__See also:__ [to_list/1](#to_list-1).

<a name="get-2"></a>

### get/2 ###

<pre><code>
get(Index, Vector) -&gt; Value | no_return()
</code></pre>

<ul class="definitions"><li><code>Index = <a href="#type-index">index()</a></code></li><li><code>Vector = <a href="#type-t">t()</a></code></li><li><code>Value = term()</code></li></ul>

Returns value of element in `Vector` at `0`-based `Index`.
`Index` must be an integer and satisfy condition `0 =< Index =< size(Vector)` or a `badarg` error will be raised.
`Vector` must be a valid vector or a `{badvec,Vector}` error will be raised.

__See also:__ [find/2](#find-2), [get/3](#get-3).

<a name="get-3"></a>

### get/3 ###

<pre><code>
get(Index, Vector, Default) -&gt; Value | Default
</code></pre>

<ul class="definitions"><li><code>Index = <a href="#type-index">index()</a></code></li><li><code>Vector = <a href="#type-t">t()</a></code></li><li><code>Default = term()</code></li><li><code>Value = term()</code></li></ul>

Returns value of element in `Vector` at `0`-based `Index` or `Default` if `Index >= size(Vector)`.
`Index` must be a non-negative integer or a `badarg` error will be raised.
`Vector` must be a valid vector or a `{badvec,Vector}` error will be raised.

__See also:__ [find/2](#find-2), [get/2](#get-2).

<a name="is_empty-1"></a>

### is_empty/1 ###

<pre><code>
is_empty(Vector) -&gt; boolean()
</code></pre>

<ul class="definitions"><li><code>Vector = <a href="#type-t">t()</a></code></li></ul>

Returns `true` if `Vector` is empty, `false` otherwise.
`Vector` must be a valid vector or a `{badvec,Vector}` error will be raised.

__See also:__ [size/1](#size-1).

<a name="is_steady_vector-1"></a>

### is_steady_vector/1 ###

<pre><code>
is_steady_vector(Term) -&gt; boolean()
</code></pre>

<ul class="definitions"><li><code>Term = term()</code></li></ul>

Returns `true` if `Term` is a `steady_vector`, `false` otherwise.

<a name="last-1"></a>

### last/1 ###

<pre><code>
last(Vector) -&gt; Value | no_return()
</code></pre>

<ul class="definitions"><li><code>Vector = <a href="#type-t">t()</a></code></li><li><code>Value = term()</code></li></ul>

Returns the last value of a non-empty `Vector`.
If `Vector` is empty, an `emptyvec` error will be raised.
`Vector` must be a valid vector or a `{badvec,Vector}` error will be raised.

<a name="last-2"></a>

### last/2 ###

<pre><code>
last(Vector, Default) -&gt; Value | Default
</code></pre>

<ul class="definitions"><li><code>Vector = <a href="#type-t">t()</a></code></li><li><code>Default = term()</code></li><li><code>Value = term()</code></li></ul>

Returns the last value `Vector` if it's not empty, `Default` otherwise.
`Vector` must be a valid vector or a `{badvec,Vector}` error will be raised.

<a name="map-2"></a>

### map/2 ###

<pre><code>
map(Fun, Vector1) -&gt; Vector2
</code></pre>

<ul class="definitions"><li><code>Fun = fun((Index, Value1) -&gt; Value2)</code></li><li><code>Index = <a href="#type-index">index()</a></code></li><li><code>Value1 = term()</code></li><li><code>Value2 = term()</code></li><li><code>Vector1 = <a href="#type-t">t()</a></code></li><li><code>Vector2 = <a href="#type-t">t()</a></code></li></ul>

Maps function `Fun(Index, Value)` to all values of vector `Vector`.
Returns a new vector containing the mapped values.
`Vector` must be a valid vector or a `{badvec,Vector}` error will be raised.

<a name="new-0"></a>

### new/0 ###

<pre><code>
new() -&gt; Vector
</code></pre>

<ul class="definitions"><li><code>Vector = <a href="#type-t">t()</a></code></li></ul>

Returns an empty vector.

<a name="remove_last-1"></a>

### remove_last/1 ###

<pre><code>
remove_last(Vector1) -&gt; Vector2 | no_return()
</code></pre>

<ul class="definitions"><li><code>Vector1 = <a href="#type-t">t()</a></code></li><li><code>Vector2 = <a href="#type-t">t()</a></code></li></ul>

Removes the last value of non-empty `Vector1` and returns `Vector2` with the element removed.
If `Vector` is empty, an `emptyvec` error will be raised.
`Vector` must be a valid vector or a `{badvec,Vector}` error will be raised.

<a name="set-3"></a>

### set/3 ###

<pre><code>
set(Index, Value, Vector1) -&gt; Vector2 | no_return()
</code></pre>

<ul class="definitions"><li><code>Index = <a href="#type-index">index()</a></code></li><li><code>Value = term()</code></li><li><code>Vector1 = <a href="#type-t">t()</a></code></li><li><code>Vector2 = <a href="#type-t">t()</a></code></li></ul>

Returns updated `Vector2` with element at `0`-based `Index` set to `Value`.
`Index` must be an integer and satisfy condition `0 =< Index < size(Vector)`
or a `badarg` error will be raised. If `Index` equals `size(Vector)`, this
function will behave like append/2.
`Vector` must be a valid vector or a `{badvec,Vector}` error will be raised.

__See also:__ [append/2](#append-2).

<a name="size-1"></a>

### size/1 ###

<pre><code>
size(Vector) -&gt; non_neg_integer()
</code></pre>

<ul class="definitions"><li><code>Vector = <a href="#type-t">t()</a></code></li></ul>

Returns number of elements in `Vector`.
`Vector` must be a valid vector or a `{badvec,Vector}` error will be raised.

__See also:__ [is_empty/1](#is_empty-1).

<a name="to_list-1"></a>

### to_list/1 ###

<pre><code>
to_list(Vector) -&gt; list()
</code></pre>

<ul class="definitions"><li><code>Vector = <a href="#type-t">t()</a></code></li></ul>

Returns list with `Vector`'s elements in the same order.
`Vector` must be a valid vector or a `{badvec,Vector}` error will be raised.

__See also:__ [from_list/1](#from_list-1).


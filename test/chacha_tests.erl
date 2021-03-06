-module(chacha_tests).

%% Include cut BEFORE
%-compile({parse_transform, cut}).
-compile({parse_transform, chicha}).
-compile({parse_transform, chacha}).

-compile(export_all).


id(X) -> X.
pair(X, Y) -> {X, Y}.

last(Xs) ->
    chain(lists:last/1 -- Xs).

do_nothing(Xs) ->
    chain(lists:map(id/1) -- Xs).

do_nothing2(Xs) ->
    F = chain(lists:map(id/1)),
    F(Xs).

do_nothing_too(Xs) ->
    chain(id/1 -- Xs).

do_nothing_too2(Xs) ->
    chain(id -- Xs).

do_nothing_too3(Xs) ->
    Id = fun id/1,
    chain(Id -- Xs).

do_nothing_too4(Xs) ->
    chain(fun(X) -> X end -- Xs).

do_nothing_too5(Xs) ->
   chain(fun id/1 -- Xs).


simple_foldl_before() ->
    fun(Acc, Xs) ->
        lists:foldl(fun pair/2, Acc, Xs)
        end.

simple_foldl(X) ->
    chain(lists:foldl(pair/2, X)).


head([H|_]) -> H.
is_upper(X) when X >= $A, X =< $Z -> true.

%% TODO: This function is too trivial.
words(S) -> string:tokens(S, " ").


case1_before() ->
    fun(Str) ->
        length(lists:filter(fun([H|_]) -> is_upper(H) end, words(Str)))
        end.

case1() ->
    chain(length, lists:filter(chain(is_upper, head)), words).



is_space(X) -> X =:= $ .
with_first(F, [H|T]) -> [F(H)|T].


case2_before() ->
    lists:map(fun(X) -> 
                [H|T] = lists:dropwhile(fun is_space/1, X), 
                [string:to_upper(H)|T] 
                end, 
              [" a", "f", "   ee"]).


case2() ->
    chain(lists:map(chain(with_first(string:to_upper/1), 
                          lists:dropwhile(is_space/1))) 
          -- [" a", "f", "   ee"]).


case3() ->
    chain(io:write(user) -- mess).

case3_alt1() ->
    F = fun(X, Y) -> FF = chain(io:write(X)), FF(Y) end,
    F(user, mess).


sum(Xs) ->
    chain(lists:foldl(erlang:'+'/2, 0) -- Xs).


con(H, T) -> [H|T].

append(List) ->
    %% It is [1,2,3|List].
    chain(con(1), con(2), con(3) -- List).


appendr(List) ->
    %% It is [1,2,3|List].
    chainr(con(1), con(2), con(3) -- List).

appendl(List) ->
    %% It is [3,2,1|List].
    chainl(con(1), con(2), con(3) -- List).


-include_lib("eunit/include/eunit.hrl").

-ifdef(TEST).

case1_test_() ->
    [ ?_assertEqual((case1())("Hello, Mike! Hello, Joe!"), 4)
    , ?_assertEqual((case1_before())("Hello, Mike! Hello, Joe!"), 4)
    ].

case2_test_() ->
    [ ?_assertEqual(case2(), ["A", "F", "Ee"])
    , ?_assertEqual(case2_before(), ["A", "F", "Ee"])
    ].

last_test_() ->
    [ ?_assertEqual(last("Test"), $t)
    ].

sum_test_() ->
    [ ?_assertEqual(sum([1,2,3]), 6)
    ].

do_nothing_test_() ->
    [ ?_assertEqual(do_nothing([1,2,3]), [1,2,3])
    , ?_assertEqual(do_nothing2([1,2,3]), [1,2,3])
    , ?_assertEqual(do_nothing_too([1,2,3]), [1,2,3])
    , ?_assertEqual(do_nothing_too2([1,2,3]), [1,2,3])
    , ?_assertEqual(do_nothing_too3([1,2,3]), [1,2,3])
    , ?_assertEqual(do_nothing_too4([1,2,3]), [1,2,3])
    , ?_assertEqual(do_nothing_too5([1,2,3]), [1,2,3])
    ].


append_test_() ->
    [ ?_assertEqual(append([]), [1,2,3])
    , ?_assertEqual(append([4,5]), [1,2,3,4,5])
    ].


appendr_test_() ->
    [ ?_assertEqual(appendr([]), [1,2,3])
    , ?_assertEqual(appendr([4,5]), [1,2,3,4,5])
    ].


appendl_test_() ->
    [ ?_assertEqual(appendl([]), [3,2,1])
    , ?_assertEqual(appendl([4,5]), [3,2,1,4,5])
    ].

-endif.

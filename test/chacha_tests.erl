-module(chacha_tests).

-compile({parse_transform, chacha}).
-compile({parse_transform, cut}).

-compile(export_all).


id(X) -> X.
pair(X, Y) -> {X, Y}.

%%lsts:map/2.
%%+id.
%%hof(Test, _St)

chain(VarFun, 


 do_nothing(Xs) ->
     chain(lists:map(id/1) -- Xs).

do_nothing_alt(Xs) ->
    chain(lists:map(id/1) -- Xs).


simple_foldl_before() ->
    fun(Acc, Xs) ->
        lists:foldl(fun pair/2, Acc, Xs)
        end.

simple_foldl(X) ->
    chain(lists:foldl(pair/2, X)).


do_nothing2(Xs) ->
    F = chain(lists:map(id/1)),
    F(Xs).

t() -> tt(_).

case3() ->
    chain(io:write(user) -- mess).

case3_alt1() ->
    F = fun(X, Y) -> FF = chain(io:write(X)), FF(Y) end,
    F(user, mess).


head([H|_]) -> H.
is_upper(X) when X >= $A, X =< $Z -> true.
words(S) -> string:tokens(S, " ").


case1_before() ->
    length(filter(fun([H|_]) -> fun is_upper/1 end, words("Hi all."))).

case1() ->
    chain(length, filter(chain(is_upper, head)), words -- "Hi all.").



is_space(X) -> X =:= $ .

case2_before() ->
    lists:map(fun(X) -> lists:dropwhile(fun is_space/1, X) end, [" a", "f", "   e"]).

case2() ->
    chain(lists:map(chain(lists:dropwhile, is_space)) -- [" a", "f", "   e"]).


case3_before() ->
    io:write(user, mess).

beetween_trans_before(AppNode) ->
    Pos = erl_syntax:get_pos(AppNode),
    %% Call it fore all new nodes.
    New = fun(NewNode) -> erl_syntax:set_pos(NewNode, Pos) end,
    %% Extract arguments of the `in' function.
    [SubjectForm, FromForm, ToForm] =
        erl_syntax:application_arguments(AppNode),
    GtEqOp = New(erl_syntax:operator('>=')),
    LoEqOp = New(erl_syntax:operator('=<')),
    AndOp  = New(erl_syntax:operator('andalso')),
    Exp1 = New(erl_syntax:infix_expr(SubjectForm, GtEqOp, FromForm)),
    Exp2 = New(erl_syntax:infix_expr(SubjectForm, LoEqOp, ToForm)),
    Exp3 = New(erl_syntax:infix_expr(Exp1, AndOp, Exp2)),
    GuardAST = New(erl_syntax:parentheses(Exp3)),
    erl_syntax:revert(GuardAST).

beetween_trans(AppNode) ->
    Pos = erl_syntax:get_pos(AppNode),
    %% Call it fore all new nodes.
    %% Use cut from erlando.
 %  New = erl_syntax:set_pos(_, Pos),
    New = fun(Node) -> erl_syntax:set_pos(Node, Pos) end,

    [SubjectForm, FromForm, ToForm] =
        erl_syntax:application_arguments(AppNode),

    Op = chain(New, erl_syntax:operator),
    WithSubject = fun(LitOp, Form) -> 
        chain(New, erl_syntax:infix_expr(SubjectForm, Op(LitOp)) -- Form)
        end,

    chain(erl_syntax:revert
        , New, erl_syntax:parentheses
        , New  
        %% It is a simple term
        -- erl_syntax:infix_expr(WithSubject('>=', FromForm)
                                , Op('andalso')
                                , WithSubject('=<', ToForm)).

append_value_rec_before(Action, SlotId, Value, Ignore, S2T, Bin1) ->
    Bin2 = append_type(action_type(Action), Bin1),
    Bin3 = append_slot(SlotId, Bin2),
    Bin4 = append_value(SlotId, Value, S2T, Bin3),
    append_boolean(Ignore, Bin4).


append_value_rec_before_seq(Action, SlotId, Value, Ignore, S2T, Bin@) ->
    Bin@ = append_type(action_type(Action), Bin@),
    Bin@ = append_slot(SlotId, Bin@),
    Bin@ = append_value(SlotId, Value, S2T, Bin@),
    Bin@ = append_boolean(Ignore, Bin@),
    Bin@.

append_value_rec_before_nested(Action, SlotId, Value, Ignore, S2T, Bin) ->
    append_boolean(Ignore, 
                   append_value(SlotId, Value, S2T, 
                                append_slot(SlotId, 
                                            append_type(action_type(Action), 
                                                        Bin)))).


append_value_rec(Action, SlotId, Value, Ignore, S2T, Bin) ->
    chain(
     append_boolean(Ignore)
    ,append_value(SlotId, Value, S2T)
    ,append_slot(SlotId)
    ,append_type(action_type(Action))).


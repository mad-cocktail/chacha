-module(chacha_tests).

-compile({parse_transform, perry}).
-compile(export_all).


id(X) -> X.

do_nothing(Xs) ->
    chain + lists:map(id) ++ Xs.

do_nothing2(Xs) ->
    F = chain * 1 + lists:map(id),
    F(Xs).

st() -> tt(_).

%case3() ->
%    chain + io:write(_) ++ user + mess.

%case3_alt1() ->
%    F = chain*2 + io:write(_),
%    F(user, mess).
%
%case3_alt2() ->
%    chain + 1 * io:write ++ user + mess.
%
%case3_alt3() ->
%    chain - (io:write ++ user + mess).
%
%
%head([H|_]) -> H.
%is_upper(X) when X >= $A, X =< $Z -> true.
%words(S) -> string:tokens(S, " ").
%
%
%case1_before() ->
%    length(filter(fun([H|_]) -> fun is_upper/1 end, words("Hi all."))).
%
%case1() ->
%    CapCount = chain + length + filter (is_upper + head) + words,
%    CapCount("Hi all.").
%
%
%case6_before() ->
%    fun(X) -> is_upper(head(X)) end.
%
%case6() ->
%    chain*1 + is_upper + head.
%
%case6_alt() ->
%    chain*1 - is_upper(_) - head(_).
%
%case6_alt3() ->
%    chain*1 - is_upper(head ++ _).
%
%is_space(X) -> X =:= $ .
%
%case2_before() ->
%    lists:map(fun(X) -> lists:dropwhile(fun is_space/1, X) end, [" a", "f", "   e"]).
%
%case2() ->
%    chain + lists:map(lists:dropwhile + is_space) ++ [" a", "f", "   e"].
%
%
%case3_before() ->
%    io:write(user, mess).
%
%beetween_trans_before(AppNode) ->
%    Pos = erl_syntax:get_pos(AppNode),
%    %% Call it fore all new nodes.
%    New = fun(NewNode) -> erl_syntax:set_pos(NewNode, Pos) end,
%    %% Extract arguments of the `in' function.
%    [SubjectForm, FromForm, ToForm] =
%        erl_syntax:application_arguments(AppNode),
%    GtEqOp = New(erl_syntax:operator('>=')),
%    LoEqOp = New(erl_syntax:operator('=<')),
%    AndOp  = New(erl_syntax:operator('andalso')),
%    Exp1 = New(erl_syntax:infix_expr(SubjectForm, GtEqOp, FromForm)),
%    Exp2 = New(erl_syntax:infix_expr(SubjectForm, LoEqOp, ToForm)),
%    Exp3 = New(erl_syntax:infix_expr(Exp1, AndOp, Exp2)),
%    GuardAST = New(erl_syntax:parentheses(Exp3)),
%    erl_syntax:revert(GuardAST).
%
%beetween_trans(AppNode) ->
%    Pos = erl_syntax:get_pos(AppNode),
%    %% Call it fore all new nodes.
%    New = chain*1 + erl_syntax:set_pos(_Node) ++ _Node + Pos,
%    [SubjectForm, FromForm, ToForm] =
%        erl_syntax:application_arguments(AppNode),
%    Op = chain*1 + New + erl_syntax:operator,
%    WithSubject = chain*2 %% Operator, Node
%                  + New + erl_syntax:infix_expr(_SubjectForm, Op + _)
%                  ++ SubjectForm,
%    chain
%    + erl_syntax:revert
%    + New + erl_syntax:parentheses
%    + New - erl_syntax:infix_expr(WithSubject ++ '>=' + FromForm,
%                                  Op ++ 'andalso',
%                                  WithSubject ++ '=<' + ToForm).
%
%%         + New + erl_syntax:infix_expr
%%               ( WithSubject ++ '>=' + FromForm
%%               , Op ++ 'andalso')
%%               + (WithSubject ++ '=<') ++ ToForm.
%
%%         + New + erl_syntax:infix_expr
%%               (2 * WithSubject,
%%                1 * Op,
%%                2 * WithSubject). ++ '>=' + FromForm + 'andalso' + '=<' + ToForm
%
%append_value_rec_before(Action, SlotId, Value, Ignore, S2T, Bin1) ->
%    Bin2 = append_type(action_type(Action), Bin1),
%    Bin3 = append_slot(SlotId, Bin2),
%    Bin4 = append_value(SlotId, Value, S2T, Bin3),
%    append_boolean(Ignore, Bin4).
%
%
%append_value_rec_before_seq(Action, SlotId, Value, Ignore, S2T, Bin@) ->
%    Bin@ = append_type(action_type(Action), Bin@),
%    Bin@ = append_slot(SlotId, Bin@),
%    Bin@ = append_value(SlotId, Value, S2T, Bin@),
%    Bin@ = append_boolean(Ignore, Bin@),
%    Bin@.
%
%append_value_rec_before_nested(Action, SlotId, Value, Ignore, S2T, Bin) ->
%    append_boolean(Ignore, 
%                   append_value(SlotId, Value, S2T, 
%                                append_slot(SlotId, 
%                                            append_type(action_type(Action), 
%                                                        Bin)))).
%
%
%append_value_rec(Action, SlotId, Value, Ignore, S2T, Bin) ->
%    chain
%    + (append_boolean ++ Ignore)
%    + (append_value   ++ SlotId + Value + S2T)
%    + (append_slot    ++ SlotId)
%% or
%%   - (append_slot    ++ SlotId, _)
%    + append_type(action_type ++ Action)
%    ++ Bin.
%
%append_value_rec_alt2(Action, SlotId, Value, Ignore, S2T, Bin) ->
%    chain
%    + (append_boolean ++ Ignore)
%    + (append_slot    ++ SlotId)
%    + (append_value   ++ SlotId + Value + S2T)
%    - (append_type(action_type ++ Action) ++ Bin).
%%   + append_type(action_type ++ Action) ++ Bin.
%%%        minus means to skip the last argument.
%
%
%%% N = 1, 3, 1 is not arity, but a count of skipped elements.
%%% For example, Fun = append_slot/2, N = 1, the second argument is Bin@.
%append_value_rec_alt(Action, SlotId, Value, Ignore, S2T, Bin) ->
%    chain
%    + 1 * append_boolean
%    + 3 * append_value
%    + 1 * append_slot
%    + append_type(action_type ++ Action)
%    ++ Ignore  + SlotId + Value + S2T +  SlotId + Bin.
%
%
%%% Syntax:
%%% chain Funs ++ Terms
%%% Funs  = + Fun
%%% Terms = + Term
%%%
%%% chain will be evaluated immediately.
%%% chain * Arity, when Arity is non_neg_integer() creates a function with arity Arity.

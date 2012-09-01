Chain operator for Erlang
=========================

The main goal of this parse transform is to make the Erlang syntax more evil.

**License**: MIT

**Author**: Uvarov Michael (freeakk@gmail.com)


.. image:: https://secure.travis-ci.org/freeakk/chacha.png?branch=master
    :alt: Build Status
    :target: http://travis-ci.org/freeakk/chacha


Syntax
======

f(g(x)) is

- in mathematics : f ∘ g (x)
- in haskell : ( f . g ) (x)
- in erlang: f(g(X)).
- here: chain(f, g -- X).


- in erlang: ``[_, X|_] = Xs.``
- in erlang (again): ``head(tail(Xs)).``
- here: ``chain(head, tail -- Xs).``


Creating a higher-order function (HOF).

- in erlang: ``F = fun([_, X|_]) -> X end.``
- here: ``F = chain(head, tail).``


Sum example
-----------

Haskell::

    sum = foldr (+) 0

In Erlang::

    sum() -> fun(Xs) -> lists:foldl(fun erlang:'+'/2, 0, Xs) end.

Here::

    sum() -> chain(lists:foldl(erlang:'+'/2 ++ 0)).
    %% Run it
    (sum())([1,2,3]).
    6



The count of titled words
-------------------------

Haskell::
    
    length . filter(is_upper . head) . words

Erlang::

    -spec words(string()) -> [non_empty_string()].
    ...
    fun(Str) -> length([X || [X|_] <- words(Str), is_upper(X)]).

Here::

    F = chain(length, filter(chain(is_upper, head)), words),
    F("Hello, Mike! Hello, Joe!").
    4


Real World Example 1
--------------------


Before::

    beetween_trans(AppNode) ->
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


After::

    beetween_trans(AppNode) ->
        Pos = erl_syntax:get_pos(AppNode),
        %% Call it for all new nodes.
        %% Use cut from erlando.
     %  New = erl_syntax:set_pos(_, Pos),
        New = fun(Node) -> erl_syntax:set_pos(Node, Pos) end,

        [SubjectForm, FromForm, ToForm] =
            erl_syntax:application_arguments(AppNode),

        Op = chain(New, erl_syntax:operator),

        %% Need an another parse transform here.
        WithSubject = fun(LitOp, Form) -> 
            chain(New, erl_syntax:infix_expr(SubjectForm, Op(LitOp)) -- Form)
            end,

        chain(erl_syntax:revert, New, erl_syntax:parentheses, New  
            %% It is a simple term
            -- erl_syntax:infix_expr(WithSubject('>=', FromForm)
                                    , Op('andalso')
                                    , WithSubject('=<', ToForm))).


Real World Example 2
--------------------

Before::

    append_value_rec_before(Action, SlotId, Value, Ignore, S2T, Bin1) ->
        Bin2 = append_type(action_type(Action), Bin1),
        Bin3 = append_slot(SlotId, Bin2),
        Bin4 = append_value(SlotId, Value, S2T, Bin3),
        append_boolean(Ignore, Bin4).

Using seqbind::

    append_value_rec_before_seq(Action, SlotId, Value, Ignore, S2T, Bin@) ->
        Bin@ = append_type(action_type(Action), Bin@),
        Bin@ = append_slot(SlotId, Bin@),
        Bin@ = append_value(SlotId, Value, S2T, Bin@),
        Bin@ = append_boolean(Ignore, Bin@),
        Bin@.

Using nested calls::

    append_value_rec_before_nested(Action, SlotId, Value, Ignore, S2T, Bin) ->
        append_boolean(Ignore, 
                       append_value(SlotId, Value, S2T, 
                                    append_slot(SlotId, 
                                                append_type(action_type(Action), 
                                                            Bin)))).

Using the chain operator::
    .. code-block:: erlang

    append_value_rec(Action, SlotId, Value, Ignore, S2T, Bin) ->
        chain(
         append_boolean(Ignore)
        ,append_value(SlotId, Value, S2T)
        ,append_slot(SlotId)
        ,append_type(action_type(Action)) -- Bin).


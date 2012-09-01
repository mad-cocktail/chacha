-module(chacha).
-export([parse_transform/2]).

-record(state, {}).


parse_transform(Forms, _Options) ->
    F = fun visitor/2,
    X = [element(1, preorder(F, initial_state(Tree), Tree)) || Tree <- Forms],
%   io:format(user, "Before:\t~p\n\nAfter:\t~p\n", [Forms, X]),
    X.

initial_state(_FormTree) ->
    #state{}.


left(X) ->
    erl_syntax:infix_expr_left(X).

right(X) ->
    erl_syntax:infix_expr_right(X).


-spec op_name(Tree) -> Op when
    Tree :: erl_syntax:syntaxTree(),
    Op :: string().
op_name(Tree) ->
    case node_type(Tree) of
        prefix_expr ->
            op_name(erl_syntax:prefix_expr_operator(Tree));
        infix_expr ->
            op_name(erl_syntax:infix_expr_operator(Tree));
        operator ->
            erl_syntax:operator_literal(Tree)
    end.


node_type(X) ->
    erl_syntax:type(X).


handle_chain(ChainArgs, ChainParamVar) when is_list(ChainArgs) ->
%   io:format(user,
%             "ChainArgs: ~p~n ChainParamVar: ~p~n",
%             [ChainArgs, ChainParamVar]),
    lists:foldr(fun handle_element/2, ChainParamVar, ChainArgs).


handle_element(Tree, Prev) ->
    push_application_argument(Prev, to_application(Tree)).


%% @doc Add an argument Arg as a last argument of the App.
push_application_argument(Arg, App) ->
    Args = erl_syntax:application_arguments(App),
    Op   = erl_syntax:application_operator(App),
    copy_pos(Arg, erl_syntax:application(Op, Args ++ [Arg])).


%% @doc Convert something into a function call.
to_application(Tree) ->
%   application
    case node_type(Tree) of
        application -> Tree;
        infix_expr  -> infix_expr_to_application(Tree);
        _           -> erl_syntax:application(Tree, [])
    end.


infix_expr_to_application(Tree) ->
    L = left(Tree),
    R = right(Tree),
    case {node_type(L), op_name(Tree), node_type(R)} of
        %% chain(id/1).
        {atom, "/", integer} ->  
            1 = erl_syntax:integer_value(R),
            erl_syntax:application(L, []);
        %% chain(lists:last/1).
        {module_qualifier, "/", integer} ->  
            1 = erl_syntax:integer_value(R),
            erl_syntax:application(L, [])
    end.


is_chain_with_param(ChainArgs) ->
    Last = lists:last(ChainArgs),
    node_type(Last) =:= infix_expr andalso op_name(Last) =:= "--".


extract_chain_param(ChainArgs) ->
    Last = lists:last(ChainArgs),
    LastFun = left(Last),
    ParamVar = right(Last),
    {replace_last_element(LastFun, ChainArgs), ParamVar}.

%% @doc Set `X' as a last element of `Xs'.
replace_last_element(X, Xs) ->
    [_Last|XsR] = lists:reverse(Xs),
    lists:reverse(XsR, [X]).


visitor(Tree, State) ->
    IsChain = is_chain(Tree),
    Skip = {Tree, State},
    if
    IsChain ->
        ChainArgs = erl_syntax:application_arguments(Tree),
        ParamExists = is_chain_with_param(ChainArgs),
        if ParamExists -> 
            {ChainArgs2, ChainParamVar} = extract_chain_param(ChainArgs),
            Tree2 = handle_chain(ChainArgs2, ChainParamVar),
            {Tree2, State};
        true ->
            %% handle hof chain
            %% Use random, because of the Erlang's warning about shadowing.
            UniqueVarName = list_to_atom("ChainParam" ++
                                         integer_to_list(random:uniform(1000))),
            ChainParamVar = copy_pos(Tree, erl_syntax:variable( UniqueVarName)),
            Tree2 = hof1(ChainParamVar, handle_chain(ChainArgs, ChainParamVar)),
            {Tree2, State}
        end;
    true -> Skip
    end.


%% @doc Create `fun(Arg) -> Body end'.
hof1(Arg, Body) ->
    C1 = erl_syntax:clause([Arg], none, [Body]),
    erl_syntax:fun_expr([C1]).


%% @doc Copy positional information from `From' to `To'.
-spec copy_pos(From, To) -> To when
    From :: erl_syntax:syntaxTree(),
    To :: From.

copy_pos(From, To) ->
    Pos = erl_syntax:get_pos(From),
    erl_syntax:set_pos(To, Pos).


-spec is_chain(Tree) -> boolean() when 
    Tree :: erl_syntax:syntaxTree().

is_chain(Tree) ->
    is_funtion_call(chain, Tree).


-spec is_funtion_call(FunName, Tree) -> boolean() when 
    FunName :: atom(),
    Tree :: erl_syntax:syntaxTree().

is_funtion_call(FunName, Tree) ->
    node_type(Tree) =:= application
    andalso always(Op = erl_syntax:application_operator(Tree))
    andalso node_type(Op) =:= atom
    andalso erl_syntax:atom_value(Op) =:= FunName.

-spec always(term()) -> true.
always(_) -> true.


%% @doc This function is like `lists:foldl/3'.
-spec preorder(F, State, Form) -> {Form, State} when
    F :: fun((Form, State) -> {Form, State}),
    State :: term(),
    Form :: erl_syntax:syntaxTree().
preorder(F, State, Form) ->
    Skip = {Form1, State1} = F(Form, State),
    case erl_syntax:subtrees(Form1) of
    [] ->
        Skip;
    List ->
        {Groups, State2} = lists:mapfoldl(handle_group(F), State1, List),
        Tree2 = erl_syntax:update_tree(Form1, Groups),
        Form2 = erl_syntax:revert(Tree2),
        {Form2, State2}
    end.


handle_group(F) ->
    fun(Group, State) -> 
        FF = fun(Subtree, State2) -> preorder(F, State2, Subtree) end,
        lists:mapfoldl(FF, State, Group)
        end.


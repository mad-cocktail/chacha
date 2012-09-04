-module(chacha).
-export([parse_transform/2]).



parse_transform(Forms, _Options) ->
    F = fun(X) -> erl_syntax:revert(visitor(X)) end,
    X = [erl_syntax_lib:map(F, Tree) || Tree <- Forms],
%   io:format(user, "Before:\t~p\n\nAfter:\t~p\n", [Forms, X]),
    X.


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


%% ChainArgs = [g,v], not reversed. use foldr.
%% g(v(ChainParamVar))
handle_chain(right, ChainArgs, ChainParamVar) when is_list(ChainArgs) ->
    %% Right-to-left order, right associativity.
    lists:foldr(fun handle_element/2, ChainParamVar, ChainArgs);
handle_chain(left,  ChainArgs, ChainParamVar) when is_list(ChainArgs) ->
    %% Left-to-right associativity, left associativity.
    lists:foldl(fun handle_element/2, ChainParamVar, ChainArgs).


-spec handle_element(Tree, Prev) -> Tree when
    Tree :: erl_syntax:syntaxTree(),
    Prev :: Tree.

handle_element(Tree, Prev) ->
    push_application_argument(Prev, to_application(Tree)).


%% @doc Add an argument Arg as a last argument of the App.
-spec push_application_argument(Arg, App) -> App when
    Arg :: erl_syntax:syntaxTree(),
    App :: erl_syntax:syntaxTree().

push_application_argument(Arg, App) ->
    Args = erl_syntax:application_arguments(App),
    Op   = erl_syntax:application_operator(App),
    copy_pos(Arg, erl_syntax:application(Op, Args ++ [Arg])).


%% @doc Convert something into a function call.
-spec to_application(Tree) -> Tree when
    Tree :: erl_syntax:syntaxTree().

to_application(Tree) ->
%   application
    case node_type(Tree) of
        application -> Tree;
        infix_expr  -> infix_expr_to_application(Tree);
        _           -> erl_syntax:application(Tree, [])
    end.


%% @doc Call something.
-spec infix_expr_to_application(Tree) -> Tree when
    Tree :: erl_syntax:syntaxTree().

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


-spec is_chain_with_param(ChainArgs) -> boolean() when
    ChainArgs :: erl_syntax:syntaxTree().

is_chain_with_param(ChainArgs) ->
    Last = lists:last(ChainArgs),
    node_type(Last) =:= infix_expr andalso op_name(Last) =:= "--".


-spec extract_chain_param(ChainArgs) -> {ChainArgs, Param} when
    ChainArgs :: erl_syntax:syntaxTree(),
    Param :: erl_syntax:syntaxTree().

extract_chain_param(ChainArgs) ->
    Last = lists:last(ChainArgs),
    LastFun = left(Last),
    ParamVar = right(Last),
    {replace_last_element(LastFun, ChainArgs), ParamVar}.


%% @doc Set `X' as a last element of `Xs'.
-spec replace_last_element(term(), [term()]) -> [term()].

replace_last_element(X, Xs) ->
    [_Last|XsR] = lists:reverse(Xs),
    lists:reverse(XsR, [X]).


-spec visitor(Tree) -> Tree when
    Tree :: erl_syntax:syntaxTree().

visitor(Tree) ->
    FunName = function_name(Tree),
    IsChain = is_chain(FunName),
    if
    IsChain ->
        ChainArgs = erl_syntax:application_arguments(Tree),
        ParamExists = is_chain_with_param(ChainArgs),
        Assoc = chain_associativity(FunName),
        if ParamExists -> 
            {ChainArgs2, ChainParamVar} = extract_chain_param(ChainArgs),
            handle_chain(Assoc, ChainArgs2, ChainParamVar);
        true ->
            %% handle hof chain
            %% Use random, because of the Erlang's warning about shadowing.
            UniqueVarName = list_to_atom("ChainParam" ++
                                         integer_to_list(random:uniform(1000))),
            ChainParamVar = copy_pos(Tree, erl_syntax:variable( UniqueVarName)),
            hof1(ChainParamVar, handle_chain(Assoc, ChainArgs, ChainParamVar))
        end;
    true -> Tree
    end.


%% @doc Create `fun(Arg) -> Body end'.
-spec hof1(Arg, Body) -> Fun when
    Arg  :: erl_syntax:syntaxTree(),
    Body :: erl_syntax:syntaxTree(),
    Fun  :: erl_syntax:syntaxTree().

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

%% chain == chainr
is_chain(FunName) ->
    lists:member(FunName, [chain, chainr, chainl]).


chain_associativity(chainl) -> left;
chain_associativity(_chain) -> right.


-spec function_name(Tree) -> FunName when 
    FunName :: atom(),
    Tree :: erl_syntax:syntaxTree().

function_name(Tree) ->
    case node_type(Tree) of
        application ->
            Op = erl_syntax:application_operator(Tree),
            case node_type(Op) of
                atom -> erl_syntax:atom_value(Op);
                _    -> undefined
            end;
        _ -> undefined
    end.


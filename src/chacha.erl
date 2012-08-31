-module(chacha).
-export([parse_transform/2]).

-record(chain, {body_node, args_node, args_nodes}).
-record(state, {variables}).


parse_transform(Forms, _Options) ->
    io:format(user, "Before:\t~p\n\n", [Forms]),
    F = fun visitor/2,
    X = [element(1, preorder(F, initial_state(Tree), Tree)) || Tree <- Forms],
    [io:format(user, "~p~n", [tree_to_string(Tree)]) || Tree <- Forms],
%   io:format(user, "Before:\t~p\n\nAfter:\t~p\n", [Forms, X]),
    Forms.

initial_state(FormTree) ->
    #state{variables = erl_syntax_lib:variables(FormTree)}.

is_chain("chain /" ++ _ ) -> true;
is_chain("chain +" ++ _ ) -> true;
is_chain(_) -> false.

-spec tree_to_string(erl_syntax:syntaxTree()) -> string().
tree_to_string(Tree) -> erl_prettypr:format(Tree).

%-spec string_to_tokens(string()) -> erl_scan:tokens().
%string_to_tokens(Str) ->
%    {ok, Tokens, _EndLocation} = erl_scan:string(Str),
%    Tokens.


left(X) ->
    erl_syntax:infix_expr_left(X).

right(X) ->
    erl_syntax:infix_expr_right(X).


-spec op_name(Tree) -> Op when
    Tree :: erl_syntax:syntaxTree(),
    Op :: string().
op_name(Tree) ->
    Type = node_type(Tree),
    case Type of
        prefix_expr ->
            op_name(erl_syntax:prefix_expr_operator(Tree));
        infix_expr ->
            op_name(erl_syntax:infix_expr_operator(Tree));
        operator ->
            erl_syntax:operator_literal(Tree)
    end.

node_type(X) ->
    erl_syntax:type(X).


%% @doc Split a tree of nodes, joined with the `Op' operator.
%%
%% Example:
%% `Op' = "+"
%% Source string: Node1 + Node2 + Node3
%% Result: [Node1, "+", Node2, "+", Node3].
-spec tokenize(Tree) -> [Node|Op] when
    Tree :: erl_syntax:syntaxTree(),
    Node :: Tree,
    Op :: string().
tokenize(Tree) ->
    Type = node_type(Tree),
    case Type of
        prefix_expr ->
            Op = op_name(Tree),
            Right = tokenize(erl_syntax:prefix_expr_argument(Tree)),
            [Op] ++ Right;
        infix_expr ->
            Op = op_name(Tree),
            Left  = tokenize(left(Tree)),
            Right = tokenize(right(Tree)),
            Left ++ [Op] ++ Right;
        _OtherType ->
            [Tree] %% Latest right node
    end.


handle_args(Tree) ->
    OpAndNodeList = tokenize(Tree),
    {Ops, Nodes} = unzip_list(OpAndNodeList),
    [erlang:error({bad_operator, Op}) || Op <- Ops, Op =/= "+"],
    Nodes.


%% @doc Split `[Node, Op, Node, Op, Node]'.
-spec unzip_list([Node|Op]) -> {[Op], [Node]} when
    Node :: erl_syntax:syntaxTree(),
    Op :: string().

unzip_list([H|T]) ->
    unzip_list(T, [], [H]).

unzip_list([Op, Node|Xs], Ops, Nodes) ->
    unzip_list(Xs, [Op|Ops], [Node|Nodes]);
unzip_list([], Ops, Nodes) ->
    {lists:reverse(Ops), lists:reverse(Nodes)}.



expected_type(Type, Node) ->
    case node_type(Node) of
        Type -> ok;
        BadType -> erlang:error({badnodetype, Type, BadType, Node})
    end. 


%% @doc Handle "chain + Funs ++ Args".
handle_chain(Node) ->
    expected_type(infix_expr, Node),
    case op_name(Node) of
        "++" ->
            Left  = left(Node),
            Right = right(Node),
            #chain{body_node  = Left,
                   args_node  = Right,
                   args_nodes = handle_args(Right)};
        _ ->
            #chain{body_node = Node, args_nodes = []}
    end.


%% @doc Maybe decode arity.
read_body_arity(["/", ArityNode|Ts]) ->
    expected_type(integer, ArityNode),
    Arity = erl_syntax:integer_value(ArityNode),
    {Arity, Ts};
read_body_arity(Ts) ->
    {undefined, Ts}.


read_minus(["-" | Ts]) -> {true,  Ts};
read_minus(Ts)         -> {false, Ts}.

read_funtion_arity([Left, "/", Right|Ts]) ->
    case {node_type(Left), node_type(Right)} of
        {_, integer} -> {Left, erl_syntax:integer_value(Right), Ts}; 
        %% Fun / 1
        %% 1 / Fun
        {integer, _} -> {Right, erl_syntax:integer_value(Left), Ts}
    end;
read_funtion_arity([FunWithoutArity|Ts]) ->
    {FunWithoutArity, 1, Ts}.

decode_fun_definition(Node) ->
    case node_type(Node) of
        parentheses -> 
            decode_parenthess_function(Node);
        _Type -> 
            {undecoded, _Type, Node}
    end.

-record(function, {name, arity, funs, args}).
decode_parenthess_function(Node) ->
    Body = erl_syntax:parentheses_body(Node),
    %% + Node +
    %% + (Body) +
    %% Body = FunNameAndArity + F1 + F2 + _Skip ++ A1 + A2 + A3
    {NameAndFuns, Args} = split_with("++", tokenize(Body)),
    {FunName, FunArity, Funs} = read_funtion_arity(NameAndFuns),
    #function{name = FunName, arity = FunArity, funs = Funs, args = Args}.

%   %% Replace don't care vars of the Funs list with Args values.
%   %% If the count of don't care vars > the count of Args, then
%   %% ignore the tail.
%   %% Example:
%   %% Funs = _, _, _
%   %% Args = 1
%   %% Funs2 = 1, _, _
%   {Funs2, Args2} = insert_missed_args(Funs, Args),

%   %% Variables from Params2 (it is a tail) will be used in the next functions.
%   {Funs3, Params2} = insert_missed_args(Funs2, Params),


split_with(X, Ys) ->
    lists:splitwith(fun(Y) -> Y =:= X end, Ys).

split_all_with(_, []) -> [];
split_all_with(X, Ys) ->
    {Group, Ys2} = split_with(X, Ys),
    [Group | split_all_with(X, Ys2)].


read_function(["+" | Ts]) ->
    read_function(Ts);
read_function([]) ->
    eof;
read_function(Ts) ->
    {NoPassLastArg, Ts2} = read_minus(Ts),
    {Fun, Arity, Ts3} = read_funtion_arity(Ts2),
    decode_fun_definition(Fun).


handle_functions(Tokens) when is_list(Tokens) ->
    Fun = read_function(Tokens),
    io:format(user, "Fun: ~p~n", [Fun]).
    


%% @doc Args are values after "++".
-spec handle_body(Node, Args, UsedVars) -> term() when
    Node :: erl_syntax:syntaxTree(),
    Args :: [Node],
    UsedVars :: set().

handle_body(Node, Args, UsedVars2) ->
    [_ChainAtom|Tokens] = tokenize(Node),
    %% ExArity is an expected arity, if this is a HOF.
    {ExArity, Tokens2} = read_body_arity(Tokens),
    io:format(user, "Fun Tokens: ~p~n", [Tokens2]),
    %% Args of the operator.
    {ArgVars, Params, UsedVars3} = create_args(Args, ExArity, UsedVars2),
    io:format(user, "~n ArgVars: ~p~nParams: ~p~n", [ArgVars, Params]),
    handle_functions(Tokens2).


to_int(undefined) -> 0;
to_int(N) -> N.


create_args(Args, ExArity, UsedVars) ->
    Arity = to_int(ExArity),
    {ArgVars, UsedVars2} = new_var_names(Arity, UsedVars),
    %% All missed values will be replaced with values from `Params'.
    Params = merge(insert_missed_args(Args, ArgVars)),
    {ArgVars, Params, UsedVars2}.


%% @doc Replace missed variables with its autogenerated names.
%% Missed variables are `_Missed' or `_'.
%% If you want this: 
%%  `_DontCare = hello, chain + io:write ++ _DontCare.'
%%  Use `DontCare = _DontCare, chain + io:write ++ DontCare.'
%%
%% What is `Vars'?
%%
%% F = chain + ... is
%% F = fun(Vars) ...
-spec insert_missed_args(Args, Vars) -> {Args, Vars} when
    Args :: [erl_syntax:syntaxTree()],
    Vars :: [atom()].
insert_missed_args(Args, Vars) ->
    insert_missed_args(Args, Vars, []).

%% Bs holds the reversed list of Args with replaced DC vars.
insert_missed_args([A|As], [V|Vs] = Vars, Bs) ->
    IsSkipped = is_dont_care_var(A),
    if 
        IsSkipped ->
            %% Use a variable as an argument.
            insert_missed_args(As, Vs, [V|Bs]);
        true ->
            insert_missed_args(As, Vars, [A|Bs])
    end;
insert_missed_args(Args, Vars, Bs) ->
    ArgsNoDC = lists:reverse(Bs, Args),
    %% What is ArgsNoDC?
    %%
    %% F = chain + ... ++ Args
    %% Args = V1 + V2 + V3
    %% If there is no don't care vars, Args = ArgsNoDC.
    %%
    %% if
    %% F = chain/4 + ... ++ V1 + V2
    %% then Vars = V3 + V4
    {ArgsNoDC, Vars}.

merge({X, Y}) -> X ++ Y.


is_dont_care_var(Arg) ->
    case node_type(Arg) of
        underscore -> true;
        variable -> $_ =:= head(erl_syntax:variable_literal(Arg));
        _OtherType -> false
    end.


head([H|_]) -> H.


-spec new_var_names(N, Used) -> {Vars, Used} when
    N :: integer(),
    Used :: set(), % set(atom()),
    Vars :: [atom()].

new_var_names(N, Used) ->
    New = erl_syntax_lib:new_variable_names(N, Used),
    {New, sets:union(Used, sets:from_list(New))}.

visitor(Tree, State) ->
    Str = tree_to_string(Tree),
    IsChain = is_chain(Str),
    Skip = {Tree, State},
    if
    IsChain ->
%       Tokens = string_to_tokens(Str),
%       io:format(user, "Str:\t~ts\nTree:~p\t\nTokens:\t~p\n",
%                 [Str, Tree, Tokens]),
        io:format(user, "Str:\t~ts\nTree:\t~p\n", [Str, Tree]),
        Chain = handle_chain(Tree),
        io:format(user, "Chain:\t~p\n", [Chain]),
        Body = handle_body(Chain#chain.body_node, 
                           Chain#chain.args_nodes,
                           State#state.variables),
        io:format(user, "Body:\t~p\n", [Body]),
        {Tree, State};
    true -> Skip
    end.


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


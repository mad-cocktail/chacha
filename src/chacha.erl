-module(chacha).
-export([parse_transform/2]).


parse_transform(Forms, _Options) ->
    io:format(user, "Before:\t~p\n\n", [Forms]),
    F = fun visitor/1,
    X = [postorder(F, Tree) || Tree <- Forms],
    io:format(user, "Before:\t~p\n\nAfter:\t~p\n", [Forms, X]),
    X.


visitor(AbsForms) ->
    Tokens = erl_parse:tokens(AbsForms),
    io:format(user, "Tokens:\t~p\n\nForms:\t~p\n", [Tokens, AbsForms]),
    AbsForms.

                                                                     
postorder(F, Form) ->                                                
    NewTree =                                                        
        case erl_syntax:subtrees(Form) of                            
        [] ->                                                        
            Form;                                                    
        List ->                                                      
            Groups = [handle_group(F, Group) || Group <- List],      
            Tree2 = erl_syntax:update_tree(Form, Groups),            
            Form2 = erl_syntax:revert(Tree2),                        
            Form2                                                    
        end,                                                         
    F(NewTree).                                                      

handle_group(F, Group) ->                        
    [postorder(F, Subtree) || Subtree <- Group]. 


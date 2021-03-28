-module(rpn).
-compile(export_all).

% (1+2-3)/4*(3+5)
% transform expression to syntax tree
% preorder traversal
% 1,2,+,3,-,4,/,3,5,+,*

-record(syntax_node, {
    l_child=null,
    value='',
    r_child=null,
    type=null, % operand or operator
    pior=null % high or low
}).

-record(syntax_tree_parser,{
    l_tree=null,
    r_tree=null,
    brackets_recursive=0,
    work_stack=[]
}).

syntax_parse(#syntax_tree_parser{l_tree=L, work_stack=[]}) ->
    io:format("~w ~n", [L]),
    L;

syntax_parse(#syntax_tree_parser{l_tree=L, r_tree=R, work_stack=["("|T]}) ->
    if
        L == null ->
            syntax_parse(#syntax_tree_parser{l_tree=L, r_tree=R, work_stack=T});
        true ->
            Node = #syntax_node{l_child=L#syntax_node.l_child, r_child=L#syntax_node.r_child, type=L#syntax_node.type, value=L#syntax_node.value, pior=low},
            io:format("Node: ~w ~n", [Node]),
            syntax_parse(#syntax_tree_parser{l_tree=Node, r_tree=R, work_stack=T})
    end;
syntax_parse(#syntax_tree_parser{l_tree=L, r_tree=R, work_stack=[")"|T]}) ->
    Node = #syntax_node{l_child=L#syntax_node.l_child, r_child=L#syntax_node.r_child, type=L#syntax_node.type, value=L#syntax_node.value, pior=high},
    io:format("Node: ~w ~n", [Node]),
    syntax_parse(#syntax_tree_parser{l_tree=Node, r_tree=R, work_stack=T});

syntax_parse(#syntax_tree_parser{l_tree=L, r_tree=R, work_stack=["+"|T]}) ->
    if 
        L#syntax_node.pior == low ->
            Node = #syntax_node{l_child=L#syntax_node.r_child, r_child=R, type=operator, value="+"},
            Node1 = #syntax_node{l_child=L#syntax_node.l_child, r_child=Node, type=L#syntax_node.type, value=L#syntax_node.value, pior=L#syntax_node.pior},
            io:format("Node1: ~w ~n", [Node1]),
            syntax_parse(#syntax_tree_parser{l_tree=Node1, r_tree=null, work_stack=T});
        true ->
            Node = #syntax_node{l_child=L, r_child=R, type=operator, value="+"},
            syntax_parse(#syntax_tree_parser{l_tree=Node, r_tree=null, work_stack=T})
    end;
syntax_parse(#syntax_tree_parser{l_tree=L, r_tree=R, work_stack=["-"|T]}) ->
    if 
        L#syntax_node.pior == low ->
            Node = #syntax_node{l_child=L#syntax_node.r_child, r_child=R, type=operator, value="-"},
            Node1 = #syntax_node{l_child=L#syntax_node.l_child, r_child=Node, type=L#syntax_node.type, value=L#syntax_node.value, pior=L#syntax_node.pior},
            io:format("Node1: ~w ~n", [Node1]),
            syntax_parse(#syntax_tree_parser{l_tree=Node1, r_tree=null, work_stack=T});
        true ->
            Node = #syntax_node{l_child=L, r_child=R, type=operator, value="-"},
            syntax_parse(#syntax_tree_parser{l_tree=Node, r_tree=null, work_stack=T})
    end;

syntax_parse(#syntax_tree_parser{l_tree=L, r_tree=R, work_stack=["*"|T]}) ->
    if 
        L#syntax_node.value == "+", L#syntax_node.pior /= high ->
            Node = #syntax_node{l_child=L#syntax_node.r_child, r_child=R, type=operator, value="*"},
            syntax_parse(#syntax_tree_parser{l_tree=L#syntax_node.l_child, r_tree=Node, work_stack=["+"|T]});
        L#syntax_node.value == "-", L#syntax_node.pior /= high ->
            Node = #syntax_node{l_child=L#syntax_node.r_child, r_child=R, type=operator, value="*"},
            syntax_parse(#syntax_tree_parser{l_tree=L#syntax_node.l_child, r_tree=Node, work_stack=["-"|T]});
        true ->
            Node = #syntax_node{l_child=L, r_child=R, type=operator, value="*"},
            syntax_parse(#syntax_tree_parser{l_tree=Node, r_tree=null, work_stack=T})
    end;
syntax_parse(#syntax_tree_parser{l_tree=L, r_tree=R, work_stack=["/"|T]}) ->
    if 
        L#syntax_node.value == "+", L#syntax_node.pior /= high ->
            Node = #syntax_node{l_child=L#syntax_node.r_child, r_child=R, type=operator, value="/"},
            syntax_parse(#syntax_tree_parser{l_tree=L#syntax_node.l_child, r_tree=Node, work_stack=["+"|T]});
        L#syntax_node.value == "-", L#syntax_node.pior /= high ->
            Node = #syntax_node{l_child=L#syntax_node.r_child, r_child=R, type=operator, value="/"},
            syntax_parse(#syntax_tree_parser{l_tree=L#syntax_node.l_child, r_tree=Node, work_stack=["-"|T]});
        true ->
            Node = #syntax_node{l_child=L, r_child=R, type=operator, value="/"},
            syntax_parse(#syntax_tree_parser{l_tree=Node, r_tree=null, work_stack=T})
    end;

syntax_parse(#syntax_tree_parser{l_tree=null, r_tree=null, work_stack=[H|T]}) ->
    Node = #syntax_node{type=operand, value=H},
    syntax_parse(#syntax_tree_parser{l_tree=Node, r_tree=null, work_stack=T});
syntax_parse(#syntax_tree_parser{l_tree=null, r_tree=R, work_stack=[H|T]}) ->
    Node = #syntax_node{type=operand, value=H},
    syntax_parse(#syntax_tree_parser{l_tree=Node, r_tree=R, work_stack=T});
syntax_parse(#syntax_tree_parser{l_tree=L, r_tree=null, work_stack=[H|T]}) ->
    if
        L#syntax_node.type == operator ->
            Node = syntax_parse(#syntax_tree_parser{l_tree=L#syntax_node.r_child, work_stack=[H]}), %% so fucking powerful!!!
            Node1 = #syntax_node{l_child=L#syntax_node.l_child, r_child=Node, type=L#syntax_node.type, value=L#syntax_node.value, pior=L#syntax_node.pior},
            syntax_parse(#syntax_tree_parser{l_tree=Node1, r_tree=null, work_stack=T});
        true ->
            syntax_error
    end;

syntax_parse(_) ->
    pattern_error.

parse(L) ->
    syntax_parse(#syntax_tree_parser{work_stack=L}).

concat([H|T], L2, Res) ->
    concat(T, L2, [H|Res]);
concat([], [H|T], Res) ->
    concat([], T, [H|Res]);
concat([], [], Res) ->
    lists:reverse(Res).
concat(L1, L2) ->
    concat(L1, L2, []).

postorder_trav(#syntax_node{l_child=L,r_child=R,value=V}) ->
    concat(concat(postorder_trav(L), postorder_trav(R)), [V]);
postorder_trav(_) ->
    [].

to_list(Tree) ->
    postorder_trav(Tree).

string_to_number(S) ->
    case string:to_float(S) of 
        {error, no_float} -> list_to_integer(S);
        {F, _} -> F
    end.

rpn(["+"|T], [Op1,Op2|Stack]) ->
    rpn(T, [Op2+Op1|Stack]);
rpn(["-"|T], [Op1,Op2|Stack]) ->
    rpn(T, [Op2-Op1|Stack]);
rpn(["*"|T], [Op1,Op2|Stack]) ->
    rpn(T, [Op2*Op1|Stack]);
rpn(["/"|T], [Op1,Op2|Stack]) ->
    rpn(T, [Op2/Op1|Stack]);
rpn([H|T], Stack) ->
    rpn(T, [string_to_number(H)|Stack]);
rpn([], [H|_]) ->
    H.
rpn(L) ->
    rpn(L, []).

tokens([H|T], _) ->
    H == "(";
tokens(["("|T], [C|S]) ->
    tokens(T, ["(", C|S]);
tokens([")"|T], [C|S]) ->
    tokens(T, [")", C|S]);
tokens(["+"|T], [C|S]) ->
    tokens(T, ["+", C|S]);
tokens(["-"|T], [C|S]) ->
    tokens(T, ["-", C|S]);
tokens(["*"|T], [C|S]) ->
    tokens(T, ["*", C|S]);
tokens(["/"|T], [C|S]) ->
    tokens(T, ["/", C|S]);
tokens([H|T], [C|S]) ->
    tokens(T, [[H,C]|S]);
tokens([], S) ->
    lists:reverse(S).
tokens(S) ->
    tokens(S, []).

eval(L) ->
    rpn(to_list(parse(string:tokens(L, " ")))).

test() ->
    % T = syntax_parse(#syntax_tree_parser{work_stack=[10,"*",1,"+",2,"*",3,"-",4,"*",5,"/",6]}),
    % syntax_parse(#syntax_tree_parser{work_stack=[10,"*",1]}).
    % syntax_parse(#syntax_tree_parser{work_stack=["(","(",1,"-",2,")","+",3,")","*",4]}).
    % syntax_parse(#syntax_tree_parser{work_stack=["(",1,"-",2,"+",3,")","*",4]}).
    % syntax_parse(#syntax_tree_parser{work_stack=["(",1,"-",2,"*",3,")","*",4]}).
    % syntax_parse(#syntax_tree_parser{work_stack=[1,"*","(",2,"+",3,")"]}).
    % T=syntax_parse(#syntax_tree_parser{work_stack=["(",1,"-",2,")","*","(",3,"+",4,")"]}),
    % L=to_list(T),
    % rpn(L).
    % string:tokens("( 1 - 2 ) * ( 3 + 4 )", " ").
    % eval("( 1.0 + 2 ) * ( 3 + 4 )").
    tokens("(1+2)*(3+4)").
    
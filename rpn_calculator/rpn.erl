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
    brackets=null,
    type=null % operand or operator
}).

-record(syntax_tree_parser,{
    tree=null,
    work_stack=[]
}).

insert_op2(#syntax_node{l_child=L, r_child=null, type=T, value=V},N2) ->
    #syntax_node{l_child=L, r_child=N2, type=T, value=V};
insert_op2(N1,N2) ->
    N1#syntax_node{r_child=insert_op2(N1#syntax_node.r_child,N2)}.

syntax_parse(#syntax_tree_parser{tree=L, work_stack=[]}) ->
    io:format("Node0: ~w ~n", [L]),
    #syntax_tree_parser{tree=L, work_stack=[]};

syntax_parse(#syntax_tree_parser{tree=null, work_stack=["("|T]}) ->
    P = syntax_parse(#syntax_tree_parser{work_stack=T}),
    io:format("Node1: ~w ~n", [P]),
    syntax_parse(#syntax_tree_parser{tree=P#syntax_tree_parser.tree, work_stack=P#syntax_tree_parser.work_stack});
syntax_parse(#syntax_tree_parser{tree=L, work_stack=["("|T]}) ->
    P = syntax_parse(#syntax_tree_parser{work_stack=T}),
    io:format("Node2: ~w ~n", [P#syntax_tree_parser.tree]),
    Node = insert_op2(L,P#syntax_tree_parser.tree),
    % Node = #syntax_node{l_child=L#syntax_node.l_child, r_child=P#syntax_tree_parser.tree, type=L#syntax_node.type, value=L#syntax_node.value},
    io:format("Node3: ~w ~n", [Node]),
    io:format("Node4: ~w ~n", [L]),
    syntax_parse(#syntax_tree_parser{tree=Node, work_stack=P#syntax_tree_parser.work_stack});
syntax_parse(#syntax_tree_parser{tree=L, work_stack=[")"|T]}) ->
    io:format("Node5: ~w ~n", [L]),
    #syntax_tree_parser{tree=L#syntax_node{brackets=true}, work_stack=T};

syntax_parse(#syntax_tree_parser{tree=L, work_stack=["+"|T]}) ->
    Node = #syntax_node{l_child=L, type=operator, value="+"},
    io:format("Node6: ~w ~n", [Node]),
    syntax_parse(#syntax_tree_parser{tree=Node, work_stack=T});
syntax_parse(#syntax_tree_parser{tree=L, work_stack=["-"|T]}) ->
    Node = #syntax_node{l_child=L, type=operator, value="-"},
    io:format("Node7: ~w ~n", [Node]),
    syntax_parse(#syntax_tree_parser{tree=Node, work_stack=T});

syntax_parse(#syntax_tree_parser{tree=L, work_stack=["*"|T]}) ->
    if 
        L#syntax_node.value == "+", L#syntax_node.brackets =/= true ->
            Node = #syntax_node{l_child=L#syntax_node.r_child, type=operator, value="*"},
            io:format("Node8: ~w ~n", [Node]),
            Node1 = L#syntax_node{r_child = Node},
            io:format("Node9: ~w ~n", [Node1]),
            syntax_parse(#syntax_tree_parser{tree=Node1, work_stack=T});
        L#syntax_node.value == "-", L#syntax_node.brackets =/= true ->
            Node = #syntax_node{l_child=L#syntax_node.r_child, type=operator, value="*"},
            io:format("Node10: ~w ~n", [Node]),
            Node1 = L#syntax_node{r_child = Node},
            io:format("Node11: ~w ~n", [Node1]),
            syntax_parse(#syntax_tree_parser{tree=Node1, work_stack=T});
        true ->
            Node = #syntax_node{l_child=L, type=operator, value="*"},
            io:format("Node12: ~w ~n", [Node]),
            syntax_parse(#syntax_tree_parser{tree=Node, work_stack=T})
    end;
syntax_parse(#syntax_tree_parser{tree=L, work_stack=["/"|T]}) ->
    if 
        L#syntax_node.value == "+", L#syntax_node.brackets =/= true ->
            Node = #syntax_node{l_child=L#syntax_node.r_child, type=operator, value="/"},
            io:format("Node13: ~w ~n", [Node]),
            Node1 = L#syntax_node{r_child = Node},
            io:format("Node14: ~w ~n", [Node1]),
            syntax_parse(#syntax_tree_parser{tree=Node1, work_stack=T});
        L#syntax_node.value == "-", L#syntax_node.brackets =/= true ->
            Node = #syntax_node{l_child=L#syntax_node.r_child, type=operator, value="/"},
            io:format("Node15: ~w ~n", [Node]),
            Node1 = L#syntax_node{r_child = Node},
            io:format("Node16: ~w ~n", [Node1]),
            syntax_parse(#syntax_tree_parser{tree=Node1, work_stack=T});
        true ->
            Node = #syntax_node{l_child=L, type=operator, value="/"},
            io:format("Node17: ~w ~n", [Node]),
            syntax_parse(#syntax_tree_parser{tree=Node, work_stack=T})
    end;

syntax_parse(#syntax_tree_parser{tree=null, work_stack=[H|T]}) ->
    Node = #syntax_node{type=operand, value=H},
    io:format("Node18: ~w ~n", [Node]),
    syntax_parse(#syntax_tree_parser{tree=Node, work_stack=T});
syntax_parse(#syntax_tree_parser{tree=L, work_stack=[H|T]}) ->
    if
        L#syntax_node.type == operator ->
            Node = #syntax_node{type=operand, value=H},
            io:format("Node19: ~w ~n", [Node]),
            Node1 = insert_op2(L, Node),
            io:format("Node20: ~w ~n", [Node1]),
            syntax_parse(#syntax_tree_parser{tree=Node1, work_stack=T});
        true ->
            syntax_error
    end;

syntax_parse(_) ->
    pattern_error.

parse(L) ->
    T = syntax_parse(#syntax_tree_parser{work_stack=L}),
    T#syntax_tree_parser.tree.

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
    io:format("~w+~w~n",[Op2,Op1]),
    rpn(T, [Op2+Op1|Stack]);
rpn(["-"|T], [Op1,Op2|Stack]) ->
    io:format("~w-~w~n",[Op2,Op1]),
    rpn(T, [Op2-Op1|Stack]);
rpn(["*"|T], [Op1,Op2|Stack]) ->
    io:format("~w*~w~n",[Op2,Op1]),
    rpn(T, [Op2*Op1|Stack]);
rpn(["/"|T], [Op1,Op2|Stack]) ->
    io:format("~w/~w~n",[Op2,Op1]),
    rpn(T, [Op2/Op1|Stack]);
rpn([H|T], Stack) ->
    rpn(T, [string_to_number(H)|Stack]);
rpn([], [H|_]) ->
    H.
rpn(L) ->
    rpn(L, []).

tokens([$(|T], L) ->
    tokens(T, ["(" | L]);
tokens([$)|T], L) ->
    tokens(T, [")" | L]);
tokens([$+|T], L) ->
    tokens(T, ["+" | L]);
tokens([$-|T], L) ->
    tokens(T, ["-" | L]);
tokens([$*|T], L) ->
    tokens(T, ["*" | L]);
tokens([$/|T], L) ->
    tokens(T, ["/" | L]);

tokens([H|T], ["("|L]) ->
    tokens(T, [[H],"("|L]);
tokens([H|T], [")"|L]) ->
    tokens(T, [[H],")"|L]);
tokens([H|T], ["+"|L]) ->
    tokens(T, [[H],"+"|L]);
tokens([H|T], ["-"|L]) ->
    tokens(T, [[H],"-"|L]);
tokens([H|T], ["*"|L]) ->
    tokens(T, [[H],"*"|L]);
tokens([H|T], ["/"|L]) ->
    tokens(T, [[H],"/"|L]);
tokens([H|T], [S|L]) ->
    tokens(T, [lists:reverse([H|lists:reverse(S)])|L]);
tokens([H|T], []) ->
    tokens(T, [[H]]);
tokens([], S) ->
    lists:reverse(S).
tokens(S) ->
    tokens(S, []).

eval(L) ->
    rpn(to_list(parse(string:tokens(L, " ")))).

eval_v2(L) ->
    rpn(to_list(parse(tokens(L)))).

test() ->
    % T = syntax_parse(#syntax_tree_parser{work_stack=[10,"*",1,"+",2,"*",3,"-",4,"*",5,"/",6]}),
    % T=syntax_parse(#syntax_tree_parser{work_stack=[10,"*",1]}),
    % T=syntax_parse(#syntax_tree_parser{work_stack=["(","(",1,"-",2,")","+",3,")","*",4]}),
    % T=syntax_parse(#syntax_tree_parser{work_stack=["(",1,"-",2,"+",3,")","*",4]}),
    % T=syntax_parse(#syntax_tree_parser{work_stack=["(",1,"-",2,"*",3,")","*",4]}),
    % T=syntax_parse(#syntax_tree_parser{work_stack=[1,"*","(",2,"+",3,")"]}),
    % T=syntax_parse(#syntax_tree_parser{work_stack=[1,"+",2.5,"*","(",3,"+",4,")"]}),
    % T=syntax_parse(#syntax_tree_parser{work_stack=["(",1,"+",2.5,")","*","(",3,"+",4,")"]}),
    % L=to_list(T#syntax_tree_parser.tree).
    % rpn(L).
    % string:tokens("( 1 - 2 ) * ( 3 + 4 )", " ").
    % eval("( 1.0 + 2 ) * ( 3 + 4 )").
    % tokens("(134+256)*(3+459)").
    % tokens("134.5+256*(3+459)").
    eval_v2("(1+2.5)*(2-1)").
    
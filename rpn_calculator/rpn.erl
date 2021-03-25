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
    type=null % operand or operator
}).

-record(syntax_tree_parser,{
    l_tree=null,
    r_tree=null,
    brackets_recursive=0,
    work_stack=[]
}).

syntax_parse0(#syntax_tree_parser{l_tree=L, brackets_recursive=0, work_stack=[]}) ->
    io:format("~w ~n", [L]),
    L;

syntax_parse0(#syntax_tree_parser{l_tree=L, r_tree=R, brackets_recursive=B, work_stack=['+'|T]}) ->
    io:format("L:~w ~n", [L]),
    io:format("R:~w ~n", [R]),
    Node = #syntax_node{l_child=L, r_child=R, type=operator, value='+'},
    io:format("Node:~w ~n", [Node]),
    syntax_parse0(#syntax_tree_parser{l_tree=Node, r_tree=null, brackets_recursive=B, work_stack=T});
syntax_parse0(#syntax_tree_parser{l_tree=L, r_tree=R, brackets_recursive=B, work_stack=['-'|T]}) ->
    Node = #syntax_node{l_child=L, r_child=R, type=operator, value='-'},
    syntax_parse0(#syntax_tree_parser{l_tree=Node, r_tree=null, brackets_recursive=B, work_stack=T});

syntax_parse0(#syntax_tree_parser{l_tree=L, r_tree=R, brackets_recursive=B, work_stack=['*'|T]}) ->
    io:format("L:~w ~n", [L]),
    io:format("R:~w ~n", [R]),
    if 
        L#syntax_node.value == '+' ->
            Node = #syntax_node{l_child=L#syntax_node.r_child, r_child=R, type=operator, value='*'},
            io:format("Node:~w ~n", [Node]),
            syntax_parse0(#syntax_tree_parser{l_tree=L#syntax_node.l_child, r_tree=Node, brackets_recursive=B, work_stack=['+'|T]});
        L#syntax_node.value == '-' ->
            Node = #syntax_node{l_child=L#syntax_node.r_child, r_child=R, type=operator, value='*'},
            syntax_parse0(#syntax_tree_parser{l_tree=L#syntax_node.l_child, r_tree=Node, brackets_recursive=B, work_stack=['-'|T]});
        true ->
            Node = #syntax_node{l_child=L, r_child=R, type=operator, value='*'},
            syntax_parse0(#syntax_tree_parser{l_tree=Node, r_tree=null, brackets_recursive=B, work_stack=T})
    end;

syntax_parse0(#syntax_tree_parser{l_tree=L, r_tree=R, brackets_recursive=B, work_stack=['/'|T]}) ->
    if 
        L#syntax_node.value == '+' ->
            Node = #syntax_node{l_child=L#syntax_node.r_child, r_child=R, type=operator, value='/'},
            syntax_parse0(#syntax_tree_parser{l_tree=L#syntax_node.l_child, r_tree=Node, brackets_recursive=B, work_stack=['+'|T]});
        L#syntax_node.value == '-' ->
            Node = #syntax_node{l_child=L#syntax_node.r_child, r_child=R, type=operator, value='/'},
            syntax_parse0(#syntax_tree_parser{l_tree=L#syntax_node.l_child, r_tree=Node, brackets_recursive=B, work_stack=['-'|T]});
        true ->
            Node = #syntax_node{l_child=L, r_child=R, type=operator, value='/'},
            syntax_parse0(#syntax_tree_parser{l_tree=Node, r_tree=null, brackets_recursive=B, work_stack=T})
    end;

syntax_parse0(#syntax_tree_parser{l_tree=null, r_tree=null, brackets_recursive=B, work_stack=[H|T]}) ->
    Node = #syntax_node{type=operand, value=H},
    syntax_parse0(#syntax_tree_parser{l_tree=Node, r_tree=null, brackets_recursive=B, work_stack=T});
syntax_parse0(#syntax_tree_parser{l_tree=null, r_tree=R, brackets_recursive=B, work_stack=[H|T]}) ->
    Node = #syntax_node{type=operand, value=H},
    syntax_parse0(#syntax_tree_parser{l_tree=Node, r_tree=R, brackets_recursive=B, work_stack=T});
syntax_parse0(#syntax_tree_parser{l_tree=L, r_tree=null, brackets_recursive=B, work_stack=[H|T]}) ->
    if
        L#syntax_node.type == operator ->
            Node = #syntax_node{type=operand, value=H},
            Node1 = #syntax_node{l_child=L#syntax_node.l_child, r_child=Node, type=L#syntax_node.type, value=L#syntax_node.value},
            syntax_parse0(#syntax_tree_parser{l_tree=Node1, r_tree=null, brackets_recursive=B, work_stack=T});
        true ->
            syntax_error
    end;

syntax_parse0(_) ->
    pattern_error.

test() ->
    syntax_parse0(#syntax_tree_parser{work_stack=[1,'+',2,'*',1]}).
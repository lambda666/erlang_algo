-module(rpn).
-compile(export_all).

% (1+2-3)/4*(3+5)
% transform expression to syntax tree
% preorder traversal
% 1,2,+,3,-,4,/,3,5,+,*

-record(syntax_node, {
    l_child=null,
    r_child=null,
    type=null,% operand or operator
    value=''
}).

-record(syntax_tree_parser,{
    l_tree=null,
    r_tree=null,
    brackets_recursive=0,
    work_stack=[]
}).

syntax_parse(_) ->
    null;
syntax_parse(#syntax_tree_parser{l_tree=L, brackets_recursive=0, work_stack=[]}) ->
    L;

syntax_parse(#syntax_tree_parser{l_tree=L, r_tree=R, brackets_recursive=B, work_stack=['('|T]}) ->
    syntax_parse(#syntax_tree_parser{l_tree=L, r_tree=R, brackets_recursive=B+1, work_stack=T});
syntax_parse(#syntax_tree_parser{l_tree=L, r_tree=R, brackets_recursive=B, work_stack=[')'|T]}) ->
    syntax_parse(#syntax_tree_parser{l_tree=L, r_tree=R, brackets_recursive=B-1, work_stack=T});

syntax_parse(#syntax_tree_parser{l_tree=L, r_tree=R, brackets_recursive=B, work_stack=['+'|T]}) ->
    Node = #syntax_node{l_child=L, r_child=R, type=operator, value='+'},
    syntax_parse(#syntax_tree_parser{l_tree=Node, r_tree=null, brackets_recursive=B, work_stack=T});
syntax_parse(#syntax_tree_parser{l_tree=L, r_tree=R, brackets_recursive=B, work_stack=['-'|T]}) ->
    Node = #syntax_node{l_child=L, r_child=R, type=operator, value='-'},
    syntax_parse(#syntax_tree_parser{l_tree=Node, r_tree=null, brackets_recursive=B, work_stack=T});
syntax_parse(#syntax_tree_parser{l_tree=L, r_tree=R, brackets_recursive=B, work_stack=['*'|T]}) ->
    Node = #syntax_node{l_child=L, r_child=R, type=operator, value='*'},
    syntax_parse(#syntax_tree_parser{l_tree=Node, r_tree=null, brackets_recursive=B, work_stack=T});
syntax_parse(#syntax_tree_parser{l_tree=L, r_tree=R, brackets_recursive=B, work_stack=['/'|T]}) ->
    Node = #syntax_node{l_child=L, r_child=R, type=operator, value='/'},
    syntax_parse(#syntax_tree_parser{l_tree=Node, r_tree=null, brackets_recursive=B, work_stack=T});

syntax_parse(#syntax_tree_parser{l_tree=null, r_tree=null, brackets_recursive=B, work_stack=[H|T]}) ->
    Node = #syntax_node{type=operand, value=H},
    syntax_parse(#syntax_tree_parser{l_tree=Node, r_tree=null, brackets_recursive=B, work_stack=T});
syntax_parse(#syntax_tree_parser{l_tree=null, r_tree=R, brackets_recursive=B, work_stack=[H|T]}) ->
    Node = #syntax_node{type=operand, value=H},
    syntax_parse(#syntax_tree_parser{l_tree=Node, r_tree=R, brackets_recursive=B, work_stack=T});
syntax_parse(#syntax_tree_parser{l_tree=L, r_tree=null, brackets_recursive=B, work_stack=[H|T]}) ->
    Node = #syntax_node{type=operand, value=H},
    if
        L#syntax_node.type == operator ->
            Node1 = #syntax_node{l_child=L#syntax_node.l_child, r_child=Node, type=L#syntax_node.type, value=L#syntax_node.value},
            syntax_parse(#syntax_tree_parser{l_tree=Node1, r_tree=null, brackets_recursive=B, work_stack=T});
        true ->
            
    syntax_parse(#syntax_tree_parser{l_tree=L, r_tree=Node, brackets_recursive=B, work_stack=T}).
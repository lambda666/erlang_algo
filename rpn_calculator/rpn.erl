-module(rpn).
-compile(export_all).

% (1+2-3)/4*(3+5)
% transform expression to syntax tree
% preorder traversal
% 1,2,+,3,-,4,/,3,5,+,*

-record(syntax_tree, {
    l_child=null,
    type=null,% operand or operator
    value='',
    r_chile=null,
    parent=null
}).
-record(syntax_tree_parser,{
    work_stack=[],
    tree=#syntax_tree{}
}).
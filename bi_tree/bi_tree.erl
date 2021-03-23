-module(bi_tree).
-compile(export_all).

-record(bnode, {
    lchild=null,
    key=null,
    rchild=null
}).

make_bnode(L,K,R) ->
    #bnode{lchild=L,key=K,rchild=R}.

insert(null, Key) ->
    make_bnode(null, Key, null);
insert(#bnode{lchild=L,key=K,rchild=R}, Key) ->
    if
        K == null ->
            make_bnode(L, Key, R);
        Key < K ->
            make_bnode(insert(L, Key), K, R);
        Key > K ->
            make_bnode(L, K, insert(R, Key));
        true ->
            make_bnode(L, K, R)
    end.

concat([H|T], L2, Res) ->
    concat(T, L2, [H|Res]);
concat([], [H|T], Res) ->
    concat([], T, [H|Res]);
concat([], [], Res) ->
    lists:reverse(Res).
concat(L1, L2) ->
    concat(L1, L2, []).

preorder_trav(_) ->
    [];
preorder_trav(#bnode{lchild=L,key=K,rchild=R}) ->
    concat([K|preorder_trav(L)],preorder_trav(R)).

inorder_trav(_) ->
    [];
inorder_trav(#bnode{lchild=L,key=K,rchild=R}) ->
    concat(inorder_trav(L),[K|inorder_trav(R)]).

postorder_trav(_) ->
    [];
postorder_trav(#bnode{lchild=L,key=K,rchild=R}) ->
    concat(concat(postorder_trav(L), postorder_trav(R)), [K]).

leftmost(#bnode{lchild=null,key=Key}) ->
    Key;
leftmost(#bnode{lchild=L}) ->
    leftmost(L).

rightmost(#bnode{key=Key, rchild=null}) ->
    Key;
rightmost(#bnode{rchild=R}) ->
    rightmost(R).

depth(_) ->
    0;
depth(#bnode{lchild=L,rchild=R}) ->
    1 + max(depth(L), depth(R)).

delete(#bnode{lchild=null,key=Key,rchild=R}, Key) ->
    R;
delete(#bnode{lchild=L,key=Key,rchild=null}, Key) ->
    L;
delete(#bnode{lchild=L,key=K,rchild=R}, Key) ->
    if
        K == null ->
            make_bnode(L, Key, R);
        Key < K ->
            make_bnode(delete(L, Key), K, R);
        Key > K ->
            make_bnode(L, K, delete(R, Key));
        true ->
            L_depth = depth(L),
            R_depth = depth(R),
            if 
                L_depth > R_depth ->
                    New = rightmost(L),
                    make_bnode(delete(L, New), New, R);
                true ->
                    New = leftmost(R),
                    make_bnode(L, New, delete(R, New))
            end
    end.

test() ->
    EmptyTree = make_bnode(null, null, null),
    N1 = insert(EmptyTree, 5),
    N2 = insert(N1, 2),
    N3 = insert(N2, 1),
    N4 = insert(N3, 9),
    inorder_trav(N4),
    N5 = delete(N4, 9).
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

preoder_trav(#bnode{lchild=L,key=K,rchild=R}) ->
    preoder_trav(L),K,preoder_trav(R).



test() ->
    Node = make_bnode(null, null, null),
    N1 = insert(Node, 1),
    N2 = insert(N1, 2),
    N2.
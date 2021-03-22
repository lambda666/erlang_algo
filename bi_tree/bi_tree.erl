-module(bi_tree).
-compile(export_all).

-record(btree, {
    lchild,
    value,
    rchild,
    parent
}).
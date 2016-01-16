-define(tabName2etsName(TabName),
case TabName of
    a ->
        aa;
    b ->
        bb;
    _ ->
        cc
end).

-record(cache, {tab_name::atom(), cache_name::atom()}).
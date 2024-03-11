-module(net_info).

-export([init_node_addres/3, new_user/2, delete_user/2]).

init_node_addres(Addr, Node, Myname) ->
    Table = ets:new(nodes_table, [ordered_set]),
    ets:insert(Table, {Myname, handler}),
    try
        {Addr, Node} ! {all_node, handler, Myname},
        receive
            {handler, NodeList} ->
                {ets:insert(Table, NodeList), Table}
        after 5000 ->
            {not_active_user, Table}
        end
    catch
        _:_ ->
            {not_active_user, Table}
    end.

new_user([], _) ->
    ok;
new_user([{Name, handler} | NodeList], Node) when Name /= node() ->
    {handler, Name} ! {add_user, Node},
    new_user(NodeList, Node);
new_user([_ | NodeList], Node) ->
    new_user(NodeList, Node).

delete_user([], _) ->
    ok;
delete_user([{Name, handler} | NodeList], Node) when Name /= node() ->
    {handler, Name} ! {delete_user, Node},
    delete_user(NodeList, Node);
delete_user([_ | NodeList], Node) ->
    new_user(NodeList, Node).

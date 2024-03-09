-module(net_info).

-export([init_node_addres/3, new_user/2, delete_user/2]).

init_node_addres(Addr, Node, Myname) ->
    Table = ets:new(nodes_table, [ordered_set, named_table, public]),
    ets:insert(Table, {Myname, Addr}),
    try {Addr, Node} ! {self(), all_node, Myname},
        receive
            NodeList ->
                {etc:insert(Table, NodeList), Table}
        after 2000 ->
            {not_active_user, Table}
        end
    catch
        _:_ -> 
            {not_active_user, Table}
    end.

new_user([], _) ->
    ok;
new_user([{Name, Addr} | NodeList], Node) ->
    {Addr, Name} ! {add_user, Node},
    new_user(NodeList, Node).

delete_user([], _) ->
    ok;
delete_user([{Name, Addr} | NodeList], Node) ->
    {Addr, Name} ! {add_user, Node},
    delete_user(NodeList, Node).

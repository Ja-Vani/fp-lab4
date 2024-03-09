-module(main_handler).

-export([handler/2, init_empty_data/0, init_data/2]).

init_empty_data() ->
    ets:new(data_table, [ordered_set, named_table, public]).

init_data([], _) -> init_empty_data();
init_data([{Name, Addr} | _], Myname) ->
    DataTable = init_empty_data(),
    {Addr, Name} ! {self(), all_data, Myname},
    receive
        {_, List} ->
            ets:insert(DataTable, List)
    end.

send_all_node_add([], _) ->
    ok;
send_all_node_add([{Name, Addr} | NodeList], Data) when Name =:= node(), Addr =:= self() ->
    {Addr, Name} ! {add_from, Data},
    send_all_node_add(NodeList, Data);
send_all_node_add([_ | NodeList], Data) ->
    send_all_node_add(NodeList, Data).

send_all_node_delete([], _) ->
    ok;
send_all_node_delete([{Name, Addr} | NodeList], Data) when Name =:= node(), Addr =:= self() ->
    {Addr, Name} ! {delete_from, Data},
    send_all_node_delete(NodeList, Data);
send_all_node_delete([_ | NodeList], Data) ->
    send_all_node_delete(NodeList, Data).

handler(NodeTable, DataTable) ->
    NodeList = ets:tab2list(NodeTable),
    receive
        {add, Data} ->
            ets:insert(DataTable, Data),
            send_all_node_add(NodeList, Data),
            handler(NodeTable, DataTable);
        {delete, Key} ->
            ets:delete(DataTable, Key),
            send_all_node_delete(NodeList, Key),
            handler(NodeTable, DataTable);
        {add_from, Data} ->
            ets:insert(DataTable, Data),
            handler(NodeTable, DataTable);
        {delete_from, Key} ->
            ets:delete(DataTable, Key),
            handler(NodeTable, DataTable);
        {Pid, all_node, Myname} ->
            net_info:new_user({Myname, Pid}, NodeList),
            {Pid, Myname} ! NodeList,
            ets:insert(NodeTable, {Myname, Pid}),
            handler(NodeTable, DataTable);
        {add_user, Node} ->
            ets:insert(NodeTable, Node),
            handler(NodeTable, DataTable);
        {delete_user, Name} ->
            ets:delete(NodeTable, Name),
            handler(NodeTable, DataTable);
        {close} ->
            net_info:delete_user(NodeList, {node(), self()})
    end.

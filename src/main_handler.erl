-module(main_handler).

-export([handler_init/2]).

init_empty_data() ->
    ets:new(data_table, [ordered_set]).

init_data([], _) -> init_empty_data();
init_data([{Name, handler} | _], Myname) ->
    DataTable = init_empty_data(),
    {handler, Name} ! {all_data, handler, Myname},
    receive
        {handler, List} ->
            ets:insert(DataTable, List),
            DataTable
    end.

send_all_node_add([], _) ->
    ok;
send_all_node_add([{Name, handler} | NodeList], Data) when Name /= node() ->
    {handler, Name} ! {add_from, Data},
    send_all_node_add(NodeList, Data);
send_all_node_add([_ | NodeList], Data) ->
    send_all_node_add(NodeList, Data).

send_all_node_delete([], _) ->
    ok;
send_all_node_delete([{Name, handler} | NodeList], Data) when Name /= node() ->
    {handler, Name} ! {delete_from, Data},
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
        {all_node, handler, Myname} ->
            {handler, Myname} ! {handler, NodeList},
            net_info:new_user(NodeList, {Myname, handler}),
            ets:insert(NodeTable, {Myname, handler}),
            handler(NodeTable, DataTable);
        {all_data, handler, Myname} ->
            {handler, Myname} ! {handler, ets:tab2list(DataTable)},
            handler(NodeTable, DataTable);
        {add_user, Node} ->
            ets:insert(NodeTable, Node),
            handler(NodeTable, DataTable);
        {delete_user, Name} ->
            ets:delete(NodeTable, Name),
            handler(NodeTable, DataTable);
        {close} ->
            net_info:delete_user(NodeList, {node(), handler});
        {info, Pid} ->
            Pid ! {handler, node()},
            handler(NodeTable, DataTable);
        {node_info, Pid} ->
            Pid ! {handler, NodeList},
            handler(NodeTable, DataTable);
        {read, Pid} ->
            Pid ! {handler, ets:tab2list(DataTable)},
            handler(NodeTable, DataTable)
    end.

handler_init(Addr, NodeName) ->
    case net_info:init_node_addres(Addr, NodeName, node()) of
        {not_active_user, NodeTable} ->
            DataTable = init_empty_data();
        {_, NodeTable} ->
            DataTable = init_data(ets:tab2list(NodeTable), node())
    end,
    handler(NodeTable, DataTable).
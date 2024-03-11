-module(main_handler).

-export([handler_init/2]).

init_empty_data() ->
    ets:new(data_table, [ordered_set]).

send_all_node_delete([], _) ->
    ok;
send_all_node_delete([{Name, handler} | NodeList], Data) when Name /= node() ->
    {handler, Name} ! {delete_from, Data},
    send_all_node_delete(NodeList, Data);
send_all_node_delete([_ | NodeList], Data) ->
    send_all_node_delete(NodeList, Data).

get_data_from_all([], List) ->
    List;
get_data_from_all([{Name, handler} | NodeList], List) when Name /= node() ->
    {handler, Name} ! {need_data, node()},
    receive
        {get_data, NewList} ->
            NewList
    end,
    get_data_from_all(NodeList, [NewList | List]);
get_data_from_all([_ | NodeList], List) ->
    get_data_from_all(NodeList, List).

handler(NodeTable, DataTable) ->
    NodeList = ets:tab2list(NodeTable),
    receive
        {add, Data} ->
            ets:insert(DataTable, Data),
            handler(NodeTable, DataTable);
        {delete, Key} ->
            ets:delete(DataTable, Key),
            send_all_node_delete(NodeList, Key),
            handler(NodeTable, DataTable);
        {delete_from, Key} ->
            ets:delete(DataTable, Key),
            handler(NodeTable, DataTable);
        {all_node, handler, Myname} ->
            {handler, Myname} ! {handler, NodeList},
            net_info:new_user(NodeList, {Myname, handler}),
            ets:insert(NodeTable, {Myname, handler}),
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
            List = get_data_from_all(NodeList, ets:tab2list(DataTable)),
            Pid ! {handler, List},
            handler(NodeTable, DataTable);
        {need_data, Name} ->
            {handler, Name} ! {get_data, ets:tab2list(DataTable)},
            handler(NodeTable, DataTable)
    end.

handler_init(Addr, NodeName) ->
    {_, NodeTable} = net_info:init_node_addres(Addr, NodeName, node()),
    DataTable = init_empty_data(),
    handler(NodeTable, DataTable).

-module(node_reciver).

-export([reciver/0, get_data_from_nodes/2]).

get_data_from_all([], List, _) ->
    List;
get_data_from_all([{Name, reciver} | NodeList], List, Key) when Name /= node() ->
    {reciver, Name} ! {need_data, node(), Key},
    receive
        {get_data, NewList} ->
            NewList
    end,
    get_data_from_all(NodeList, [NewList | List], Key);
get_data_from_all([_ | NodeList], List, Key) ->
    get_data_from_all(NodeList, List, Key).

reciver() ->
    receive
        {all_node, reciver, Myname} ->
            {reciver, Myname} ! {reciver, main_handler:nodes()},
            net_info:new_user(
                main_handler:nodes(), {Myname, reciver}),
            ets:insert(
                main_handler:info(), {Myname, reciver}),
            reciver();
        {add_user, Node} ->
            ets:insert(
                main_handler:info(), Node),
            reciver();
        {delete_user, Name} ->
            ets:delete(
                main_handler:info(), Name),
            reciver();
        {need_data, Node, Key} ->
            {reciver, Node} ! {get_data, main_handler:get(Key)}
    end.

get_data_from_nodes(Key, Table) ->
    get_data_from_all(ets:tab2list(Table), [], Key).

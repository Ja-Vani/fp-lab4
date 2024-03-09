-module(main).

-export([main/2]).

start_input(Pid, DataTable) ->
    case io:get_line("Write> ") of
        {error, _} ->
            start_input(Pid, DataTable);
        eof ->
            eof;
        Line ->
            [CommandStr | Data] = string:tokens(Line, " \n"),
            Command = list_to_atom(CommandStr),
            case Command of
                add ->
                    Pid ! {add, {erlang:localtime(), Data}},
                    start_input(Pid, DataTable);
                delete ->
                    Pid ! {delete, {erlang:localtime(), Data}},
                    start_input(Pid, DataTable);
                read ->
                    List = ets:tab2list(DataTable),
                    io:write(List),
                    start_input(Pid, DataTable);
                close ->
                    Pid ! {close};
                _ ->
                    start_input(Pid, DataTable)
            end
    end.

main(Addr, NodeName) ->
    case net_info:init_node_addres(Addr, NodeName, node()) of
        {not_active_user, NodeTable} ->
            DataTable = main_handler:init_empty_data();
        {NodeList, NodeTable} ->
            DataTable = main_handler:init_data(NodeList, node())
    end,
    start_input(spawn(main_handler, handler, [NodeTable, DataTable]), DataTable).

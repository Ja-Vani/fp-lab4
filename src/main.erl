-module(main).

-export([main/2]).

start_input() ->
    case io:get_line("Write> ") of
        {error, _} ->
            start_input();
        eof ->
            eof;
        "\n"->
            start_input();
        Line ->
            [CommandStr | Data] = string:tokens(Line, " \n"),
            Command = list_to_atom(CommandStr),
            case Command of
                add ->
                    handler ! {add, {erlang:localtime(), {Data}}},
                    start_input();
                delete ->
                    handler ! {delete, Data}, 
                    start_input();
                read ->
                    handler ! {read, self()},
                    receive
                        {handler, List} ->
                            io:format("~p\n", [List])
                    end,
                    start_input();
                close ->
                    handler ! {close};
                info ->
                    handler ! {info, self()},
                    receive
                        {handler, Node} ->
                            io:format("Addr: ~w\nNode: ~w\n", [handler, Node])
                    end,
                    start_input();
                nodes ->
                    handler ! {node_info, self()},
                    receive
                        {handler, List} ->
                            io:format("Addr: ~w\nNodes: ~p\n", [handler, List])
                    end,
                    start_input();
                _ ->
                    start_input()
            end
    end.

main(Addr, NodeName) ->
    Pid = spawn(main_handler, handler_init, [Addr, NodeName]),
    register(handler, Pid),
    start_input().

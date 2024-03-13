-module(main).

-export([main/0, start_input/0]).
-export([init/1]).

start_input() ->
    case io:get_line("Write> ") of
        {error, _} ->
            start_input();
        eof ->
            eof;
        "\n" ->
            start_input();
        Line ->
            [CommandStr | Data] = string:tokens(Line, " \n"),
            Command = list_to_atom(CommandStr),
            case Command of
                add ->
                    [Key | Value] = Data,
                    main_handler:set(Key, Value),
                    start_input();
                delete ->
                    main_handler:set(Data, []),
                    start_input();
                read ->
                    List = main_handler:get(Data),
                    io:format("~p\n", [List]),
                    start_input();
                close ->
                    reciver ! {close};
                info ->
                    List = main_handler:info(),
                    io:format("Nodes: ~p\n", [List]),
                    start_input();
                nodes ->
                    List = main_handler:nodes(),
                    io:format("Nodes: ~p\n", [List]),
                    start_input();
                _ ->
                    start_input()
            end
    end.

main() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    SupervisorSpecification =
        #{strategy => one_for_one,
          intensity => 10,
          period => 60},

    ChildSpecifications =
        [#{id => handler,
           start => {main_handler, handler_init, []},
           restart => permanent,
           shutdown => 2000,
           type => worker,
           modules => [main_handler]},
         #{id => handler,
           start => {main, start_input, []},
           restart => permanent,
           shutdown => 2000,
           type => worker,
           modules => [main]},
         #{id => reciver,
           start => {node_reciver, reciver, []},
           restart => permanent,
           shutdown => 2000,
           type => worker,
           modules => [node_reciver]}],
    {ok, {SupervisorSpecification, ChildSpecifications}}.

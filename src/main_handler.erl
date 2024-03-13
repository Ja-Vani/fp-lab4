-module(main_handler).

-export([handler_init/0, set/2, get/1, info/0, nodes/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

init_empty_data() ->
    ets:new(data_table, [ordered_set]).

handle_cast({set, Key, Value}, {NodeTable, DataTable}) ->
    ets:insert(DataTable, {Key, Value}),
    {noreply, {NodeTable, DataTable}};
handle_cast({add_node, Node}, {NodeTable, DataTable}) ->
    Nodes = net_info:get_nodes(Node),
    ets:insert(NodeTable, Nodes),
    {noreply, {NodeTable, DataTable}}.

handle_call({get, Key}, _From, {NodeTable, DataTable}) ->
    List =
        case ets:lookup(DataTable, Key) of
            [] ->
                node_reciver:get_data_from_nodes(Key, NodeTable);
            Data ->
                Data
        end,
    {reply, List, {NodeTable, DataTable}};
handle_call({info}, _From, {NodeTable, DataTable}) ->
    {reply, node(), {NodeTable, DataTable}};
handle_call({nodes}, _From, {NodeTable, DataTable}) ->
    {reply, ets:tab2list(NodeTable), {NodeTable, DataTable}}.

handler_init() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

set(Key, Value) ->
    gen_server:cast({global, ?MODULE}, {set, Key, Value}).

get(Key) ->
    gen_server:call({global, ?MODULE}, {get, Key}).

info() ->
    gen_server:call(?MODULE, {info}).

nodes() ->
    gen_server:call(?MODULE, {nodes}).

init([]) ->
    {_, NodeTable} = net_info:init_node_addres(0, 0, node()),
    DataTable = init_empty_data(),
    {ok, {NodeTable, DataTable}}.

handle_info(_Message, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

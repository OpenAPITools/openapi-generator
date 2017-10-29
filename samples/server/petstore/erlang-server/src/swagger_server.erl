-module(swagger_server).


-define(DEFAULT_ACCEPTORS_POOLSIZE, 100).
-define(DEFAULT_LOGIC_HANDLER, swagger_default_logic_handler).

-export([child_spec/2]).

-spec child_spec( ID :: any(), #{
    ip            => inet:ip_address(),
    port          => inet:port_number(),
    logic_handler => module(),
    net_opts      => []
}) -> supervisor:child_spec().

child_spec(ID, #{
    ip            := IP ,
    port          := Port,
    net_opts      := NetOpts
} = Params) ->
    AcceptorsPool = ?DEFAULT_ACCEPTORS_POOLSIZE,
    {Transport, TransportOpts} = get_socket_transport(IP, Port, NetOpts),
    LogicHandler = maps:get(logic_handler, Params, ?DEFAULT_LOGIC_HANDLER),
    ExtraOpts = maps:get(cowboy_extra_opts, Params, []),
    CowboyOpts = get_cowboy_config(LogicHandler, ExtraOpts),
    ranch:child_spec({?MODULE, ID}, AcceptorsPool,
        Transport, TransportOpts, cowboy_protocol, CowboyOpts).

get_socket_transport(IP, Port, Options) ->
    Opts = [
        {ip,   IP},
        {port, Port}
    ],
    case swagger_utils:get_opt(ssl, Options) of
        SslOpts = [_|_] ->
            {ranch_ssl, Opts ++ SslOpts};
        undefined ->
            {ranch_tcp, Opts}
    end.

get_cowboy_config(LogicHandler, ExtraOpts) ->
    get_cowboy_config(LogicHandler, ExtraOpts, get_default_opts(LogicHandler)).

get_cowboy_config(_LogicHandler, [], Opts) ->
    Opts;

get_cowboy_config(LogicHandler, [{env, Env} | Rest], Opts) ->
    NewEnv = case proplists:get_value(dispatch, Env) of
        undefined -> [get_default_dispatch(LogicHandler) | Env];
        _ -> Env
    end,
    get_cowboy_config(LogicHandler, Rest, store_key(env, NewEnv, Opts));

get_cowboy_config(LogicHandler, [{Key, Value}| Rest], Opts) ->
    get_cowboy_config(LogicHandler, Rest, store_key(Key, Value, Opts)).

get_default_dispatch(LogicHandler) ->
    Paths = swagger_router:get_paths(LogicHandler),
    {dispatch, cowboy_router:compile(Paths)}.

get_default_opts(LogicHandler) ->
    [{env, [get_default_dispatch(LogicHandler)]}].

store_key(Key, Value, Opts) ->
    lists:keystore(Key, 1, Opts, {Key, Value}).

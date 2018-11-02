-module(openapi_server).


-define(DEFAULT_LOGIC_HANDLER, openapi_default_logic_handler).

-export([start/2]).

-spec start( ID :: any(), #{
    ip            => inet:ip_address(),
    port          => inet:port_number(),
    logic_handler => module(),
    net_opts      => []
}) -> {ok, pid()} | {error, any()}.

start(ID, #{
    ip            := IP ,
    port          := Port,
    net_opts      := NetOpts
} = Params) ->
    {Transport, TransportOpts} = get_socket_transport(IP, Port, NetOpts),
    LogicHandler = maps:get(logic_handler, Params, ?DEFAULT_LOGIC_HANDLER),
    ExtraOpts = maps:get(cowboy_extra_opts, Params, []),
    CowboyOpts = get_cowboy_config(LogicHandler, ExtraOpts),
    case Transport of
        ssl ->
            cowboy:start_tls(ID, TransportOpts, CowboyOpts);
        tcp ->
            cowboy:start_clear(ID, TransportOpts, CowboyOpts)
    end.

get_socket_transport(IP, Port, Options) ->
    Opts = [
        {ip,   IP},
        {port, Port}
    ],
    case openapi_utils:get_opt(ssl, Options) of
        SslOpts = [_|_] ->
            {ssl, Opts ++ SslOpts};
        undefined ->
            {tcp, Opts}
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
    Paths = openapi_router:get_paths(LogicHandler),
    #{dispatch => cowboy_router:compile(Paths)}.

get_default_opts(LogicHandler) ->
    #{env => get_default_dispatch(LogicHandler)}.

store_key(Key, Value, Opts) ->
    lists:keystore(Key, 1, Opts, {Key, Value}).

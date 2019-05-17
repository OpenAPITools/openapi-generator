-module(petstore_statem).

-behaviour(proper_statem).

-include("petstore.hrl").
-include_lib("proper/include/proper_common.hrl").
-include_lib("stdlib/include/assert.hrl").

-compile(export_all).
-compile(nowarn_export_all).

-include("petstore_statem.hrl").

%%==============================================================================
%% The statem's property
%%==============================================================================

prop_main() ->
  setup(),
  ?FORALL( Cmds
         , proper_statem:commands(?MODULE)
         , begin
             cleanup(),
             { History
             , State
             , Result
             } = proper_statem:run_commands(?MODULE, Cmds),
             ?WHENFAIL(
                io:format("History: ~p\nState: ~p\nResult: ~p\nCmds: ~p\n",
                          [ History
                          , State
                          , Result
                          , proper_statem:command_names(Cmds)
                          ]),
                proper:aggregate( proper_statem:command_names(Cmds)
                                , Result =:= ok
                                )
               )
           end
         ).

%%==============================================================================
%% Setup
%%==============================================================================

setup() -> ok.

%%==============================================================================
%% Cleanup
%%==============================================================================

cleanup() -> ok.

%%==============================================================================
%% Initial State
%%==============================================================================

initial_state() -> #{}.

%%==============================================================================
%% create_user
%%==============================================================================

create_user(PetstoreUser) ->
  petstore_api:create_user(PetstoreUser).

create_user_args(_S) ->
  [petstore_user:petstore_user()].

%%==============================================================================
%% create_users_with_array_input
%%==============================================================================

create_users_with_array_input(PetstoreUserArray) ->
  petstore_api:create_users_with_array_input(PetstoreUserArray).

create_users_with_array_input_args(_S) ->
  [list(petstore_user:petstore_user())].

%%==============================================================================
%% create_users_with_list_input
%%==============================================================================

create_users_with_list_input(PetstoreUserArray) ->
  petstore_api:create_users_with_list_input(PetstoreUserArray).

create_users_with_list_input_args(_S) ->
  [list(petstore_user:petstore_user())].

%%==============================================================================
%% delete_user
%%==============================================================================

delete_user(Username) ->
  petstore_api:delete_user(Username).

delete_user_args(_S) ->
  [binary()].

%%==============================================================================
%% get_user_by_name
%%==============================================================================

get_user_by_name(Username) ->
  petstore_api:get_user_by_name(Username).

get_user_by_name_args(_S) ->
  [binary()].

%%==============================================================================
%% login_user
%%==============================================================================

login_user(Username, Password) ->
  petstore_api:login_user(Username, Password).

login_user_args(_S) ->
  [binary(), binary()].

%%==============================================================================
%% logout_user
%%==============================================================================

logout_user() ->
  petstore_api:logout_user().

logout_user_args(_S) ->
  [].

%%==============================================================================
%% update_user
%%==============================================================================

update_user(Username, PetstoreUser) ->
  petstore_api:update_user(Username, PetstoreUser).

update_user_args(_S) ->
  [binary(), petstore_user:petstore_user()].


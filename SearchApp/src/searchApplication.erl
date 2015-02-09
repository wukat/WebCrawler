%%%-------------------------------------------------------------------
%%% @author wukat (Wojciech Kasperek)
%%% @copyright (C) 2015, wukat
%%% @doc
%%%
%%% @end
%%% Created : 09. sty 2015 17:13
%%%-------------------------------------------------------------------
-module(searchApplication).
-author("wukat").

-behaviour(application).

%% API
-export([standard_start/0]).

%% Application callbacks
-export([start/2,
  stop/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts application with all dependencies
%%
%% @end
%%--------------------------------------------------------------------
standard_start() ->
  start_deps(searchApplication, permanent).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @end
%%--------------------------------------------------------------------
-spec(start(StartType :: normal | {takeover, node()} | {failover, node()},
    StartArgs :: term()) ->
  {ok, pid()} |
  {ok, pid(), State :: term()} |
  {error, Reason :: term()}).
start(_StartType, _StartArgs) ->
  case topSearchSupervisor:start_link() of
    {ok, Pid} ->
      global:register_name(searchSupervisor, Pid),
      io:fwrite("~s", ["You can now search! Use searchServer:search_in_text or searchServer:search_by_title to look for your words in pages texts or titles. \n"]),
      {ok, Pid};
    Error ->
      Error
  end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec(stop(State :: term()) -> term()).
stop(_State) ->
  exit(global:whereis_name(searchSupervisor), shutdown),
  ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Starts all application App dependencies
%%
%% @end
%%--------------------------------------------------------------------
start_deps(App, Type) ->
  case application:start(App, Type) of
    ok ->
      ok;
    {error, {not_started, Dep}} ->
      start_deps(Dep, Type),
      start_deps(App, Type),
      io:fwrite("~s", ["Starting dependency: " ++ atom_to_list(Dep) ++ "\n"]);
    {error, {already_started, Dep}} ->
      io:fwrite("~s", ["Already started: " ++ atom_to_list(Dep) ++ "\n"]);
    {error, Err} ->
      io:fwrite("~s", ["Something went wrong, check dependencies! \n"]),
      erlang:display(Err)
  end.
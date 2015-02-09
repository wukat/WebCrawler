%%%-------------------------------------------------------------------
%%% @author wukat (Wojciech Kasperek)
%%% @copyright (C) 2015, wukat
%%% @doc
%%%
%%% @end
%%% Created : 08. sty 2015 14:31
%%%-------------------------------------------------------------------
-module(crawlerApplication).
-author("wukat").

-behaviour(application).

%% API
-export([standard_start/0]).

%% Application callbacks
-export([stop/1, start/2]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts application with default parameters and all dependencies
%%
%% @end
%%--------------------------------------------------------------------
standard_start() ->
  start_deps(crawlerApplication, permanent).

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
  case topCrawlerSupervisor:start_link(_StartArgs) of
    {ok, Pid} ->
      global:register_name(topSupervisor, Pid),
      io:fwrite("~s", ["Crawler started! \n"]),
      [Domain, Base_link] = _StartArgs,
      io:fwrite("~s", ["Domain: " ++ Domain ++ "; start link: " ++ Base_link ++ ".\n"]),
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
  exit(global:whereis_name(topSupervisor), shutdown),
  ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%
%% %%--------------------------------------------------------------------
%% %% @private
%% %% @doc
%% %% Starts crawling
%% %%
%% %% @end
%% %%--------------------------------------------------------------------
%% start_crawler(Domain, Base_link) ->
%%   {_, Pid} = topCrawlerSupervisor:start_link([Domain, Base_link]),
%%   global:register_name(topSupervisor, Pid),
%%   io:fwrite("~s", ["Crawler started! \n"]),
%%   io:fwrite("~s", ["Domain: " ++ Domain ++ "; start link: " ++ Base_link ++ ".\n"]),
%%   {ok, Pid}.

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
    {error, _} ->
      io:fwrite("~s", ["Something went wrong, check dependencies!"])
  end.
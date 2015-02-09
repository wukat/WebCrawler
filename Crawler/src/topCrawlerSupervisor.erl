%%%-------------------------------------------------------------------
%%% @author wukat (Wojciech Kasperek)
%%% @copyright (C) 2015, wukat
%%% @doc Top application supervisor
%%%
%%% @end
%%% Created : 07. sty 2015 23:23
%%%-------------------------------------------------------------------
-module(topCrawlerSupervisor).
-author("wukat").

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1, add_child/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Adds child - domainServer -  with given parameters
%%
%% Expected parameters format:
%% Base_link -> like http://www.something.com/ (must be http://www.)
%% Domain -> like something.com
%% Link -> like Base_link
%% Note, that Base_link should be a base of link (ex. Link = Base_link ++ "index.html/") and both should be in given Domain.
%% @end
%%--------------------------------------------------------------------
add_child([Domain, Base_link, Link]) ->
  Restart = permanent,
  Shutdown = 2000,
  Type = worker,
  supervisor:start_child(topCrawlerSupervisor, {Base_link, {domainServer, start_link, [Domain, Base_link, Link]},
    Restart, Shutdown, Type, [domainServer]}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% Base_link -> like http://www.something.com (must be http://www.
%% Domain -> like something.com
%% @end
%%--------------------------------------------------------------------
-spec(start_link([string()]) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link([Domain, Base_link]) ->
  case re:run(Base_link, Domain) of
    nomatch -> erlang:display("Base link must be in domain!");
    _ ->
      erlastic_search:create_index(<<"agh.edu.pl">>),
      supervisor:start_link({local, ?SERVER}, ?MODULE, [Domain, Base_link, Base_link])
  end.

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
    MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
    [ChildSpec :: supervisor:child_spec()]
  }} |
  ignore |
  {error, Reason :: term()}).
init([Domain, Base_link, Link]) ->
  RestartStrategy = one_for_one,
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Restart = permanent,
  Shutdown = 2000,
  Type = worker,

  AChild = {Base_link, {domainServer, start_link, [Domain, Base_link, Link]},
    Restart, Shutdown, Type, [domainServer]},

  {ok, {SupFlags, [AChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

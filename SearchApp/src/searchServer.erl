%%%-------------------------------------------------------------------
%%% @author wukat (Wojciech Kasperek)
%%% @copyright (C) 2015, wukat
%%% @doc Gen server - search api
%%%
%%% @end
%%% Created : 09. sty 2015 12:53
%%%-------------------------------------------------------------------
-module(searchServer).
-author("wukat").

-behaviour(gen_server).

%% API
-export([start_link/0, search_by_title/1, search_in_text/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Search given value in collected sites' titles
%%
%% @end
%%--------------------------------------------------------------------
search_by_title(Value) ->
  io:fwrite("~ts", [gen_server:call(searchServer, {search, "title", Value})]).

%%--------------------------------------------------------------------
%% @doc
%% Search given value in collected sites
%%
%% @end
%%--------------------------------------------------------------------
search_in_text(Value) ->
  io:fwrite("~ts", [gen_server:call(searchServer, {search, "text", Value})]).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call({search, In, Value}, _From, State) ->
  {reply, process_response(erlastic_search:search(<<"agh.edu.pl">>, <<"external">>, unicode:characters_to_binary(In ++ ":" ++ http_uri:encode(Value)))), State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Process elastic_search response for presentation purpose
%%
%% @end
%%--------------------------------------------------------------------
process_response({error, {_, [{_, Message}, _]}}) ->
  Message;
process_response({ok, [_, _, _, Hits]}) ->
  case Hits of
    {_, [_, _, {<<"hits">>, Results}]} -> case process_results(Results, "") of
                                            "" -> "No matches \n";
                                            Res -> Res
                                          end;
    _ -> "Something went wrong"
  end;
process_response({error, _}) ->
  "Error, one word only. \n".

%%--------------------------------------------------------------------
%% @doc
%% Process results collected from elastic_search response for presentation purpose
%%
%% @end
%%--------------------------------------------------------------------
process_results([], Acc) -> Acc;
process_results([Result | Results], Acc) ->
  [_, _, _, _, Source] = Result,
  {_, [{_, Address}, {_, Title}, {_, _}]} = Source,
  process_results(Results, Acc ++ unicode:characters_to_list(Title) ++ ": " ++ unicode:characters_to_list(Address) ++ "\n").
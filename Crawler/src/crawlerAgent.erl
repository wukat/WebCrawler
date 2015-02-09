%%%-------------------------------------------------------------------
%%% @author wukat (Wojciech Kasperek)
%%% @copyright (C) 2015, wukat
%%% @doc
%%%
%%% @end
%%% Created : 08. sty 2015 09:57
%%%-------------------------------------------------------------------
-module(crawlerAgent).
-author("wukat").

%% API
-export([crawl/2]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts crawlerAgent work
%%
%% Agent gets site from given Link, makes HTML tree, shows all link
%% on site and saves site in search engine. Found links are sent to
%% appropriate domainServer. If no appropriate domainServer, new one
%% is created by supervisor.
%% @end
%%--------------------------------------------------------------------
crawl(Link, Domain) ->
  {_, X} = request(Link),
  HTMLTree = get_rid_of_html_tag(X),
  Links = ordsets:from_list(get_all_links(HTMLTree, Link, Domain)),
  save(HTMLTree, Link),
  send_links(Links, Domain).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Function sends given links to appropriate domainServer or calls
%% supervisor to create new domainServer.
%% @end
%%--------------------------------------------------------------------
send_links([], _) ->
  ok;
send_links([Link | Links], Domain) ->
  Base = get_url_base(Link),
  case find_child(Base, supervisor:which_children(topCrawlerSupervisor)) of
    no -> topCrawlerSupervisor:add_child([Domain, Base, Link]);
    {ok, Pid} -> gen_server:cast(Pid, {link, Link})
  end,
  send_links(Links, Domain).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Function finds appropriate domainServer in all supervisor's children.
%% @end
%%--------------------------------------------------------------------
find_child(_, []) -> no;
find_child(Name, [Child | Children]) ->
  case Child of
    {NameC, Pid, _, _} when NameC =:= Name -> {ok, Pid};
    _ -> find_child(Name, Children)
  end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Function makes base from URL, ex.:
%% "http://www.agh.edu.pl/kandydaci/" -> "http://www.agh.edu.pl/"
%% "www.agh.edu.pl/kandydaci/" -> "http://www.agh.edu.pl/"
%% "http://agh.edu.pl/kandydaci/" -> "http://www.agh.edu.pl/"
%% @end
%%--------------------------------------------------------------------
-spec(get_url_base(string()) -> string()).
get_url_base(Url) ->
  {Http, Base, _, _, _} = mochiweb_util:urlsplit(Url),
  case string:left(Base, 3) of
    "www" -> Http ++ "://" ++ Base;
    _ -> Http ++ "://www." ++ Base
  end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Function requests given Url. Returns only site content or error.
%% @end
%%--------------------------------------------------------------------
-spec(request(string()) -> {ok, string() | binary()} | error).
request(Url) ->
  case httpc:request(Url) of
    {ok, {{_, 200, _}, _, Result}} -> {ok, Result};
    _ -> error
  end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Function finds all links in given domain on site. Returns list of
%% absolute links.
%% @end
%%--------------------------------------------------------------------
-spec(get_all_links(string() | binary(), string(), string()) -> [string()] | notHtml).
get_all_links(Site, Site_address, Domain) ->
  check_domain(make_links_absolute(find_all_links(Site), Site_address), Domain).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Function checks if Link is in given Domain.
%% @end
%%--------------------------------------------------------------------
-spec(check_domain([string()], string()) -> [string()]).
check_domain(Links, Domain) ->
  lists:filter(fun(Link) -> {_, Base, _, _, _} = mochiweb_util:urlsplit(Link),
    re:run(Base, Domain) =/= nomatch end, Links).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Function finds attirbute in {key, value} pairs list.
%% Note that in html node "<a href="mailto@..."> attirbute is "href".
%% @end
%%--------------------------------------------------------------------
-spec(find_attribute([{binary(), binary()}], binary()) -> {binary(), binary()} | {}).
find_attribute(Attributes, Attribute) ->
  check_attributes(Attributes, Attribute).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% See find_attribute/2.
%% @end
%%--------------------------------------------------------------------
-spec(check_attributes([{binary(), binary()}], binary()) -> {binary(), binary()} | {}).
check_attributes([], _) -> {};
check_attributes([First | Rest], Attribute) ->
  {Key, Value} = First,
  if Key =:= Attribute ->
    {Key, Value};
    true -> check_attributes(Rest, Attribute)
  end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Function makes properly formatted link from given and base.
%% Proper format of link is "http://www.domain/something"
%% or "http://www.domain/mo/"
%% where domain is ex. "uci.agh.edu.pl".
%% @end
%%--------------------------------------------------------------------
-spec(make_link(string(), string()) -> string()).
make_link(Link, Base) ->
  LinkNew = case string:to_lower(string:left(Link, 4)) of
              "http" ->
                [Beg, End] = lists:map(fun binary_to_list/1, re:split(Link, "://")),
                case string:left(End, 3) of
                  "www" -> Link;
                  _ -> Beg ++ "://www." ++ End
                end;
              _ -> make_absolute_link(Link, Base)
            end,
  remove_slash_from_end(remove_get_part(LinkNew)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Remove part of link after # or ? (also (offset)).
%% @end
%%--------------------------------------------------------------------
remove_get_part(Link) ->
  {Http, Base, Next, _, _} = mochiweb_util:urlsplit(Link),
  case re:run(Next, "(offset)") of
    {match, _} -> Http ++ "://" ++ Base;
    _ -> Http ++ "://" ++ Base ++ Next
  end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Remove / from end of link.
%% @end
%%--------------------------------------------------------------------
remove_slash_from_end(Link) ->
  case string:right(Link, 1) of
    "/" -> remove_last_sign(Link);
    _ -> Link
  end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Remove last element from list.
%% @end
%%--------------------------------------------------------------------
remove_last_sign(Link) ->
  [_ | End] = lists:reverse(Link),
  lists:reverse(End).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Function makes absolute link from non absolute and base.
%% @end
%%--------------------------------------------------------------------
-spec(make_absolute_link(string(), string()) -> string()).
make_absolute_link(Link, Base) ->
  case check_mailto(Link) of
    false ->
      case string:left(Link, 1) of
        "#" -> Base;
        "/" -> "/" ++ RestOfLink = Link, make_absolute_link(RestOfLink, Base);
        _ ->
          Safe_link = mochiweb_util:safe_relative_path(Link),
          if
            Safe_link =/= undefined -> Base ++ "/" ++ Safe_link;
            true -> Base
          end
      end;
    true -> Base
  end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Function checks if link is mailto link.
%% @end
%%--------------------------------------------------------------------
check_mailto(Link) ->
  case re:run(Link, "@") of
    nomatch -> false;
    _ -> true
  end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Function finds all links in HTML tree.
%% @end
%%--------------------------------------------------------------------
-type html_node() :: {binary(), [{binary(), binary()}], [html_node() | binary()]}.
-spec(find_all_links(Body :: [html_node() | binary()]) -> [{binary(), binary()}]).
find_all_links(Body) ->
  Found = find_tokens_in_list(Body, <<"a">>),
  Links = [find_attribute(Attributes, <<"href">>) || {_, Attributes, _} <- Found],
  lists:filter(fun(X) -> X =/= [] end, Links).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Function finds token elements (ex. <<"a">>) in list of HTML nodes.
%% @end
%%--------------------------------------------------------------------
-spec(find_tokens_in_list([html_node() | binary()], binary()) -> [html_node()]).
find_tokens_in_list([], _) -> [];
find_tokens_in_list([El | Rest], Token) ->
  process_element(El, Token) ++ find_tokens_in_list(Rest, Token).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Function process  HTML node to find token elements in it.
%% @end
%%--------------------------------------------------------------------
-spec(process_element(html_node() | binary(), binary()) -> [html_node()]).
process_element(El, Token) ->
  case El of
    {Name, _, _} when Name =:= Token -> [El];
    {_, _, Value} -> find_tokens_in_list(Value, Token);
    _ -> []
  end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Function makes list of absolute links from list of Links.
%% @end
%%--------------------------------------------------------------------
-spec(make_links_absolute([binary()], string()) -> [string()]).
make_links_absolute([], _) -> [];
make_links_absolute(Links, Site_address) ->
  [make_link(binary_to_list(Link), get_url_base(Site_address)) || {_, Link} <- Links].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Function removes html tag from HTML tree.
%% @end
%%--------------------------------------------------------------------
get_rid_of_html_tag(Site) ->
  case mochiweb_html:parse(Site) of
    {<<"html">>, _, Value} -> Value;
    _ -> notHtml
  end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Function saves site in search engine.
%%
%% Saved data are site address, site title and text on site.
%% @end
%%--------------------------------------------------------------------
save(Page, Link) ->
  Title = get_title(Page),
  Text = html_body_to_text(Page),
  erlastic_search:index_doc_with_id(<<"agh.edu.pl">>, <<"external">>, list_to_binary(http_uri:encode(Link)), [{<<"address">>, list_to_binary(Link)}, {<<"title">>, unicode:characters_to_binary(Title)}, {<<"text">>, unicode:characters_to_binary(Text)}]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Function finds first occurance of Tag in HTML tree part.
%% @end
%%--------------------------------------------------------------------
get_tag(Body, Tag) ->
  [First | _] = find_tokens_in_list(Body, Tag),
  {_, _, Token} = First,
  Token.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Function gets site title.
%% @end
%%--------------------------------------------------------------------
get_title(Body) ->
  [Title] = get_tag(Body, <<"title">>),
  unicode:characters_to_list(Title).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Function converts body of HTML file to text (removes all tags).
%% @end
%%--------------------------------------------------------------------
html_body_to_text(Body) ->
  html_to_text(get_tag(Body, <<"body">>)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Function converts HTML file to text (removes all tags). DFS.
%% @end
%%--------------------------------------------------------------------
html_to_text(Body) ->
  breadth_html_to_text(Body, "").

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Used in html_to_text, goes through HTML nodes list entering
%% into every node.
%% @end
%%--------------------------------------------------------------------
breadth_html_to_text([], Acc) -> Acc;
breadth_html_to_text([First | Body], Acc) ->
  breadth_html_to_text(Body, Acc ++ " " ++ enter_node_html_to_text(First)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Used in breadth_html_to_text, if it is leaf of HTML tree, than
%% returns contained text (if it's not comment, script or style node);
%% if it's not a leaf, goes throogh it.
%% @end
%%--------------------------------------------------------------------
enter_node_html_to_text({comment, _}) ->
  "";
enter_node_html_to_text({Tag, _, Next}) ->
  case Tag of
    <<"script">> -> "";
    <<"style">> -> "";
    _ -> breadth_html_to_text(Next, "")
  end;
enter_node_html_to_text(Text) ->
  unicode:characters_to_list(Text).
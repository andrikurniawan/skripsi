-module(m_abs).
-behaviour(gen_model).

-export([
	m_find_value/3,
	m_to_list/2,
	m_value/2
]).

-include_lib("zotonic.hrl").

-define(API_URL, "http://localhost:8888/total").

-spec m_find_value(Key, Source, Context) -> #m{} | undefined | any() when
    Key:: integer() | atom() | string(),
    Source:: #m{},
    Context:: #context{}.

m_find_value(api_url, _, _Context) ->
    ?API_URL;

m_find_value(Type, #m{value=undefined} = M, _Context) ->
    M#m{value=[Type]};

m_find_value({query, Query}, #m{value=Q} = _, _Context) when is_list(Q) ->
	[Key] = Q,
	JsonQuery = jiffy:encode({Query}),
	{ResultPost} = fetch_data(JsonQuery),
    proplists:get_value(atom_to_binary(Key, latin1), ResultPost);

% Other values won't be processed
m_find_value(_, _, _Context) ->
    undefined. 

% Enggak perlu, tapi merupakan behavior dari gen_model
-spec m_to_list(Source, Context) -> list() when
    Source:: #m{},
    Context:: #context{}.
m_to_list(#m{value=undefined} = _M, _Context) ->
    [];
m_to_list(#m{value=Query} = _M, _Context) ->
    fetch_data(Query).

-spec m_value(Source, Context) -> undefined | any() when
    Source:: #m{},
    Context:: #context{}.
m_value(_, _Context) ->
    undefined.

-spec fetch_data(Query) -> list() when
    Query:: list().
fetch_data([]) ->
    [{error, "Params missing"}];
fetch_data(Query) ->
	    Url = ?API_URL,
	    case post_page_body(Url, Query) of
	        {error, Error} ->
	            [{error, Error}];
	        Json ->     
	            jiffy:decode(Json)
	    end.

post_page_body(Url, Body) ->
	case httpc:request(post, {Url, [], "application/json", Body}, [], []) of
		{ok,{_, _, Response}} ->
			Response;
		Error ->
			{error, Error}
	end.
-module(m_abs).
-behaviour(gen_model).

-export([
	m_find_value/3,
	m_to_list/2,
	m_value/2
]).

-include_lib("zotonic.hrl").

-define(RULES, "/home/andri/skripsi/zotonic/rules.txt").

-spec m_find_value(Key, Source, Context) -> #m{} | undefined | any() when
    Key:: integer() | atom() | string(),
    Source:: #m{},
    Context:: #context{}.

m_find_value(Type, #m{value=undefined} = M, _Context) ->
    M#m{value=[Type]};

m_find_value({query, Query}, #m{value=Q} = _, _Context) when is_list(Q) ->
	[Key] = Q,
	[Url, Param] = lookup_rules(Key),
	case validate_params(Param, Query) of
		false ->
			[{error, "Num of Params not same"}];
		true ->
			{DecodeJson} = fetch_data(binary_to_list(Url), jiffy:encode({Query})),
			proplists:get_value(atom_to_binary(Key, latin1), DecodeJson)
	end;

% Other values won't be processed
m_find_value(_, _, _Context) ->
    undefined. 

m_to_list(_, _Context) ->
	[].

m_value(_, _Context) ->
	undefined.

-spec fetch_data(Url, Query) -> list() when
	Url:: list(),
    Query:: list().
fetch_data(_,[]) ->
    [{error, "Params missing"}];
fetch_data("",_) ->
	[{error, "Url missing"}];
fetch_data(Url, Query) ->
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

lookup_rules(Key) ->
	File = ?RULES,
	case read_file(File) of
		{error, Error} ->
			[{error, Error}];
		[] ->
			[{error, "File empty"}];
		Json ->
			{DecodeJson} = jiffy:decode(Json),
			proplists:get_value(atom_to_binary(Key, latin1), DecodeJson)
	end.

read_file(File) ->
	case file:read_file(File) of
		{ok, Data} ->
			Data;
		eof ->
			[];
		Error ->
			{error, Error}
	end.

validate_params(Param, Query) ->
	case length(Query) == Param of
		false ->
			false;
		true ->
			true
	end.
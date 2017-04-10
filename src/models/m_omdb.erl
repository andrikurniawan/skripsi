-module(m_omdb).
-behaviour(gen_model).

-export([
    m_find_value/3,
    m_to_list/2,
    m_value/2
]).

-include_lib("zotonic.hrl").

-define(API_URL, "http://www.omdbapi.com/?").

-spec m_find_value(Key, Source, Context) -> #m{} | undefined | any() when
    Key:: integer() | atom() | string(),
    Source:: #m{},
    Context:: #context{}.
    
% Syntax: m.omdb.api_url
m_find_value(api_url, _, _Context) ->
    ?API_URL;

% % Syntax: m.omdb.movie[QueryString]
% m_find_value(movie, #m{value=undefined} = M, _Context) ->
%     M#m{value=[{type, "movie"}]};

% % Syntax: m.omdb.series[QueryString]
% m_find_value(series, #m{value=undefined} = M, _Context) ->
%     M#m{value=[{type, "series"}]};

% % Syntax: m.omdb.episode[QueryString]
% m_find_value(episode, #m{value=undefined} = M, _Context) ->
%     M#m{value=[{type, "episode"}]};

% Syntax: m.omdb.series[QueryString]
m_find_value(Type, #m{value=undefined} = M, _Context) ->
    M#m{value=[{type, Type}]};

% Syntax: m.omdb["tt0123456789"]
m_find_value("tt" ++ _Number = Id, #m{value=undefined} = M, _Context) ->
    M#m{value=[{id, Id}]};

% Syntax: m.omdb.sometype["tt0123456789"]
m_find_value("tt" ++ _Number = Id, #m{value=Query} = M, _Context) when is_list(Query) ->
    M#m{value=[{id, Id}] ++ Query};
    
% Syntax: m.omdb["some title"]
m_find_value(Title, #m{value=undefined} = M, _Context) when is_list(Title) ->
    M#m{value=[{title, Title}]};

% Syntax: m.omdb.sometype["some title"]
% If no atom is passed it must be a title (string)
m_find_value(Title, #m{value=Query} = M, _Context) when is_list(Title) ->
    M#m{value=[{title, Title}] ++ Query};
    
% Syntax: m.omdb[{query QueryParams}]
% For m.omdb[{query title="Dollhouse"}], Query is: [{title,"Dollhouse"}]
m_find_value({query, Query}, #m{value=undefined} = M, _Context) ->
    M#m{value=Query};

% Syntax: m.omdb.sometype[{query QueryParams}]
% For m.omdb.series[{query title="Dollhouse"}], Query is: [{title,"Dollhouse"}] and Q is: [{type,"series"}]
m_find_value({query, Query}, #m{value=Q} = M, _Context) when is_list(Q) ->
    M#m{value=Query ++ Q};

% Syntax: m.omdb[QueryString].title or m.omdb.sometype[QueryString].title
% Key is in this case 'title'
m_find_value(Key, #m{value=Query} = _M, _Context) when is_atom(Key) ->
    proplists:get_value(Key, fetch_data(Query));

% Other values won't be processed
m_find_value(_, _, _Context) ->
    undefined. 


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
    case proplists:is_defined(title, Query) or proplists:is_defined(id, Query) of
        false -> [{error, "Param id or title missing"}];
        true -> 
            QueryParts = lists:map(fun(Q) ->
                make_query_string(Q)
            end, Query),
            Url = ?API_URL ++ string:join(QueryParts, "&"),
            case get_page_body(Url) of
                {error, Error} ->
                    [{error, Error}];
                Json ->     
                    {struct, JsonData} = mochijson2:decode(Json),
                    lists:map(fun(D) ->
                        convert_data_prop(D)
                    end, JsonData)
            end
    end.
    

%% Translate query params id, title and type into parameters that OMDB wants
-spec make_query_string({Key, Value}) -> string() when
    Key:: atom(),
    Value:: string().
make_query_string({id, Id}) ->
    "i=" ++ Id;
make_query_string({title, Title}) ->
    "t=" ++ re:replace(Title, " ", "+", [global, {return, list}]);
make_query_string({type, Type}) ->
    "type=" ++ Type;
make_query_string(_) ->
    "".


%% Translate binary keys to atoms
-spec convert_data_prop({Key, Value}) -> {atom(), string()} when
    Key:: binary(),
    Value:: binary().
convert_data_prop({Key, Value}) ->
    {z_convert:to_atom(string:to_lower(z_convert:to_list(Key))), Value}.

convert_binary(Query) ->
    {mochijson2:encode([{X, list_to_binary(Y)} || {X,Y} <- Query])}.

get_page_body(Url) ->
    case httpc:request(get, {Url, []}, [], []) of
        {ok, {_, Headers, Body}} ->
            case content_length(Headers) of
                0 -> {error, "No content"};
                _ -> Body
            end;
        Error -> 
            {error, Error}
    end.

content_length(Headers) ->
list_to_integer(proplists:get_value("content-length", Headers, "0")).
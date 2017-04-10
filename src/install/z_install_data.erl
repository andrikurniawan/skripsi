%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2016 Marc Worrell, Arjan Scherpenisse
%%
%% @doc Initialize the database with start data.

%% Copyright 2009-2016 Marc Worrell, Arjan Scherpenisse
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%%     http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(z_install_data).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
         install/2,
         install_category/1,
         install_modules/1
]).

-include_lib("zotonic.hrl").

%% @doc Insert boot data into the database.
%% @spec install(Host::atom(), Connection) -> ok
install(Host, Context) ->
    lager:info("~p: Install start.", [Host]),
    ok = install_category(Context),
    ok = install_rsc(Context),
    ok = install_identity(Context),
    ok = install_predicate(Context),
    ok = install_skeleton_modules(Context),
    z_db:equery("SELECT setval('rsc_id_seq', m) FROM (select 1 + max(id) as m from rsc) sub", Context),
    lager:info("~p: Install done.", [Host]),
    ok.

%% @doc Install all modules for the site.
%% The list of modules is read from either the site config file, 
%% under the key <tt>install_modules</tt>.
-spec install_modules(#context{}) -> ok.
install_modules(Context) ->
    Host = Context#context.host,
    {ok, Config} = z_sites_manager:get_site_config(Host),
    Modules = [Host | proplists:get_value(install_modules, Config, [])],
    [install_module(M, Context) || M <- Modules],
    ok.

%% @doc Install all skeleton modules for the site.
%% If the <tt>install_modules</tt> is not defined then the standard list
%% of modules from the skeleton is installed.
-spec install_skeleton_modules(#context{}) -> ok.
install_skeleton_modules(Context) ->
    Host = Context#context.host,
    {ok, Config} = z_sites_manager:get_site_config(Host),
    case proplists:get_value(install_modules, Config, []) of
        [] ->
            install_module({skeleton, proplists:get_value(skeleton, Config)}, Context),
            ok;
        _ ->
            ok
    end.

install_module({skeleton, undefined}, _C) ->
    ok;
install_module({skeleton, S}, C) ->
    [install_module(M, C) || M <- get_skeleton_modules(S)];
install_module(M, Context) when is_atom(M); is_binary(M); is_list(M) ->
    case z_db:equery("update module set is_active = true where name = $1", [M], Context) of
        {ok, 1} = R ->
            R;
        {ok, 0} ->
            {ok, 1} = z_db:equery("insert into module (name, is_active) values ($1, true)", [M], Context)
    end.

-spec get_skeleton_modules(Skeleton::atom()) -> list().
get_skeleton_modules(empty) ->
    [
     mod_base,
     mod_menu,
     mod_oauth,
     mod_search,
     mod_oembed,
     mod_signal,
     mod_mqtt,
     mod_logging,
     mod_l10n,

     mod_authentication,
     mod_acl_adminonly,
     mod_editor_tinymce,

     mod_admin,
     mod_admin_category,
     mod_admin_config,
     mod_admin_identity,
     mod_admin_modules,
     mod_admin_predicate,

     mod_media_exif
    ];
get_skeleton_modules(blog) ->
    [
     mod_base,
     mod_base_site,
     mod_menu,
     mod_oauth,
     mod_search,
     mod_oembed,
     mod_atom_feed,
     mod_translation,
     mod_signal,
     mod_logging,
     mod_mqtt,
     mod_l10n,

     mod_seo,
     mod_seo_sitemap,

     mod_authentication,
     mod_acl_adminonly,
     mod_editor_tinymce,

     mod_admin,
     mod_admin_category,
     mod_admin_config,
     mod_admin_identity,
     mod_admin_modules,
     mod_admin_predicate,

     mod_media_exif,

     mod_comment,
     mod_bootstrap
    ];
get_skeleton_modules(_) ->
    %% nodb | undefined | OtherUnknown -> []
    []. 


install_category(C) ->
    lager:info("Inserting categories"),
    %% The egg has to lay a fk-checked chicken here, so the insertion order is sensitive.

    %% 1. Insert the categories "meta" and "category" 
    {ok, 2} = z_db:equery("
                    insert into hierarchy (name, id, parent_id, nr, lvl, lft, rght) 
                    values
                        ('$category', 115, null, 90000000, 1, 90000000, 92000000),
                        ('$category', 116, null, 91000000, 2, 91000000, 91000000)
                    ", C),

    %% make "category" a sub-category of "meta"
    {ok, 1} = z_db:equery("update hierarchy set parent_id = 115 where id = 116", C),

    %% "http://purl.org/dc/terms/DCMIType" ?
    {ok, 1} = z_db:equery("
            insert into rsc (id, is_protected, visible_for, category_id, name, uri, props)
            values (116, true, 0, 116, 'category', $1, $2)
            ", [    undefined, 
                    ?DB_PROPS([{title, {trans, [{en, <<"Category">>}, {nl, <<"Categorie">>}]}}])
                ], C),

    {ok, 1} = z_db:equery("
            insert into rsc (id, is_protected, visible_for, category_id, name, uri, props)
            values (115, true, 0, 116, 'meta', $1, $2)
            ", [    undefined, 
                    ?DB_PROPS([{title, {trans, [{en, <<"Meta">>}, {nl, <<"Meta">>}]}}])
                ], C),
    
    %% Now that we have the category "category" we can insert all other categories.
    Cats = [
        {101,undefined,  1,1,1,1, other,       true,  undefined,                                   [{title, {trans, [{en, <<"Uncategorized">>}, {nl, <<"Zonder categorie">>}]}}] },

        {104,undefined,  2,1,2,4, text,        false, "http://purl.org/dc/dcmitype/Text",          [{title, {trans, [{en, <<"Text">>}, {nl, <<"Tekst">>}]}}] },
            {106,104,    3,2,3,3, article,     false, undefined,                                   [{title, {trans, [{en, <<"Article">>}, {nl, <<"Artikel">>}]}}] },
                {109,106,4,3,4,4, news,        false, undefined,                                   [{title, {trans, [{en, <<"News">>}, {nl, <<"Nieuws">>}]}}] },

        {102,undefined,  5,1,5,5, person,      true,  undefined,                                   [{title, {trans, [{en, <<"Person">>}, {nl, <<"Persoon">>}]}}] },

        {119,undefined,  6,1,6,7, location,    false, undefined,                                   [{title, {trans, [{en, <<"Location">>}, {nl, <<"Locatie">>}]}}] },
            {107,119,    7,2,7,7, website,     false, undefined,                                   [{title, {trans, [{en, <<"Website">>}, {nl, <<"Website">>}]}}] },

        {108, undefined, 8,1,8,8, event,       false, "http://purl.org/dc/dcmitype/Event",         [{title, {trans, [{en, <<"Event">>}, {nl, <<"Evenement">>}]}}] },

        {103,undefined,  9,1,9,9, artifact,    false, "http://purl.org/dc/dcmitype/PhysicalObject",[{title, {trans, [{en, <<"Artifact">>}, {nl, <<"Artefact">>}]}}] },

        {110,undefined,  10,1,10,14, media,       true,  "http://purl.org/dc/dcmitype/Image",         [{title, {trans, [{en, <<"Media">>}, {nl, <<"Media">>}]}}] }, 
            {111,110,    11,2,11,11, image,       true,  "http://purl.org/dc/dcmitype/StillImage",    [{title, {trans, [{en, <<"Image">>}, {nl, <<"Afbeelding">>}]}}] },
            {112,110,    12,2,12,12, video,       true,  "http://purl.org/dc/dcmitype/MovingImage",   [{title, {trans, [{en, <<"Video">>}, {nl, <<"Video">>}]}}] },
            {113,110,    13,2,13,13, audio,       true,  "http://purl.org/dc/dcmitype/Sound",         [{title, {trans, [{en, <<"Audio">>}, {nl, <<"Geluid">>}]}}] },
            {114,110,    14,2,14,14, document,    true,  undefined,							         [{title, {trans, [{en, <<"Document">>}, {nl, <<"Document">>}]}}] },

        {120,undefined,  15,1,15,16, collection,  false, "http://purl.org/dc/dcmitype/Collection",    [{title, {trans, [{en, <<"Collection">>}, {nl, <<"Collectie">>}]}}] },
            {121,120,    16,2,16,16, 'query',     false, "http://purl.org/dc/dcmitype/Dataset",       [{title, {trans, [{en, <<"Search query">>}, {nl, <<"Zoekopdracht">>}]}}] },

        {122,undefined,  17,1,17,18, categorization,true,undefined,                                   [{title, {trans, [{en, <<"Categorization">>}, {nl, <<"Categorisatie">>}]}}] },
            {123,122,    18,2,18,18, keyword,     true,  undefined,                                   [{title, {trans, [{en, <<"Keyword">>}, {nl, <<"Trefwoord">>}]}}] },

        % 115. Meta (see above)
            % 116. Category (see above)
            {117,115,92,2,92,92,predicate,true,undefined,[{title,{trans,[{en,<<"Predicate">>},{nl,<<"Predikaat">>}]}}]},{124,undefined,19,1,19,19,charityorganization,false,"http://www.semanticweb.org/bravyto/ontologies/2016/9/Charity-Organization#CharityOrganization",[{title,{trans,[{en,<<"CharityOrganization">>}]}}]},{125,undefined,20,1,20,22,donation,false,"http://www.semanticweb.org/bravyto/ontologies/2016/9/Charity-Organization#Donation",[{title,{trans,[{en,<<"Donation">>}]}}]},{126,125,21,1,21,21,item,false,"http://www.semanticweb.org/bravyto/ontologies/2016/9/Charity-Organization#Item",[{title,{trans,[{en,<<"Item">>}]}}]},{127,125,22,1,22,22,money,false,"http://www.semanticweb.org/bravyto/ontologies/2016/9/Charity-Organization#Money",[{title,{trans,[{en,<<"Money">>}]}}]},{128,undefined,23,1,23,24,donor,false,"http://www.semanticweb.org/bravyto/ontologies/2016/9/Charity-Organization#Donor",[{title,{trans,[{en,<<"Donor">>}]}}]},{129,undefined,24,1,24,24,expense,false,"http://www.semanticweb.org/bravyto/ontologies/2016/9/Charity-Organization#Expense",[{title,{trans,[{en,<<"Expense">>}]}}]},{130,undefined,25,1,25,25,financialreport,false,"http://www.semanticweb.org/bravyto/ontologies/2016/9/Charity-Organization#FinancialReport",[{title,{trans,[{en,<<"FinancialReport">>}]}}]},{131,undefined,26,1,26,26,income,false,"http://www.semanticweb.org/bravyto/ontologies/2016/9/Charity-Organization#Income",[{title,{trans,[{en,<<"Income">>}]}}]},{132,undefined,27,1,27,27,membernotification,false,"http://www.semanticweb.org/bravyto/ontologies/2016/9/Charity-Organization#MemberNotification",[{title,{trans,[{en,<<"MemberNotification">>}]}}]},{133,undefined,28,1,28,31,program,false,"http://www.semanticweb.org/bravyto/ontologies/2016/9/Charity-Organization#Program",[{title,{trans,[{en,<<"Program">>}]}}]},{134,133,29,1,29,29,continousprogram,false,"http://www.semanticweb.org/bravyto/ontologies/2016/9/Charity-Organization#ContinousProgram",[{title,{trans,[{en,<<"ContinousProgram">>}]}}]},{135,133,30,1,30,30,eventualprogram,false,"http://www.semanticweb.org/bravyto/ontologies/2016/9/Charity-Organization#EventualProgram",[{title,{trans,[{en,<<"EventualProgram">>}]}}]},{136,133,31,1,31,31,periodicprogram,false,"http://www.semanticweb.org/bravyto/ontologies/2016/9/Charity-Organization#PeriodicProgram",[{title,{trans,[{en,<<"PeriodicProgram">>}]}}]},{137,undefined,32,1,32,32,storyboard,false,"http://www.semanticweb.org/bravyto/ontologies/2016/9/Charity-Organization#StoryBoard",[{title,{trans,[{en,<<"StoryBoard">>}]}}]},{138,undefined,33,1,33,37,target,false,"http://www.semanticweb.org/bravyto/ontologies/2016/9/Charity-Organization#Target",[{title,{trans,[{en,<<"Target">>}]}}]},{139,138,34,1,34,36,beneficiary,false,"http://www.semanticweb.org/bravyto/ontologies/2016/9/Charity-Organization#Beneficiary",[{title,{trans,[{en,<<"Beneficiary">>}]}}]},{140,139,35,1,35,35,individualbeneficiary,false,"http://www.semanticweb.org/bravyto/ontologies/2016/9/Charity-Organization#IndividualBeneficiary",[{title,{trans,[{en,<<"IndividualBeneficiary">>}]}}]},{141,139,36,1,36,36,institutionalbeneficiary,false,"http://www.semanticweb.org/bravyto/ontologies/2016/9/Charity-Organization#InstitutionalBeneficiary",[{title,{trans,[{en,<<"InstitutionalBeneficiary">>}]}}]},{142,138,37,1,37,37,product,false,"http://www.semanticweb.org/bravyto/ontologies/2016/9/Charity-Organization#Product",[{title,{trans,[{en,<<"Product">>}]}}]},{143,undefined,38,1,38,38,volunteer,false,"http://www.semanticweb.org/bravyto/ontologies/2016/9/Charity-Organization#Volunteer",[{title,{trans,[{en,<<"Volunteer">>}]}}]}
    ],

    InsertCatFun = fun({Id, ParentId, Nr, Lvl, Left, Right, Name, Protected, Uri, Props}) ->
        {ok, 1} = z_db:equery("
                insert into rsc (id, visible_for, category_id, is_protected, name, uri, props)
                values ($1, 0, 116, $2, $3, $4, $5)
                ", [ Id, Protected, Name, Uri, ?DB_PROPS(Props) ], C),
        {ok, 1} = z_db:equery("
                insert into hierarchy (name, id, parent_id, nr, lvl, lft, rght)
                values ('$category', $1, $2, $3, $4, $5, $6)", 
                [Id, ParentId, Nr*1000000, Lvl, Left*1000000, Right*1000000-1], C)
    end,
    lists:foreach(InsertCatFun, Cats),
    ok.
    

%% @doc Install some initial resources, most important is the system administrator
%% @todo Add the hostname to the uri
install_rsc(C) ->
    lager:info("Inserting base resources (admin, etc.)"),
    Rsc = [
        % id  vsfr  cat   protect name,         props
        [   1,  0,  102,  true,    "administrator",   ?DB_PROPS([{title,<<"Site Administrator">>}]) ]
    ],
    
    [ {ok,1} = z_db:equery("
            insert into rsc (id, visible_for, category_id, is_protected, name, props)
            values ($1, $2, $3, $4, $5, $6)
            ", R, C) || R <- Rsc ],
    {ok, _} = z_db:equery("update rsc set creator_id = 1, modifier_id = 1, is_published = true", C),
    ok.


%% @doc Install the admin user as an user.  Uses the hard coded password "admin" when no password defined in the environment.
install_identity(C) ->
    lager:info("Inserting username for the admin"),
    Hash = m_identity:hash([]),
    {ok, 1} = z_db:equery("
        insert into identity (rsc_id, type, key, is_unique, propb)
        values (1, 'username_pw', 'admin', true, $1)", [{term, Hash}], C),
    ok.
    

%% @doc Install some initial predicates, this list should be extended with common and useful predicates
%% See http://dublincore.org/documents/dcmi-terms/
%% @todo Extend and check this list.  Add allowed from/to categories.
install_predicate(C) ->
    lager:info("Inserting predicates"),
    Preds = [
        % id   protect name       uri                                                  props
        [ 300, true,   "about",    "http://www.w3.org/1999/02/22-rdf-syntax-ns#about",  ?DB_PROPS([{reversed, false},{title, {trans, [{en,"About"},    {nl,"Over"}]}}])],
        [ 301, true,   "author",   "http://purl.org/dc/terms/creator",                  ?DB_PROPS([{reversed, false},{title, {trans, [{en,"Author"},   {nl,"Auteur"}]}}])],
        [ 303, true,   "relation", "http://purl.org/dc/terms/relation",                 ?DB_PROPS([{reversed, false},{title, {trans, [{en,"Target"}, {nl,"Relatie"}]}}])],
        [ 304, true,   "depiction","http://xmlns.com/foaf/0.1/depiction",               ?DB_PROPS([{reversed, false},{title, {trans, [{en,"Depiction"},{nl,"Afbeelding"}]}}])],
        [ 308, true,   "subject",  "http://purl.org/dc/elements/1.1/subject",           ?DB_PROPS([{reversed, false},{title, {trans, [{en,"Keyword"},  {nl,"Trefwoord"}]}}])],
        [ 309, true,   "hasdocument", "http://zotonic.net/predicate/hasDocument",       ?DB_PROPS([{reversed, false},{title, {trans, [{en,"Document"}, {nl,"Document"}]}}])],
		[ 310, true,   "haspart",  "http://purl.org/dc/terms/hasPart",					?DB_PROPS([{reversed, false},{title, {trans, [{en,"Contains"}, {nl,"Bevat"}]}}])],[311,true,"carry_out","http://www.semanticweb.org/bravyto/ontologies/2016/9/Charity-Organization#carry_out",?DB_PROPS([{reversed,false},{title,{trans,[{en,"carry_out"}]}}])],[312,true,"channeled_through","http://www.semanticweb.org/bravyto/ontologies/2016/9/Charity-Organization#channeled_through",?DB_PROPS([{reversed,false},{title,{trans,[{en,"channeled_through"}]}}])],[313,true,"contribute_to","http://www.semanticweb.org/bravyto/ontologies/2016/9/Charity-Organization#contribute_to",?DB_PROPS([{reversed,false},{title,{trans,[{en,"contribute_to"}]}}])],[314,true,"distribute_to","http://www.semanticweb.org/bravyto/ontologies/2016/9/Charity-Organization#distribute_to",?DB_PROPS([{reversed,false},{title,{trans,[{en,"distribute_to"}]}}])],[315,true,"donate","http://www.semanticweb.org/bravyto/ontologies/2016/9/Charity-Organization#donate",?DB_PROPS([{reversed,false},{title,{trans,[{en,"donate"}]}}])],[316,true,"forprogram","http://www.semanticweb.org/bravyto/ontologies/2016/9/Charity-Organization#forProgram",?DB_PROPS([{reversed,false},{title,{trans,[{en,"forProgram"}]}}])],[317,true,"generate","http://www.semanticweb.org/bravyto/ontologies/2016/9/Charity-Organization#generate",?DB_PROPS([{reversed,false},{title,{trans,[{en,"generate"}]}}])],[318,true,"getnotification","http://www.semanticweb.org/bravyto/ontologies/2016/9/Charity-Organization#getNotification",?DB_PROPS([{reversed,false},{title,{trans,[{en,"getNotification"}]}}])],[319,true,"given_to","http://www.semanticweb.org/bravyto/ontologies/2016/9/Charity-Organization#given_to",?DB_PROPS([{reversed,false},{title,{trans,[{en,"given_to"}]}}])],[320,true,"hasfinancialreport","http://www.semanticweb.org/bravyto/ontologies/2016/9/Charity-Organization#hasFinancialReport",?DB_PROPS([{reversed,false},{title,{trans,[{en,"hasFinancialReport"}]}}])],[321,true,"hasstoryboard","http://www.semanticweb.org/bravyto/ontologies/2016/9/Charity-Organization#hasStoryBoard",?DB_PROPS([{reversed,false},{title,{trans,[{en,"hasStoryBoard"}]}}])],[322,true,"partof","http://www.semanticweb.org/bravyto/ontologies/2016/9/Charity-Organization#partOf",?DB_PROPS([{reversed,false},{title,{trans,[{en,"partOf"}]}}])],[323,true,"relatedwith","http://www.semanticweb.org/bravyto/ontologies/2016/9/Charity-Organization#relatedWith",?DB_PROPS([{reversed,false},{title,{trans,[{en,"relatedWith"}]}}])]
    ],

    CatId   = z_db:q1("select id from rsc where name = 'predicate'", C),
    
    [ {ok,1} = z_db:equery("
            insert into rsc (id, visible_for, is_protected, name, uri, props, category_id, is_published, creator_id, modifier_id)
            values ($1, 0, $2, $3, $4, $5, $6, true, 1, 1)
            ", R ++ [CatId], C) || R <- Preds],

    ObjSubj = [
        [300, true,  104], %  text   -> about     -> _
        [301, false, 102], %  _      -> author    -> person
        [304, false, 110], %  _      -> depiction -> image
	[303, true, 102],

        [308, true,  104], %  text     -> subject   -> _
        [308, true,  102], %  person   -> subject   -> _
        [308, true,  119], %  location -> subject   -> _
        [308, true,  108], %  event    -> subject   -> _
        [308, true,  103], %  artifact -> subject   -> _
        [308, true,  110], %  media    -> subject   -> _
        [308, true,  114], %  collection -> subject   -> _
        [308, false, 123], %  _      -> subject   -> keyword

        [309, true,  102], %  person   -> document -> _
        [309, true,  103], %  artifact -> document -> _
        [309, true,  104], %  text     -> document -> _
        [309, true,  119], %  location -> document -> _
        [309, false, 114], %  _        -> document -> media

        [310, true,  120],[311,true,124],[311,false,133],[312,true,125],[312,false,133],[313,true,133],[313,false,139],[314,true,125],[314,false,138],[315,true,128],[315,false,125],[316,true,125],[316,false,133],[317,true,133],[317,false,130],[318,true,128],[318,false,132],[319,true,130],[319,false,128],[320,true,133],[320,false,130],[321,true,133],[321,false,137],[322,true,128],[322,false,130],[323,true,129],[323,false,132]

    
    ],
    
    [ {ok, 1} = z_db:equery("
            insert into predicate_category (predicate_id, is_subject, category_id) 
            values ($1, $2, $3)", OS, C) || OS <- ObjSubj ],
    ok.

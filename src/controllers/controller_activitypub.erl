%% @author Driebit
%% @copyright 2025 Driebit

-module(controller_activitypub).
-author("Driebit <tech@driebit.nl>").

-export([
    resource_exists/1,
    forbidden/1,
    service_available/1,
    allowed_methods/1,
    content_types_provided/1,

    process/4
]).

-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_mod_driebit_rdf/include/driebit_rdf.hrl").

resource_exists(Context) ->
    ContextQs = z_context:ensure_qs(Context),
    Id = get_id(ContextQs),
    {m_rsc:exists(Id, ContextQs), ContextQs}.

forbidden(Context) ->
    ContextQs = z_context:ensure_qs(Context),
    Id = get_id(ContextQs),
    {not z_acl:rsc_visible(Id, ContextQs), ContextQs}.

service_available(Context) ->
    ContextQs = z_context:ensure_qs(Context),
    % unlike 'controller_api', this doesn't look at the configuration, but
    % instead it unconditionally exposes the context between domains.
    % This makes sharing content much easier.
    ContextCh = z_context:set_cors_headers([{<<"access-control-allow-origin">>, <<"*">>}], ContextQs),
    z_context:logger_md(ContextCh),
    % The 'id' and 'collection' arguments are required:
    Id = get_id(ContextCh),
    Collection = get_collection(ContextCh),
    if
        Id =:= undefined ->
            ?LOG_WARNING(#{
                text => <<"missing necessary parameter: id">>,
                in => controller_activitypub,
                path => z_context:get(zotonic_dispatch_path, ContextCh)
            }),
            {{halt, 400}, ContextCh};
        Collection =:= undefined ->
            ?LOG_WARNING(#{
                text => <<"missing necessary parameter: collection">>,
                in => controller_activitypub,
                path => z_context:get(zotonic_dispatch_path, ContextCh)
            }),
            {{halt, 400}, ContextCh};
        true ->
            {true, ContextCh}
    end.

allowed_methods(Context) ->
    % TODO: add support for POSTs (as well as 'content_types_accepted')
    {[<<"GET">>], Context}.

content_types_provided(Context) ->
    {
        [
            {<<"application">>, <<"activity+json">>, []},
            {<<"application">>, <<"ld+json">>, [{<<"profile">>, <<"\"https://www.w3.org/ns/activitystreams\"">>}]}
        ],
        Context
    }.

process(_Method, _AcceptedCT, _ProvidedCT, Context) ->
    ContextQs = z_context:ensure_qs(Context),
    RscId = m_rsc:rid(get_id(ContextQs), ContextQs),
    Collection = get_collection(ContextQs),
    case rdf_activitypub:collection_to_rdf(RscId, Collection, ContextQs) of
        {ok, Result} when is_binary(Result) ->
            {Result, ContextQs};
        Error ->
            ?LOG_ERROR(#{
                text => <<"unexpected conversion error">>,
                in => controller_activitypub,
                rsc_id => RscId,
                collection => Collection,
                message => Error
            }),
            {{halt, 500}, ContextQs}
    end.

get_id(Context) ->
    case get_argument(id, Context) of
        undefined -> z_acl:user(Context);
        Id -> z_convert:to_binary(Id)
    end.

get_collection(Context) ->
    z_convert:to_atom(get_argument(collection, Context)).

get_argument(ArgName, Context) ->
    case z_context:get(ArgName, Context) of
        undefined -> z_context:get_q(ArgName, Context);
        Value -> Value
    end.

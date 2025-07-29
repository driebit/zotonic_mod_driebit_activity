%% @author Driebit
%% @copyright 2025 Driebit
%% @doc Default implementation for ActivityPub's ontology
%% See also: https://www.w3.org/TR/activitypub/
%% @end

-module(rdf_activitypub).
-author("Driebit <tech@driebit.nl>").

-include_lib("zotonic_core/include/zotonic.hrl").
-include("zotonic_mod_driebit_rdf/include/driebit_rdf.hrl").

-export([
    triple_to_rdf/2,
    collection_to_rdf/3
]).


-spec triple_to_rdf(#triple_to_rdf{}, z:context()) ->
    {ok, #rdf_triple{}} | {ok, list(#rdf_triple{})} | {error, term()} | undefined.
% The ID property is always there, so we take this opportunity to add activitypub's
% special collections to actor objects, see: https://www.w3.org/TR/activitypub/#actor-objects
% Moreover, since this ontology is a superset of 'activitystreams', we delegate
% each notification to its module as well.
triple_to_rdf(
    #triple_to_rdf{
        rsc_id = RscId,
        link_type = property,
        ontology = activitypub,
        category = person,
        link_name = <<"id">>
    } = TripleToRdf,
    Context
) ->
    with_actor_collections(TripleToRdf, RscId, Context);
triple_to_rdf(
    #triple_to_rdf{
        rsc_id = RscId,
        link_type = property,
        ontology = activitypub,
        category = acl_collaboration_group,
        link_name = <<"id">>
    } = TripleToRdf,
    Context
) ->
    with_actor_collections(TripleToRdf, RscId, Context);
triple_to_rdf(
    #triple_to_rdf{
        rsc_id = RscId,
        link_type = property,
        ontology = activitypub,
        category = organisation,
        link_name = <<"id">>
    } = TripleToRdf,
    Context
) ->
    with_actor_collections(TripleToRdf, RscId, Context);
triple_to_rdf(#triple_to_rdf{ontology = activitypub} = TripleToRdf, Context) ->
    delegate_notification(TripleToRdf, Context);
triple_to_rdf(_TripleToRdf, _Context) ->
    undefined.


delegate_notification(TripleToRdf, Context) ->
    rdf_activitystreams:triple_to_rdf(
        TripleToRdf#triple_to_rdf{ontology = activitystreams},
        Context
    ).

with_actor_collections(TripleToRdf, RscId, Context) ->
    case delegate_notification(TripleToRdf, Context) of
        {ok, Triples} when is_list(Triples) ->
            {ok, Triples ++ actor_collections(RscId, Context)};
        {ok, Triple} ->
            {ok, [Triple | actor_collections(RscId, Context)]};
        Otherwise ->
            Otherwise
    end.

actor_collections(RscId, Context) ->
    lists:map(
        fun (CollectionName) ->
            #rdf_triple{
                subject = rdf_utils:resolve_iri(RscId, Context),
                predicate = rdf_activitystreams:namespaced_iri(CollectionName),
                object = z_dispatcher:url_for(
                    activitypub_collection,
                    [
                        {id, RscId},
                        {collection, CollectionName},
                        {absolute_url, true}
                    ],
                    Context
                )
            }
        end,
        [
            inbox,
            outbox,
            following,
            followers,
            liked
        ]
    ).

%% @doc Produces an 'activitypub' serialized view of a special collection for
%% actors. See also: https://www.w3.org/TR/activitypub/#actor-objects
-spec collection_to_rdf(#triple_to_rdf{}, atom(), z:context()) -> {ok, binary()} | {error, term()}.
collection_to_rdf(RscId, CollectionName, Context) ->
    CollectionIRI = z_dispatcher:url_for(
        activitypub_collection,
        [
            {id, RscId},
            {collection, CollectionName},
            {absolute_url, true}
        ],
        Context
    ),
    CollectionItems = case CollectionName of
        inbox -> m_activity:inbox_activities(RscId, Context);
        outbox -> m_activity:outbox_activities(RscId, Context);
        following -> m_activity:following_objects(RscId, Context);
        followers -> m_activity:followers_objects(RscId, Context);
        liked -> m_activity:liked_objects(RscId, Context);
        _ -> []
    end,
    RdfGraph = sets:from_list([
        #rdf_triple{
            subject = CollectionIRI,
            predicate = rdf_xsd:rdf_namespaced_iri(type),
            object = rdf_activitystreams:namespaced_iri(<<"OrderedCollection">>)
        },
        #rdf_triple{
            subject = CollectionIRI,
            predicate = rdf_activitystreams:namespaced_iri(totalItems),
            object = rdf_utils:to_literal(length(CollectionItems), Context)
        } |
        lists:map(
            fun (CollectionItem) ->
                #rdf_triple{
                    subject = CollectionIRI,
                    predicate = rdf_activitystreams:namespaced_iri(orderedItems),
                    object = rdf_utils:resolve_iri(CollectionItem, Context)
                }
            end,
            CollectionItems
        )
    ]),
    mod_driebit_rdf:serialize_rdf(
        RdfGraph,
        activitypub,
        #{undefined => rdf_activitystreams:namespace_iri()},
        Context
    ).

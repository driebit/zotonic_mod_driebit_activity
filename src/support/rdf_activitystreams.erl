%% @author Driebit
%% @copyright 2025 Driebit
%% @doc Default implementation for the ActivityStreams 2.0 Terms and Activity Vocabulary
%% See also: https://www.w3.org/ns/activitystreams
%% See also: https://www.w3.org/TR/activitystreams-vocabulary/
%% @end

-module(rdf_activitystreams).
-author("Driebit <tech@driebit.nl>").

-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_mod_driebit_rdf/include/driebit_rdf.hrl").

-export([
    triple_to_rdf/2,

    property_to_rdf/5,
    outedge_to_rdf/5,
    inedge_to_rdf/5,

    namespace_iri/0,
    lpd_namespace_iri/0,
    vcard_namespace_iri/0,

    namespaced_iri/1,
    type_triple/3
]).


-spec triple_to_rdf(#triple_to_rdf{}, z:context()) ->
    {ok, #rdf_triple{}} | {ok, list(#rdf_triple{})} | {error, term()} | undefined.
triple_to_rdf(#triple_to_rdf{link_type = property, ontology = activitystreams} = TripleToRdf, Context) ->
    property_to_rdf(
        TripleToRdf#triple_to_rdf.rsc_id,
        TripleToRdf#triple_to_rdf.category,
        TripleToRdf#triple_to_rdf.link_name,
        TripleToRdf#triple_to_rdf.value,
        Context
    );
triple_to_rdf(#triple_to_rdf{link_type = outgoing_edge, ontology = activitystreams} = TripleToRdf, Context) ->
    outedge_to_rdf(
        TripleToRdf#triple_to_rdf.rsc_id,
        TripleToRdf#triple_to_rdf.category,
        TripleToRdf#triple_to_rdf.link_name,
        TripleToRdf#triple_to_rdf.value,
        Context
    );
triple_to_rdf(#triple_to_rdf{link_type = incoming_edge, ontology = activitystreams} = TripleToRdf, Context) ->
    inedge_to_rdf(
        TripleToRdf#triple_to_rdf.rsc_id,
        TripleToRdf#triple_to_rdf.category,
        TripleToRdf#triple_to_rdf.link_name,
        TripleToRdf#triple_to_rdf.value,
        Context
    );
triple_to_rdf(_TripleToRdf, _Context) ->
    undefined.

-spec property_to_rdf(m_rsc:resource_id(), atom(), binary(), term() | m_rsc:resource_id(), z:context()) ->
    {ok, #rdf_triple{}} | {ok, list(#rdf_triple{})} | {error, term()} | undefined.
property_to_rdf(_RscId, _Category, _PropName, undefined, _Context) ->
    % Ignore empty values
    undefined;

% The ID property doesn't produce any triple, but is always there,
% so we take this opportunity to determine the resource type instead:
property_to_rdf(RscId, text, <<"id">>, RscId, Context) ->
    {ok, type_triple(RscId, <<"Article">>, Context)};
property_to_rdf(RscId, audio, <<"id">>, RscId, Context) ->
    {ok, type_triple(RscId, <<"Audio">>, Context)};
property_to_rdf(RscId, collection, <<"id">>, RscId, Context) ->
    {ok,
        [
            type_triple(RscId, <<"OrderedCollection">>, Context),
            % for collection type we immediately calculate the total items too:
            rdf_utils:value_triple(
                RscId,
                namespaced_iri(totalItems),
                length(m_edge:objects(RscId, haspart, Context)),
                Context
            )
        ]
    };
% TODO: OrderedCollectionPage for 'query'?
property_to_rdf(RscId, document, <<"id">>, RscId, Context) ->
    {ok, type_triple(RscId, <<"Document">>, Context)};
property_to_rdf(RscId, event, <<"id">>, RscId, Context) ->
    {ok, type_triple(RscId, <<"Event">>, Context)};
property_to_rdf(RscId, acl_collaboration_group, <<"id">>, RscId, Context) ->
    {ok, type_triple(RscId, <<"Group">>, Context)};
property_to_rdf(RscId, image, <<"id">>, RscId, Context) ->
    {ok, type_triple(RscId, <<"Image">>, Context)};
property_to_rdf(RscId, organisation, <<"id">>, RscId, Context) ->
    {ok, type_triple(RscId, <<"Organization">>, Context)};
property_to_rdf(RscId, website, <<"id">>, RscId, Context) ->
    {ok, type_triple(RscId, <<"Page">>, Context)};
property_to_rdf(RscId, person, <<"id">>, RscId, Context) ->
    {ok, type_triple(RscId, <<"Person">>, Context)};
property_to_rdf(RscId, location, <<"id">>, RscId, Context) ->
    {ok, type_triple(RscId, <<"Place">>, Context)};
% Activities and subtypes:
property_to_rdf(RscId, activity, <<"id">>, RscId, Context) ->
    {ok, type_triple(RscId, <<"Activity">>, Context)};
property_to_rdf(RscId, activity_accept, <<"id">>, RscId, Context) ->
    {ok, type_triple(RscId, <<"Accept">>, Context)};
property_to_rdf(RscId, activity_add, <<"id">>, RscId, Context) ->
    {ok, type_triple(RscId, <<"Add">>, Context)};
property_to_rdf(RscId, activity_announce, <<"id">>, RscId, Context) ->
    {ok, type_triple(RscId, <<"Announce">>, Context)};
property_to_rdf(RscId, activity_arrive, <<"id">>, RscId, Context) ->
    {ok, type_triple(RscId, <<"Arrive">>, Context)};
property_to_rdf(RscId, activity_block, <<"id">>, RscId, Context) ->
    {ok, type_triple(RscId, <<"Block">>, Context)};
property_to_rdf(RscId, activity_create, <<"id">>, RscId, Context) ->
    {ok, type_triple(RscId, <<"Create">>, Context)};
property_to_rdf(RscId, activity_delete, <<"id">>, RscId, Context) ->
    {ok, type_triple(RscId, <<"Delete">>, Context)};
property_to_rdf(RscId, activity_dislike, <<"id">>, RscId, Context) ->
    {ok, type_triple(RscId, <<"Dislike">>, Context)};
property_to_rdf(RscId, activity_follow, <<"id">>, RscId, Context) ->
    {ok, type_triple(RscId, <<"Follow">>, Context)};
property_to_rdf(RscId, activity_flag, <<"id">>, RscId, Context) ->
    {ok, type_triple(RscId, <<"Flag">>, Context)};
property_to_rdf(RscId, activity_ignore, <<"id">>, RscId, Context) ->
    {ok, type_triple(RscId, <<"Ignore">>, Context)};
property_to_rdf(RscId, activity_invite, <<"id">>, RscId, Context) ->
    {ok, type_triple(RscId, <<"Invite">>, Context)};
property_to_rdf(RscId, activity_join, <<"id">>, RscId, Context) ->
    {ok, type_triple(RscId, <<"Join">>, Context)};
property_to_rdf(RscId, activity_leave, <<"id">>, RscId, Context) ->
    {ok, type_triple(RscId, <<"Leave">>, Context)};
property_to_rdf(RscId, activity_like, <<"id">>, RscId, Context) ->
    {ok, type_triple(RscId, <<"Like">>, Context)};
property_to_rdf(RscId, activity_listen, <<"id">>, RscId, Context) ->
    {ok, type_triple(RscId, <<"Listen">>, Context)};
property_to_rdf(RscId, activity_move, <<"id">>, RscId, Context) ->
    {ok, type_triple(RscId, <<"Move">>, Context)};
property_to_rdf(RscId, activity_offer, <<"id">>, RscId, Context) ->
    {ok, type_triple(RscId, <<"Offer">>, Context)};
property_to_rdf(RscId, activity_read, <<"id">>, RscId, Context) ->
    {ok, type_triple(RscId, <<"Read">>, Context)};
property_to_rdf(RscId, activity_reject, <<"id">>, RscId, Context) ->
    {ok, type_triple(RscId, <<"Reject">>, Context)};
property_to_rdf(RscId, activity_remove, <<"id">>, RscId, Context) ->
    {ok, type_triple(RscId, <<"Remove">>, Context)};
property_to_rdf(RscId, activity_tentativeAccept, <<"id">>, RscId, Context) ->
    {ok, type_triple(RscId, <<"TentativeAccept">>, Context)};
property_to_rdf(RscId, activity_tentativeReject, <<"id">>, RscId, Context) ->
    {ok, type_triple(RscId, <<"TentativeReject">>, Context)};
property_to_rdf(RscId, activity_travel, <<"id">>, RscId, Context) ->
    {ok, type_triple(RscId, <<"Travel">>, Context)};
property_to_rdf(RscId, activity_undo, <<"id">>, RscId, Context) ->
    {ok, type_triple(RscId, <<"Undo">>, Context)};
property_to_rdf(RscId, activity_update, <<"id">>, RscId, Context) ->
    {ok, type_triple(RscId, <<"Update">>, Context)};
property_to_rdf(RscId, activity_view, <<"id">>, RscId, Context) ->
    {ok, type_triple(RscId, <<"View">>, Context)};
% Every resource has the type 'Object', if there wasn't a more specific subtype
property_to_rdf(RscId, _, <<"id">>, RscId, Context) ->
    {ok, type_triple(RscId, <<"Object">>, Context)};

% valid for any Object:
property_to_rdf(RscId, _Category, <<"body">>, Value, Context) ->
    {ok, rdf_utils:value_triple(
        RscId,
        namespaced_iri(content),
        Value,
        Context
    )};
property_to_rdf(RscId, _Category, <<"title">>, Value, Context) ->
    {ok, rdf_utils:value_triple(
        RscId,
        namespaced_iri(name),
        Value,
        Context
    )};
property_to_rdf(RscId, _Category, <<"date_end">>, Value, Context) ->
    {ok, rdf_utils:value_triple(
        RscId,
        namespaced_iri(endTime),
        Value,
        Context
    )};
property_to_rdf(RscId, _Category, <<"creator_id">>, CreatorId, Context) ->
    {ok, rdf_utils:resolve_triple(
        RscId,
        namespaced_iri(endTime),
        CreatorId,
        Context
    )};
property_to_rdf(RscId, _Category, <<"publication_start">>, Value, Context) ->
    {ok, rdf_utils:value_triple(
        RscId,
        namespaced_iri(published),
        Value,
        Context
    )};
property_to_rdf(RscId, _Category, <<"date_start">>, Value, Context) ->
    {ok, rdf_utils:value_triple(
        RscId,
        namespaced_iri(startTime),
        Value,
        Context
    )};
property_to_rdf(RscId, _Category, <<"summary">>, Value, Context) ->
    {ok, rdf_utils:value_triple(
        RscId,
        namespaced_iri(summary),
        Value,
        Context
    )};
property_to_rdf(RscId, _Category, <<"modified">>, Value, Context) ->
    {ok, rdf_utils:value_triple(
        RscId,
        namespaced_iri(updated),
        Value,
        Context
    )};
property_to_rdf(RscId, _Category, <<"website">>, Value, Context) ->
    {ok, rdf_utils:value_triple(
        RscId,
        namespaced_iri(url),
        Value,
        Context
    )};
property_to_rdf(RscId, _Category, <<"mime">>, Value, Context) ->
    {ok, rdf_utils:value_triple(
        RscId,
        namespaced_iri(mediaType),
        Value,
        Context
    )};

property_to_rdf(RscId, Category, PropName, Value, Context) ->
    % If there was no match, try with parent categories (when possible):
    case lists:delete(Category, m_category:is_a(Category, Context)) of
        [] ->
            undefined;
        ParentCatList ->
            ParentCat = lists:last(ParentCatList),
            property_to_rdf(RscId, ParentCat, PropName, Value, Context)
    end.

-spec outedge_to_rdf(m_rsc:resource_id(), atom(), binary(), term() | m_rsc:resource_id(), z:context()) ->
    {ok, #rdf_triple{}} | {ok, list(#rdf_triple{})} | {error, term()} | undefined.
outedge_to_rdf(RscId, activity, <<"has_activity_actor">>, ObjectId, Context) ->
    {ok, rdf_utils:resolve_triple(
        RscId,
        namespaced_iri(actor),
        ObjectId,
        Context
    )};
outedge_to_rdf(RscId, activity, <<"has_activity_object">>, ObjectId, Context) ->
    {ok, rdf_utils:resolve_triple(
        RscId,
        namespaced_iri(object),
        ObjectId,
        Context
    )};
outedge_to_rdf(RscId, activity, <<"has_activity_target">>, ObjectId, Context) ->
    {ok, rdf_utils:resolve_triple(
        RscId,
        namespaced_iri(target),
        ObjectId,
        Context
    )};
outedge_to_rdf(RscId, activity, <<"has_activity_result">>, ObjectId, Context) ->
    {ok, rdf_utils:resolve_triple(
        RscId,
        namespaced_iri(result),
        ObjectId,
        Context
    )};
outedge_to_rdf(RscId, activity, <<"has_activity_origin">>, ObjectId, Context) ->
    {ok, rdf_utils:resolve_triple(
        RscId,
        namespaced_iri(origin),
        ObjectId,
        Context
    )};
outedge_to_rdf(RscId, activity, <<"has_activity_instrument">>, ObjectId, Context) ->
    {ok, rdf_utils:resolve_triple(
        RscId,
        namespaced_iri(instrument),
        ObjectId,
        Context
    )};
outedge_to_rdf(RscId, collection, <<"haspart">>, ObjectId, Context) ->
    {ok, rdf_utils:resolve_triple(
        RscId,
        namespaced_iri(orderedItems),
        ObjectId,
        Context
    )};

% valid for any Object:
outedge_to_rdf(RscId, _Category, <<"has_activity_audience_to">>, ObjectId, Context) ->
    {ok,
        [
            rdf_utils:resolve_triple(
                RscId,
                namespaced_iri(audience),
                ObjectId,
                Context
            ),
            rdf_utils:resolve_triple(
                RscId,
                namespaced_iri(to),
                ObjectId,
                Context
            )
        ]
    };
outedge_to_rdf(RscId, _Category, <<"has_activity_audience_bto">>, ObjectId, Context) ->
    {ok,
        [
            rdf_utils:resolve_triple(
                RscId,
                namespaced_iri(audience),
                ObjectId,
                Context
            ),
            rdf_utils:resolve_triple(
                RscId,
                namespaced_iri(bto),
                ObjectId,
                Context
            )
        ]
    };
outedge_to_rdf(RscId, _Category, <<"has_activity_audience_cc">>, ObjectId, Context) ->
    {ok,
        [
            rdf_utils:resolve_triple(
                RscId,
                namespaced_iri(audience),
                ObjectId,
                Context
            ),
            rdf_utils:resolve_triple(
                RscId,
                namespaced_iri(cc),
                ObjectId,
                Context
            )
        ]
    };
outedge_to_rdf(RscId, _Category, <<"has_activity_audience_bcc">>, ObjectId, Context) ->
    {ok,
        [
            rdf_utils:resolve_triple(
                RscId,
                namespaced_iri(audience),
                ObjectId,
                Context
            ),
            rdf_utils:resolve_triple(
                RscId,
                namespaced_iri(bcc),
                ObjectId,
                Context
            )
        ]
    };
outedge_to_rdf(RscId, _Category, <<"hasicon">>, ObjectId, Context) ->
    {ok, rdf_utils:resolve_triple(
        RscId,
        namespaced_iri(image),
        ObjectId,
        Context
    )};
outedge_to_rdf(RscId, _Category, <<"depiction">>, ObjectId, Context) ->
    {ok, rdf_utils:resolve_triple(
        RscId,
        namespaced_iri(image),
        ObjectId,
        Context
    )};
outedge_to_rdf(RscId, Category, PredName, Value, Context) ->
    % If there was no match, try with parent categories (when possible):
    case lists:delete(Category, m_category:is_a(Category, Context)) of
        [] ->
            undefined;
        ParentCatList ->
            ParentCat = lists:last(ParentCatList),
            outedge_to_rdf(RscId, ParentCat, PredName, Value, Context)
    end.

-spec inedge_to_rdf(m_rsc:resource_id(), atom(), binary(), term() | m_rsc:resource_id(), z:context()) ->
    {ok, #rdf_triple{}} | {ok, list(#rdf_triple{})} | {error, term()} | undefined.

% valid for any Object:
inedge_to_rdf(RscId, _Category, <<"author">>, ObjectId, Context) ->
    {ok, rdf_utils:resolve_triple(
        RscId,
        namespaced_iri(attributedTo),
        ObjectId,
        Context
    )};
inedge_to_rdf(RscId, Category, PredName, Value, Context) ->
    % If there was no match, try with parent categories (when possible):
    case lists:delete(Category, m_category:is_a(Category, Context)) of
        [] ->
            undefined;
        ParentCatList ->
            ParentCat = lists:last(ParentCatList),
            inedge_to_rdf(RscId, ParentCat, PredName, Value, Context)
    end.

-spec namespace_iri() -> iri().
namespace_iri() ->
    <<"https://www.w3.org/ns/activitystreams#">>.

-spec lpd_namespace_iri() -> iri().
lpd_namespace_iri() ->
    <<"http://www.w3.org/ns/ldp#">>.

-spec vcard_namespace_iri() -> iri().
vcard_namespace_iri() ->
    <<"http://www.w3.org/2006/vcard/ns#">>.


-spec namespaced_iri(atom() | binary()) -> iri().
namespaced_iri(TermName) when is_atom(TermName) ->
    namespaced_iri(z_convert:to_binary(TermName));
namespaced_iri(TermName) when is_binary(TermName) ->
    <<(namespace_iri())/binary, TermName/binary>>.

-spec type_triple(m_rsc:resource(), iri(), z:context()) -> #rdf_triple{}.
type_triple(RscId, SchemaType, Context) ->
    #rdf_triple{
        subject = rdf_utils:resolve_iri(RscId, Context),
        predicate = rdf_xsd:rdf_namespaced_iri(type),
        object = namespaced_iri(SchemaType)
    }.

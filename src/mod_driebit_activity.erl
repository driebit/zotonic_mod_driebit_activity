%% @author Driebit
%% @copyright 2025 Driebit

-module(mod_driebit_activity).
-author("Driebit <tech@driebit.nl>").

-mod_title("Driebit Activity module").
-mod_description("Tracking and managing user activities").

% Note: this was picked so that the priority is higher than mod_driebit_rdf's
-mod_prio(450).
-mod_schema(1).
-mod_provides([activity]).
-mod_depends([rdf, admin]).

-behaviour(zotonic_observer).
-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_mod_admin/include/admin_menu.hrl").
-include_lib("zotonic_mod_driebit_rdf/include/driebit_rdf.hrl").

-export([
    manage_schema/2,
    manage_data/2,

    observe_admin_menu/3,
    observe_content_types_dispatch/3,
    observe_serialization_content_type/2,
    observe_edge_insert/2,

    observe_triple_to_rdf/2,
    observe_expand_namespace/2,
    observe_serialize_rdf/2
]).

manage_schema(Version, Context) ->
    driebit_activity_schema:manage_schema(Version, Context).
manage_data(Version, Context) ->
    driebit_activity_schema:manage_data(Version, Context).

observe_admin_menu(#admin_menu{}, Acc, Context) ->
    case z_acl:user(Context) of
        undefined -> Acc;
        UserId ->
            [ #menu_item{
                id = admin_activitypub,
                parent = admin_modules,
                label = "ActivityPub",
                url = {admin_activitypub, [{id, UserId}]},
                visiblecheck = {acl, use, mod_admin}
            } | Acc ]
    end.

-spec observe_content_types_dispatch(#content_types_dispatch{}, list(), #context{}) -> list().
observe_content_types_dispatch(#content_types_dispatch{}, Acc, _Context) ->
    [
        {{<<"application">>, <<"activity+json">>, []}, rdf_activitystreams},
        {{<<"application">>, <<"ld+json">>, [{<<"profile">>, <<"https://www.w3.org/ns/activitystreams">>}]}, rdf_activitypub}
    | Acc].

-spec observe_serialization_content_type(#serialization_content_type{}, #context{}) ->
    {binary(), binary(), [{binary(), binary()}]} | undefined.
observe_serialization_content_type(#serialization_content_type{serialization = activitystreams}, _Context) ->
    {<<"application">>, <<"activity+json">>, []};
observe_serialization_content_type(#serialization_content_type{serialization = activitypub}, _Context) ->
    {<<"application">>, <<"ld+json">>, [{<<"profile">>, <<"\"https://www.w3.org/ns/activitystreams\"">>}]};
observe_serialization_content_type(#serialization_content_type{}, _Context) ->
    undefined.

% TODO: add observers to (optionally?) cc followers
-spec observe_edge_insert(#edge_insert{}, z:context()) -> any().
observe_edge_insert(#edge_insert{predicate=has_activity_object, subject_id=Subject, object_id=Object}, Context) ->
    case {m_rsc:is_a(Subject, activity_undo, Context), m_rsc:is_a(Object, activity, Context)} of
        {true, true} ->
            % when an 'undo' activity is inserted with an existing activity as
            % its object, then we (try to) automatically unpublish said object:
            m_rsc:update(Object, #{<<"is_published">> => false}, Context);
        _ ->
            ok
    end;
observe_edge_insert(#edge_insert{predicate=HasAudience, subject_id=ActivityId, object_id=UserId}, Context)
    when HasAudience =:= has_activity_audience_to
    orelse HasAudience =:= has_activity_audience_bto
    orelse HasAudience =:= has_activity_audience_cc
    orelse HasAudience =:= has_activity_audience_bcc
->
    % notify users by pushing to their 'activities' topic:
    z:info("Activity inbox fan out of ~p to user ~p", [ActivityId, UserId], #{}, Context),
    Topic = <<"user/", (z_convert:to_binary(UserId))/binary, "/activities">>,
    z_mqtt:publish(Topic, ActivityId, z_acl:sudo(Context));
observe_edge_insert(_, _) ->
    ok.

-spec observe_triple_to_rdf(#triple_to_rdf{}, #context{}) ->
    {ok, #rdf_triple{}} | {ok, list(#rdf_triple{})} | {error, term()} | undefined.
observe_triple_to_rdf(#triple_to_rdf{ontology = activitystreams} = TripleToRdf, Context) ->
    rdf_activitystreams:triple_to_rdf(TripleToRdf, Context);
observe_triple_to_rdf(#triple_to_rdf{ontology = activitypub} = TripleToRdf, Context) ->
    rdf_activitypub:triple_to_rdf(TripleToRdf, Context);
observe_triple_to_rdf(_TripleToRdf, _Context) ->
    undefined.

-spec observe_expand_namespace(#expand_namespace{}, #context{}) ->
    {binary() | undefined, iri()} | undefined.
observe_expand_namespace(#expand_namespace{name = activitystreams}, _Context) ->
    {undefined, rdf_activitystreams:namespace_iri()};
observe_expand_namespace(#expand_namespace{}, _Context) ->
    undefined.

-spec observe_serialize_rdf(#serialize_rdf{}, #context{}) ->
    {ok, binary()} | {error, term()} | undefined.
observe_serialize_rdf(#serialize_rdf{rdf_graph = RdfGraph, serialization = activitystreams, namespace_map = NSMap}, Context) ->
    rdf_json_ld:serialize(RdfGraph, NSMap, Context);
observe_serialize_rdf(#serialize_rdf{rdf_graph = RdfGraph, serialization = activitypub, namespace_map = NSMap}, Context) ->
    rdf_json_ld:serialize(RdfGraph, NSMap, Context);
observe_serialize_rdf(#serialize_rdf{}, _Context) ->
    undefined.

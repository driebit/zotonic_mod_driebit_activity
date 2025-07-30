%% @author Driebit
%% @copyright 2025 Driebit
%% @end

-module(m_activity).
-author("Driebit <tech@driebit.nl>").

-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_mod_driebit_rdf/include/driebit_rdf.hrl").

-behaviour(zotonic_model).

-export([
    event/2,
    m_get/3,

    inbox_activities/2,
    outbox_activities/2,
    following_objects/2,
    followers_objects/2,
    liked_objects/2,

    clear_inbox/1,
    clear_inbox/2,
    register/3
]).

% EVENTS

event(#postback{ message={oauth2_token_new, []} }, Context) ->
    case ensure_oauth_app(Context) of
        {ok, AppId} ->
            TokenProps = #{
                <<"is_read_only">> => false,
                <<"is_full_access">> => true
            },
            UserId = z_acl:user(Context),
            {ok, TokenId} = m_oauth2:insert_token(AppId, UserId, <<"">>, TokenProps, z_acl:sudo(Context)),
            {ok, Token} = m_oauth2:encode_bearer_token(TokenId, undefined, z_acl:sudo(Context)),
            z_render:dialog(
                ?__("New access token", Context),
                "_dialog_oauth2_app_token_view.tpl",
                [
                    {app_id, AppId},
                    {token, Token},
                    {backdrop, static},
                    {action, {reload, []}}
                ],
                Context
            );
        {error, _} ->
            z_render:growl_error(?__("Could not generate the access token.", Context), Context)
    end;
event(#postback{ message={oauth2_token_delete, []} }, Context) ->
    case oauth_token(Context) of
        {ok, TokenId} ->
            case m_oauth2:delete_token(TokenId, z_acl:sudo(Context)) of
                ok ->
                    z_render:wire({reload, []}, Context);
                {error, _} ->
                    z_render:growl_error(?__("Could not delete the token.", Context), Context)
            end;
        Error ->
            Error
    end.


% ZOTONIC MODEL

-spec m_get( list(), zotonic_model:opt_msg(), z:context() ) -> zotonic_model:return().
m_get([ <<"oauth_app">> | Rest ], _Msg, Context) ->
    case oauth_app(Context) of
        {ok, App} -> {ok, {App, Rest}};
        Error -> Error
    end;
m_get([ <<"oauth_token">> | Rest ], _Msg, Context) ->
    case oauth_token(Context) of
        {ok, TokenId} -> {ok, {TokenId, Rest}};
        Error -> Error
    end;
m_get([ RscId, <<"inbox">> | Rest ], _Msg, Context) when is_integer(RscId) ->
    {ok, {inbox_activities(RscId, Context), Rest}};
m_get([ RscId, <<"outbox">> | Rest ], _Msg, Context) when is_integer(RscId) ->
    {ok, {outbox_activities(RscId, Context), Rest}};
m_get([ RscId, <<"following">> | Rest ], _Msg, Context) when is_integer(RscId) ->
    {ok, {following_objects(RscId, Context), Rest}};
m_get([ RscId, <<"followers">> | Rest ], _Msg, Context) when is_integer(RscId) ->
    {ok, {followers_objects(RscId, Context), Rest}};
m_get([ RscId, <<"liked">> | Rest ], _Msg, Context) when is_integer(RscId) ->
    {ok, {liked_objects(RscId, Context), Rest}};
m_get([ RscId, Collection | Rest ], Msg, Context) when is_binary(RscId) ->
    m_get([ z_convert:to_integer(RscId), Collection | Rest ], Msg, Context);
m_get(_Path, _Msg, _Context) ->
    {error, unknown_path}.

%% @doc Return all visible activities that have 'RscId' as their 'audience'
inbox_activities(RscId, Context) ->
    visible_activities(
        inbox_activities,
        "SELECT subject_id FROM edge
        WHERE object_id = $1 AND predicate_id IN ($2, $3, $4, $5)
        ORDER BY created DESC",
        [
            RscId,
            m_rsc:rid(has_activity_audience_to, Context),
            m_rsc:rid(has_activity_audience_bto, Context),
            m_rsc:rid(has_activity_audience_cc, Context),
            m_rsc:rid(has_activity_audience_bcc, Context)
        ],
        RscId,
        Context
    ).

%% @doc Return all visible activities that have 'RscId' as their 'actor'
outbox_activities(RscId, Context) ->
    visible_activities(
        outbox_activities,
        "SELECT subject_id FROM edge
        WHERE object_id = $1 AND predicate_id = $2
        ORDER BY created DESC",
        [
            RscId,
            m_rsc:rid(has_activity_actor, Context)
        ],
        RscId,
        Context
    ).

%% @doc Return all the objects of visible 'follow' activities that have 'RscId' as 'actor'
following_objects(RscId, Context) ->
    visible_activities_objects(
        following_objects,
        "SELECT activity.id, follow_edge.object_id
        FROM rsc activity, edge actor_edge, edge follow_edge
        WHERE actor_edge.object_id = $1
        AND actor_edge.predicate_id = $2
        AND actor_edge.subject_id = follow_edge.subject_id
        AND actor_edge.subject_id = activity.id
        AND follow_edge.predicate_id = $3
        AND activity.category_id = $4
        AND activity.is_published = true
        ORDER BY activity.modified DESC",
        [
            RscId,
            m_rsc:rid(has_activity_actor, Context),
            m_rsc:rid(has_activity_object, Context),
            m_rsc:rid(activity_follow, Context)
        ],
        RscId,
        Context
    ).

%% @doc Return all the 'actor's of visible 'follow' activities that have 'RscId' as object
followers_objects(RscId, Context) ->
    visible_activities_objects(
        followers_objects,
        "SELECT activity.id, actor_edge.object_id
        FROM rsc activity, edge actor_edge, edge follow_edge
        WHERE follow_edge.object_id = $1
        AND follow_edge.predicate_id = $2
        AND follow_edge.subject_id = actor_edge.subject_id
        AND follow_edge.subject_id = activity.id
        AND actor_edge.predicate_id = $3
        AND activity.category_id = $4
        AND activity.is_published = true
        ORDER BY activity.modified DESC",
        [
            RscId,
            m_rsc:rid(has_activity_object, Context),
            m_rsc:rid(has_activity_actor, Context),
            m_rsc:rid(activity_follow, Context)
        ],
        RscId,
        Context
    ).

%% @doc Return all the objects of visible 'like' activities that have 'RscId' as 'actor'
liked_objects(RscId, Context) ->
    visible_activities_objects(
        liked_objects,
        "SELECT activity.id, liked_edge.object_id
        FROM rsc activity, edge actor_edge, edge liked_edge
        WHERE actor_edge.object_id = $1
        AND actor_edge.predicate_id = $2
        AND actor_edge.subject_id = liked_edge.subject_id
        AND actor_edge.subject_id = activity.id
        AND liked_edge.predicate_id = $3
        AND activity.category_id = $4
        AND activity.is_published = true
        ORDER BY activity.modified DESC",
        [
            RscId,
            m_rsc:rid(has_activity_actor, Context),
            m_rsc:rid(has_activity_object, Context),
            m_rsc:rid(activity_like, Context)
        ],
        RscId,
        Context
    ).

% DB/ACL HELPERS

oauth_app(Context) ->
    case ensure_oauth_app(Context) of
        {ok, AppId} ->
            case m_oauth2:get_app(AppId, z_acl:sudo(Context)) of
                {ok, App} -> {ok, App};
                Error -> Error
            end;
        Error -> Error
    end.

oauth_token(Context) ->
    case ensure_oauth_app(Context) of
        {ok, AppId} ->
            {ok, z_db:q1("
                SELECT id
                FROM oauth2_token
                WHERE app_id = $1 AND user_id = $2",
                [ AppId, z_acl:user(Context) ],
                Context
            )};
        Error ->
            Error
    end.


-spec ensure_oauth_app(z:context()) -> {ok, AppId :: integer()} | {error, term()}.
ensure_oauth_app(Context) ->
    SudoContext = z_acl:sudo(Context),
    % Try to find a registered AppID for 'ActivityPub'
    case m_oauth2:list_apps(SudoContext) of
        {ok, AppList} ->
            SearchRes = lists:search(
                fun (AppMap) ->
                    case maps:get(<<"description">>, AppMap, undefined) of
                        <<"ActivityPub">> -> true;
                        _ -> false
                    end
                end,
                AppList
            ),
            case SearchRes of
                {value, #{<<"id">> := AppId}} ->
                    {ok, AppId};
                _ ->
                    % If there wasn't one, add it now:
                    AppInsert = #{
                        <<"is_enabled">> => true,
                        <<"user_id">> => z_acl:user(SudoContext)
                    },
                    case m_oauth2:insert_app(AppInsert, SudoContext) of
                        {ok, NewAppId} ->
                            AppUpdate = #{
                                <<"description">> => <<"ActivityPub">>,
                                <<"is_allow_auth">> => false,
                                <<"is_allow_client_credentials">> => true,
                                <<"client_credentials_expires">> => 0,
                                <<"client_credentials_user_id">> => z_acl:user(SudoContext),
                                <<"is_client_credentials_read_only">> => false
                            },
                            case m_oauth2:update_app(NewAppId, AppUpdate, SudoContext) of
                                ok ->
                                    {ok, NewAppId};
                                Error ->
                                    Error
                            end;
                        Error ->
                            Error
                    end
            end;
        Error ->
            Error
    end.

visible_activities(QueryKey, Query, Arguments, DepId, Context) ->
    lists:uniq(lists:filtermap(
        fun ({RscId}) ->
            case m_rsc:is_a(RscId, activity, Context) andalso z_acl:rsc_visible(RscId, Context) of
                true -> {true, RscId};
                false -> false
            end
        end,
        z_depcache:memo(
            fun() -> z_db:q(Query, Arguments, Context) end,
            {QueryKey, DepId},
            3600,
            [DepId],
            Context
        )
    )).

visible_activities_objects(QueryKey, Query, Arguments, DepId, Context) ->
    lists:uniq(lists:filtermap(
        fun ({ActivityId, ObjectId}) ->
            case m_rsc:is_a(ActivityId, activity, Context) andalso z_acl:rsc_visible(ActivityId, Context) of
                true -> {true, ObjectId};
                false -> false
            end
        end,
        z_depcache:memo(
            fun() -> z_db:q(Query, Arguments, Context) end,
            {QueryKey, DepId},
            3600,
            [DepId],
            Context
        )
    )).

% RSC HELPERS


%% @doc Remove all activities from the current user's inbox
-spec clear_inbox(z:context()) -> ok.
clear_inbox(Context) ->
    lists:foreach(
        fun (ActivityId) -> clear_inbox(ActivityId, Context) end,
        inbox_activities(z_acl:user(Context), Context)
    ).

%% @doc Remove the given activity from the current user's inbox
-spec clear_inbox(m_rsc:resource_id(), z:context()) -> ok.
clear_inbox(ActivityId, Context) ->
    lists:foreach(
        fun (AudiencePredicate) ->
            m_edge:delete(
                ActivityId,
                AudiencePredicate,
                z_acl:user(Context),
                Context
            )
        end,
        [has_activity_audience_to, has_activity_audience_bto, has_activity_audience_cc, has_activity_audience_bcc]
    ).

-spec register(
    binary() | atom(),
    list(),
    z:context()
) -> {ok, m_rsc:resource_id()} | {error, term()}.
register(<<"activity", _Rest/binary>> = ActivityCategory, Options, Context) ->
    Actor = proplists:get_value(actor, Options, z_acl:user(Context)),
    Title = proplists:get_value(title, Options, <<"Registered Activity">>),
    Objects = find_options(object, Options),
    Targets = find_options(target, Options),
    Results = find_options(result, Options),
    Origins = find_options(origin, Options),
    Instruments = find_options(instrument, Options),
    AudienceTo = find_options(to, Options),
    AudienceBto = find_options(bto, Options),
    AudienceCc = find_options(cc, Options),
    AudienceBcc = find_options(bcc, Options),

    % NOTE: the creation of the edges cannot be tested with the 'admin' account
    % see: 'm_rsc_update:insert_edges'
    Props = #{
        <<"title">> => Title,
        <<"category_id">> => ActivityCategory,
        <<"is_unfindable">> => true,
        <<"is_published">> => true,
        <<"o">> => #{
            <<"has_activity_actor">> => Actor,
            <<"has_activity_object">> => Objects,
            <<"has_activity_target">> => Targets,
            <<"has_activity_result">> => Results,
            <<"has_activity_origin">> => Origins,
            <<"has_activity_instrument">> => Instruments,
            <<"has_activity_audience_to">> => AudienceTo,
            <<"has_activity_audience_bto">> => AudienceBto,
            <<"has_activity_audience_cc">> => AudienceCc,
            <<"has_activity_audience_bcc">> => AudienceBcc
        }
    },

    m_rsc:insert(Props, Context);
register(ActivityCategory, Options, Context) when is_atom(ActivityCategory) ->
    register(z_convert:to_binary(ActivityCategory), Options, Context);
register(ActivityCategory, Options, Context) when is_binary(ActivityCategory) ->
    register(<<"activity_", ActivityCategory/binary>>, Options, Context).

find_options(OptionName, Options) ->
    lists:filtermap(
        fun
            ({OptionKey, OptionValue}) when OptionKey =:= OptionName ->
                {true, OptionValue};
            (_Otherwise) ->
                false
        end,
        Options
    ).

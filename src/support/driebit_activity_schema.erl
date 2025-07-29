%% @author Driebit
%% @copyright 2025 Driebit

-module(driebit_activity_schema).
-author("Driebit <tech@driebit.nl>").

-include_lib("zotonic_core/include/zotonic.hrl").

-export([
    manage_schema/2,
    manage_data/2
]).

manage_schema(install, _Context) ->
    #datamodel{
        categories = [
            {activity, undefined, [
                {title, <<"Activity">>},
                {website, <<"https://www.w3.org/ns/activitystreams#Activity">>}
            ]},
            {activity_accept, activity, [
                {title, <<"Activity: Accept">>},
                {website, <<"https://www.w3.org/ns/activitystreams#Accept">>},
                {seo_noindex, true}
            ]},
            {activity_add, activity, [
                {title, <<"Activity: Add">>},
                {website, <<"https://www.w3.org/ns/activitystreams#Add">>},
                {seo_noindex, true}
            ]},
            {activity_announce, activity, [
                {title, <<"Activity: Announce">>},
                {website, <<"https://www.w3.org/ns/activitystreams#Announce">>},
                {seo_noindex, true}
            ]},
            {activity_arrive, activity, [
                {title, <<"Activity: Arrive">>},
                {website, <<"https://www.w3.org/ns/activitystreams#Arrive">>},
                {seo_noindex, true}
            ]},
            {activity_block, activity, [
                {title, <<"Activity: Block">>},
                {website, <<"https://www.w3.org/ns/activitystreams#Block">>},
                {seo_noindex, true}
            ]},
            {activity_create, activity, [
                {title, <<"Activity: Create">>},
                {website, <<"https://www.w3.org/ns/activitystreams#Create">>},
                {seo_noindex, true}
            ]},
            {activity_delete, activity, [
                {title, <<"Activity: Delete">>},
                {website, <<"https://www.w3.org/ns/activitystreams#Delete">>},
                {seo_noindex, true}
            ]},
            {activity_dislike, activity, [
                {title, <<"Activity: Dislike">>},
                {website, <<"https://www.w3.org/ns/activitystreams#Dislike">>},
                {seo_noindex, true}
            ]},
            {activity_follow, activity, [
                {title, <<"Activity: Follow">>},
                {website, <<"https://www.w3.org/ns/activitystreams#Follow">>},
                {seo_noindex, true}
            ]},
            {activity_flag, activity, [
                {title, <<"Activity: Flag">>},
                {website, <<"https://www.w3.org/ns/activitystreams#Flag">>},
                {seo_noindex, true}
            ]},
            {activity_ignore, activity, [
                {title, <<"Activity: Ignore">>},
                {website, <<"https://www.w3.org/ns/activitystreams#Ignore">>},
                {seo_noindex, true}
            ]},
            {activity_invite, activity, [
                {title, <<"Activity: Invite">>},
                {website, <<"https://www.w3.org/ns/activitystreams#Invite">>},
                {seo_noindex, true}
            ]},
            {activity_join, activity, [
                {title, <<"Activity: Join">>},
                {website, <<"https://www.w3.org/ns/activitystreams#Join">>},
                {seo_noindex, true}
            ]},
            {activity_leave, activity, [
                {title, <<"Activity: Leave">>},
                {website, <<"https://www.w3.org/ns/activitystreams#Leave">>},
                {seo_noindex, true}
            ]},
            {activity_like, activity, [
                {title, <<"Activity: Like">>},
                {website, <<"https://www.w3.org/ns/activitystreams#Like">>},
                {seo_noindex, true}
            ]},
            {activity_listen, activity, [
                {title, <<"Activity: Listen">>},
                {website, <<"https://www.w3.org/ns/activitystreams#Listen">>},
                {seo_noindex, true}
            ]},
            {activity_move, activity, [
                {title, <<"Activity: Move">>},
                {website, <<"https://www.w3.org/ns/activitystreams#Move">>},
                {seo_noindex, true}
            ]},
            {activity_offer, activity, [
                {title, <<"Activity: Offer">>},
                {website, <<"https://www.w3.org/ns/activitystreams#Offer">>},
                {seo_noindex, true}
            ]},
            {activity_read, activity, [
                {title, <<"Activity: Read">>},
                {website, <<"https://www.w3.org/ns/activitystreams#Read">>},
                {seo_noindex, true}
            ]},
            {activity_reject, activity, [
                {title, <<"Activity: Reject">>},
                {website, <<"https://www.w3.org/ns/activitystreams#Reject">>},
                {seo_noindex, true}
            ]},
            {activity_remove, activity, [
                {title, <<"Activity: Remove">>},
                {website, <<"https://www.w3.org/ns/activitystreams#Remove">>},
                {seo_noindex, true}
            ]},
            {activity_tentativeAccept, activity, [
                {title, <<"Activity: TentativeAccept">>},
                {website, <<"https://www.w3.org/ns/activitystreams#TentativeAccept">>},
                {seo_noindex, true}
            ]},
            {activity_tentativeReject, activity, [
                {title, <<"Activity: TentativeReject">>},
                {website, <<"https://www.w3.org/ns/activitystreams#TentativeReject">>},
                {seo_noindex, true}
            ]},
            {activity_travel, activity, [
                {title, <<"Activity: Travel">>},
                {website, <<"https://www.w3.org/ns/activitystreams#Travel">>},
                {seo_noindex, true}
            ]},
            {activity_undo, activity, [
                {title, <<"Activity: Undo">>},
                {website, <<"https://www.w3.org/ns/activitystreams#Undo">>},
                {seo_noindex, true}
            ]},
            {activity_update, activity, [
                {title, <<"Activity: Update">>},
                {website, <<"https://www.w3.org/ns/activitystreams#Update">>},
                {seo_noindex, true}
            ]},
            {activity_view, activity, [
                {title, <<"Activity: View">>},
                {website, <<"https://www.w3.org/ns/activitystreams#View">>},
                {seo_noindex, true}
            ]}
        ],
        predicates = [
            {has_activity_actor,
                [
                    {title, <<"Has actor">>},
                    {uri, <<"https://www.w3.org/ns/activitystreams#actor">>},
                    {website, <<"https://www.w3.org/ns/activitystreams#actor">>}
                ],
                []
            },
            {has_activity_object,
                [
                    {title, <<"Has object">>},
                    {uri, <<"https://www.w3.org/ns/activitystreams#object">>},
                    {website, <<"https://www.w3.org/ns/activitystreams#object">>}
                ],
                []
            },
            {has_activity_target,
                [
                    {title, <<"Has target">>},
                    {uri, <<"https://www.w3.org/ns/activitystreams#target">>},
                    {website, <<"https://www.w3.org/ns/activitystreams#target">>}
                ],
                []
            },
            {has_activity_result,
                [
                    {title, <<"Has result">>},
                    {uri, <<"https://www.w3.org/ns/activitystreams#result">>},
                    {website, <<"https://www.w3.org/ns/activitystreams#result">>}
                ],
                []
            },
            {has_activity_origin,
                [
                    {title, <<"Has origin">>},
                    {uri, <<"https://www.w3.org/ns/activitystreams#origin">>},
                    {website, <<"https://www.w3.org/ns/activitystreams#origin">>}
                ],
                []
            },
            {has_activity_instrument,
                [
                    {title, <<"Has instrument">>},
                    {uri, <<"https://www.w3.org/ns/activitystreams#instrument">>},
                    {website, <<"https://www.w3.org/ns/activitystreams#instrument">>}
                ],
                []
            },
            {has_activity_audience_to,
                [
                    {title, <<"Has to">>},
                    {uri, <<"https://www.w3.org/ns/activitystreams#to">>},
                    {website, <<"https://www.w3.org/ns/activitystreams#to">>}
                ],
                []
            },
            {has_activity_audience_bto,
                [
                    {title, <<"Has bto">>},
                    {uri, <<"https://www.w3.org/ns/activitystreams#bto">>},
                    {website, <<"https://www.w3.org/ns/activitystreams#bto">>}
                ],
                []
            },
            {has_activity_audience_cc,
                [
                    {title, <<"Has cc">>},
                    {uri, <<"https://www.w3.org/ns/activitystreams#cc">>},
                    {website, <<"https://www.w3.org/ns/activitystreams#cc">>}
                ],
                []
            },
            {has_activity_audience_bcc,
                [
                    {title, <<"Has bcc">>},
                    {uri, <<"https://www.w3.org/ns/activitystreams#bcc">>},
                    {website, <<"https://www.w3.org/ns/activitystreams#bcc">>}
                ],
                []
            }
        ],
        resources = [],
        media = [],
        edges = []
    }.

manage_data(install, Context) ->
    update_acl_rules(Context),
    ok.

update_acl_rules(Context) ->
    AclRules = [
        % Members can view, insert, update and link to their own activities:
        {rsc, [
            {acl_user_group_id, acl_user_group_members},
            {is_owner, true},
            {actions, [insert, update, link, view]},
            {category_id, activity}
        ]}
    ],
    m_acl_rule:replace_managed(AclRules, z_context:site(Context), Context).

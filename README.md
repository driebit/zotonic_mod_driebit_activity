zotonic_mod_driebit_activity
============================

A Zotonic module to track user activity on resources, specifically with support
for the [ActivityPub protocol](https://www.w3.org/TR/activitypub/).

This module is based upon and heavily relies on `zotonic_mod_driebit_rdf`,
please check out its documentation as well.

### Table of contents

1. [Features](#features)
2. [Notifications](#notifications)

Features
--------

### Represent resources with ActivityPub/ActivityStreams

This modules adds the ontologies, serializations, namespaces and content types
necessary to represent resources within the [ActivityStreams](https://www.w3.org/ns/activitystreams)
and/or [ActivityPub](https://www.w3.org/TR/activitypub/) specification.

These can be requested with content type negotiations, respectively:
```bash
curl -L -H Accept:application/activity+json https://yoursite.com/id/<id>
curl -L -H "Accept:application/ld+json; profile=\"https://www.w3.org/ns/activitystreams\"" https://yoursite.com/id/<id>
```

or with one of the generic RDF endpoints (see `zotonic_mod_driebit_rdf`), as
well as with some specific ones added for convenience:
```bash
curl -L http://yoursite.com/rdf/activitystreams/<id>
curl -L http://yoursite.com/rdf/activitypub/<id>
```

### ActivityPub support

Aside from representing standard resources and connections with them, this module
also supports [ActivityPub's special collections](https://www.w3.org/TR/activitypub/#collections)
which are automatically calculated and added to [Actor objects](https://www.w3.org/TR/activitypub/#actor-objects).

These can also be accessed with their individual entrypoints:
```bash
curl -L -H Accept:application/activity+json https://yoursite.com/activitypub/<id>/<collection>
```
where:
- `<collection>` can be either of `inbox`, `outbox`, `following`, `followers` and `liked`
- `<id>` is the user ID and can be omitted to see the current user's

At the moment this module only supports pulling data, and pushing, either by
`POST` or `DELETE` http requests, is left for future implementation.

### Admin page and authentication

This module also adds an admin page, which can be found as "ActivityPub" under
the "Modules" menu, where users with access to the admin can:
1. explore the special collections of users
2. request an OAuth2 token to authenticate their requests (instead of using cookies)

### Activity tracking and automations

The module doesn't track any user activity automatically by default, individual
sites must decide what to track and as a consequence of what.

Most commonly one will probably have to listen to [resource](https://zotonic.com/docs/2049/resource-notifications)
or [edge notifications](https://zotonic.com/docs/2088/edge-notifications) and
then insert an activity resource as a consequence.

Separate categories for all of the ActivityPub's supported activity types are
installed with this module, but for simplicity, `m_activity` has a `register`
function to add one.
For example one might want to start tracking the creation of resources with:
```erlang
    m_activity:register(
        create,
        [{object, NewRscId}],
        z_acl:logon(UserId, Context)
    ).
```
which will add a new resource of the `activity_create` category, assign the
`UserId` as its `actor` and the `NewRscId` as its object (see erlang code for
details and available options).

The module on its own doesn't automate much, as the tracking and handling logic
is mostly context-dependent on the site, with some exceptions:
1. [ActivityPub's special collections](https://www.w3.org/TR/activitypub/#collections)
  are automatically updated when new activities are registered/published
2. Inserting an `undo` activity will unpublish its `object` activity, which
  excludes it from the "object" collections above

### ACL and permissions

By default every user can insert new resources of the `activity` category (and
subcategories), but only see their own.

These permissions can of course be overridden [as usual](https://zotonic.com/docs/1536/access-control).

Note: it might not be feasible or possible to use zotonic's cookies for
authentication, see the admin menu to obtain an OAuth2 token instead.

Notifications
-------------

This module doesn't add any [Zotonic Notification](https://zotonic.com/docs/1274/notifications),
but instead merely uses the ones from `zotonic_mod_driebit_rdf` and `zotonic_core`.

There is however an MQTT notification which is sent to each user whenever there
is something new in their `inbox`, this uses the topic: `user/<user_id>/activities`.

This can also be used in templates to have push notifications with a `live` tag,
for example:
```
{% if m.acl.user as user %}
    {% live
        template="notifications.tpl"
        topic=["bridge", "origin", "user", user, "activities"]
    %}
{% endif %}
```

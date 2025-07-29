{% extends "admin_base.tpl" %}

{% block title %}{_ ActivityPub Overview _}{% endblock %}

{% block content %}
    {% with m.activity.oauth_app as app %}
        <div class="admin-header">
            <h2>{_ OAuth2 Access Token _}</h2>
            <a href="{% url admin_oauth2_apps_tokens appid=app.id %}">
                {_ OAuth2 Applications _} &gt; {{ app.description|escape }}
            </a>
        </div>
        <div class="well">
            <p>Activities, as any other resource, are subject to ACL.</p>
            <p>
                To be able to access activities as your own user you can provide
                an access token in the request headers (<code>Authorization:
                Bearer oauth2-XXXX</code>).
            </p>
            {% if m.activity.oauth_token as token_id %}
                <p>
                    You have already registered an access token for ActivityPub.
                </p>
                <button id="{{ #delete_oauth_token }}" class="btn btn-danger">
                    {_ Delete existing Access Token _}
                </button>
                {% wire
                    id=#delete_oauth_token
                    action={confirm
                        text=_"Are you sure you want to delete this access token?"
                        ok=_"Delete"
                        is_danger
                        postback={oauth2_token_delete}
                        delegate=`m_activity`
                    }
                %}
            {% else %}
                <p>
                    If you don't have one already, you can request an OAuth2 Access Token here.
                </p>
                <div class="z-button-row">
                    {% button
                        text=_"Make a new access token"
                        class="btn btn-primary"
                        postback={oauth2_token_new}
                        delegate=`m_activity`
                    %}
                </div>

            {% endif %}
        </div>
    {% endwith %}

    <div class="admin-header">
        <h2>{_ ActivityPub Actor Collections _}</h2>
        <a href="https://www.w3.org/TR/activitypub/#actor-objects">{_ Read more _}</a>
    </div>
    <div class="well "z-button-row">
        <a class="btn btn-primary" href="{% url admin_activitypub_collection id=q.id collection='inbox' %}">{_ Inbox _}</a>
        <a class="btn btn-primary" href="{% url admin_activitypub_collection id=q.id collection='outbox' %}">{_ Outbox _}</a>
        <a class="btn btn-default" href="{% url admin_activitypub_collection id=q.id collection='following' %}">{_ Following _}</a>
        <a class="btn btn-default" href="{% url admin_activitypub_collection id=q.id collection='followers' %}">{_ Followers _}</a>
        <a class="btn btn-default" href="{% url admin_activitypub_collection id=q.id collection='liked' %}">{_ Liked _}</a>
    </div>

    <div class="admin-header">
        <h2>{_ Most recent activities from _} {{ m.rsc[q.id].title }}</h2>
        <a class="btn btn-default btn-xs" href="{% url admin_overview_rsc qcreator_id=q.id qcat='activity' qsort=-created %}">{_ See all activity _}</a>
    </div>
    {% with m.search.paged[{query cat='activity' creator_id=q.id zsort="-created" page=1 pagelen=5}] as result %}
        <table class="table table-striped do_adminLinkedTable">
            <thead>
                <tr>
                    <th width="60%">{_ Type _}</th>
                    <th width="20%">{_ Created on _}</th>
                    <th width="20%">{_ Modified on _}</th>
                </tr>
            </thead>

            <tbody>
            {% for id in result|is_visible %}
                <tr class="{% if not id.is_published %}unpublished{% endif %}" data-href="{% url admin_edit_rsc id=id %}">
                    <td>{% include "_admin_overview_list_data.tpl" %}</td>
                    <td class="hidden-xs">{{ id.created|date:_"d M Y, H:i" }}</td>
                    <td>{{ id.modified|date:_"d M Y, H:i" }}</td>
                </tr>
            {% empty %}
                <tr>
                    <td colspan="5">
                        {_ No activity found. _}
                    </td>
                </tr>
            {% endfor %}
            </tbody>
        </table>
    {% endwith %}
{% endblock %}

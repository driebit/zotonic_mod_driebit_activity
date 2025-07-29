{% extends "admin_base.tpl" %}

{% block title %}{_ ActivityPub Overview _}{% endblock %}

{% block content %}
    <div class="admin-header">
        <h2>ActivityPub <em>"{{ q.collection }}"</em> {_ collection for _} {{ m.rsc[q.id].title }}</h2>
        <a class="btn btn-default btn-xs" href="{% url admin_activitypub id=q.id %}">{_ See ActivityPub overview _}</a>
    </div>
    {% with m.activity[q.id][q.collection] as result %}
        <table class="table table-striped do_adminLinkedTable">
            <thead>
                <tr>
                    <th width="20%">{_ Title _}</th>
                    <th width="20%">{_ Category _}</th>
                    <th width="20%">{_ Published on _}</th>
                    <th width="20%">{_ Created on _}</th>
                    <th width="20%">{_ Modified on _}</th>
                </tr>
            </thead>

            <tbody>
            {% for id in result|is_visible %}
                <tr class="{% if not id.is_published %}unpublished{% endif %}" data-href="{% url admin_edit_rsc id=id %}">
                    <td>
                        <span {% include "_language_attrs.tpl" %}>{{ id.title|striptags|default:_"<em>Untitled</em>" }}</span>
                    </td>
                    <td>{% include "_admin_overview_list_data.tpl" %}</td>
                    <td class="hidden-xs">{{ id.publication_start|date:_"d M Y, H:i" }}</td>
                    <td class="hidden-xs">{{ id.created|date:_"d M Y, H:i" }}</td>
                    <td>{{ id.modified|date:_"d M Y, H:i" }}</td>
                </tr>
            {% empty %}
                <tr>
                    <td colspan="5">
                        {_ None _}
                    </td>
                </tr>
            {% endfor %}
            </tbody>
        </table>
    {% endwith %}
{% endblock %}

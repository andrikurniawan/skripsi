{% extends "admin_edit_widget_std.tpl" %}

{% block widget_title %}{{ _"Data Properties CharityOrganization"|escapejs }}{% endblock %}{% block widget_show_minimized %}false{% endblock %}{% block widget_id %}content-person{% endblock %}{% block widget_content %}<fieldset>{% with m.rsc[id] as r %}<div class="row">
		</div>
    {% endwith %}
</fieldset>
{% endblock %}
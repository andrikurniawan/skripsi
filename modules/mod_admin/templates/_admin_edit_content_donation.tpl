{% extends "admin_edit_widget_std.tpl" %}

{% block widget_title %}{{ _"Data Properties Donation"|escapejs }}{% endblock %}{% block widget_show_minimized %}false{% endblock %}{% block widget_id %}content-person{% endblock %}{% block widget_content %}<fieldset>{% with m.rsc[id] as r %}<div class="row">
<div class="form-group col-lg-4 col-md-4"><label class="control-label" for="amount">{_ amount _}</label><div><input class="form-control" id="amount" type="text" name="amount" value="{{ r.amount }}" /></div></div>
		</div>
    {% endwith %}
</fieldset>
{% endblock %}
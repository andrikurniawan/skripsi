{% extends "admin_edit_widget_std.tpl" %}

{% block widget_title %}{{ _"Data Properties Program"|escapejs }}{% endblock %}{% block widget_show_minimized %}false{% endblock %}{% block widget_id %}content-person{% endblock %}{% block widget_content %}<fieldset>{% with m.rsc[id] as r %}<div class="row">
<div class="form-group col-lg-4 col-md-4"><label class="control-label" for="programtotal">{_ total _}</label><div><input class="form-control" id="programtotal" type="text" name="programtotal" value="{{ r.programtotal }}"  disabled/></div></div>{% javascript %}document.getElementById("programtotal").value = {{m.abs.totalDonation[{query id=id}]}};{% endjavascript %}
		</div>
    {% endwith %}
</fieldset>
{% endblock %}
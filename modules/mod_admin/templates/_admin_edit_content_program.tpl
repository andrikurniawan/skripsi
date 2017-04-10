{% extends "admin_edit_widget_std.tpl" %}

{% block widget_title %}{{ _"Data Properties Program"|escapejs }}{% endblock %}{% block widget_show_minimized %}false{% endblock %}{% block widget_id %}content-person{% endblock %}{% block widget_content %}<fieldset>{% with m.rsc[id] as r %}<div class="row">
<div class="form-group col-lg-4 col-md-4"><label class="control-label" for="programtotal">{_ total _}</label><div><input class="form-control" id="programtotal" type="text" name="programtotal" value="{{ r.programtotal }}"  disabled/></div></div>{% javascript %}var predId = [];var total = 0;{% endjavascript %}{% with m.search.paged[{referrers id=id page=page}] as result %}{% for id, pred_id in result %}{% javascript %}if (predId.indexOf("{{ pred_id }}") == -1) {predId.push("{{ pred_id }}");{% endjavascript %}{% for amountholder in r.s[m.rsc[pred_id].title | lower] %}{% if amountholder.amount %}{% javascript %}total += {{ amountholder.amount }};{% endjavascript %}{% endif %}{% endfor %}{% javascript %}}{% endjavascript %}{% endfor %}{% endwith %}{% javascript %}document.getElementById("programtotal").value = total;{% endjavascript %}
		</div>
    {% endwith %}
</fieldset>
{% endblock %}
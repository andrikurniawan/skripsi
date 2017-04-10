#!/bin/bash

SITE=`cat recentsite.txt`;

classlink=($(grep -oP 'owl:Class rdf:about="\K[^"]*' charity_org_rdf.owl));
domainDataPropLink=($(xml_grep 'owl:DatatypeProperty' charity_org_rdf.owl | grep -oP 'rdfs:domain rdf:resource="\K[^"]*'));
rangeDataProp=($(xml_grep 'owl:DatatypeProperty' charity_org_rdf.owl | grep -oP 'rdfs:range rdf:resource="\K[^"]*' | grep -oP '[a-z,A-Z,0-9,:,/,.,-]*#\K[^"]*'));
dataProp=($(xml_grep 'owl:DatatypeProperty' charity_org_rdf.owl | grep -oP 'rdf:about="[a-z,A-Z,0-9,:,/,.,-]*#\K[^"]*'));
class=($(xml_grep 'owl:Class' charity_org_rdf.owl | grep -oP 'rdf:about="[a-z,A-Z,0-9,:,/,.,-]*#\K[^"]*'));

echo '' > user/sites/$SITE/templates/page.tpl; 
headuser='{% extends "base.tpl" %}{% block title %}{{ m.rsc[id].title }}{% endblock %}{% block chapeau %}{% include "_article_chapeau.tpl" %}{% endblock %}{% block content %}<h1>{{ m.rsc[id].title }}</h1>{% if m.rsc[id].summary %}<p class="summary">{{ m.rsc[id].summary }}</p>{% endif %}'
tailuser='{% block below_summary %}{% if m.rsc[id].depiction %}<figure class="image-wrapper block-level-image">{% media m.rsc[id].depiction width=445 crop class=align alt=m.rsc[id].title %}</figure>{% endif %}{% endblock %}{{ m.rsc[id].body|show_media }}{% include "_blocks.tpl" %}{% block below_body %}{% endblock %}{% with m.search.paged[{query hassubject=[id, "haspart"]  sort="seq" pagelen=1 page=q.page}] as result %}{% for id in result %}{{ m.rsc[id].title }}{% endfor %}{% pager result=result dispatch="page" id=id hide_single_page %}{% endwith %}{% endblock %}'
mainuser=''
for i in "${!classlink[@]}"; do 
    echo '{{ _"Data Properties '${class[$i]}'"|escapejs }}{% endblock %}{% block widget_show_minimized %}false{% endblock %}{% block widget_id %}content-person{% endblock %}{% block widget_content %}<fieldset>{% with m.rsc[id] as r %}<div class="row">' > modules/mod_admin/templates/_admin_edit_content_main; 
    for j in "${!domainDataPropLink[@]}"; do 
        if [[ ${domainDataPropLink[$j]} = ${classlink[$i]} ]]; then 
            type='text'; 
            if [[ ${rangeDataProp[$j]} = 'integer' ]]; then 
                type='number'; 
            fi; 
            if [[ ${dataProp[$j]} = 'total' ]]; then 
                echo '<div class="form-group col-lg-4 col-md-4"><label class="control-label" for="'${class[$i],,}''${dataProp[$j],,}'">{_ '${dataProp[$j]}' _}</label><div><input class="form-control" id="'${class[$i],,}''${dataProp[$j],,}'" type="'$type'" name="'${class[$i],,}''${dataProp[$j],,}'" value="{{ r.'${class[$i],,}''${dataProp[$j],,}' }}"  disabled/></div></div>{% javascript %}var predId = [];var total = 0;{% endjavascript %}{% with m.search.paged[{referrers id=id page=page}] as result %}{% for id, pred_id in result %}{% javascript %}if (predId.indexOf("{{ pred_id }}") == -1) {predId.push("{{ pred_id }}");{% endjavascript %}{% for amountholder in r.s[m.rsc[pred_id].title | lower] %}{% if amountholder.amount %}{% javascript %}total += {{ amountholder.amount }};{% endjavascript %}{% endif %}{% endfor %}{% javascript %}}{% endjavascript %}{% endfor %}{% endwith %}{% javascript %}document.getElementById("'${class[$i],,}''${dataProp[$j],,}'").value = total;{% endjavascript %}' >> modules/mod_admin/templates/_admin_edit_content_main; 
                mainuser=$mainuser'<p class="'${class[$i],,}''${dataProp[$j],,}'" id="'${class[$i],,}''${dataProp[$j],,}'"><b>{_ '${dataProp[$j]}' _}</b> : </p>{% with m.rsc[id] as r %}{% javascript %}var predId = [];var total = 0;{% endjavascript %}{% with m.search.paged[{referrers id=id page=page}] as result %}{% for id, pred_id in result %}{% javascript %}if (predId.indexOf("{{ pred_id }}") == -1) {predId.push("{{ pred_id }}");{% endjavascript %}{% for amountholder in r.s[m.rsc[pred_id].title | lower] %}{% if amountholder.amount %}{% javascript %}total += {{ amountholder.amount }};{% endjavascript %}{% endif %}{% endfor %}{% javascript %}}{% endjavascript %}{% endfor %}{% endwith %}{% javascript %}if (total == 0) { var d = document.getElementById("'${class[$i],,}''${dataProp[$j],,}'"); d.className += " hidden"; } else { document.getElementById("'${class[$i],,}''${dataProp[$j],,}'").innerHTML += total;}{% endjavascript %}{% endwith %}';
            else 
                if [[ ${dataProp[$j]} = 'max' ]]; then 
                    echo '<div class="form-group col-lg-4 col-md-4"><label class="control-label" for="'${class[$i],,}''${dataProp[$j],,}'">{_ '${dataProp[$j]}' _}</label><div><input class="form-control" id="'${class[$i],,}''${dataProp[$j],,}'" type="'$type'" name="'${class[$i],,}''${dataProp[$j],,}'" value="{{ r.'${class[$i],,}''${dataProp[$j],,}' }}"  disabled/></div></div>{% javascript %}var predId = [];var max = 0;{% endjavascript %}{% with m.search.paged[{referrers id=id page=page}] as result %}{% for id, pred_id in result %}{% javascript %}if (predId.indexOf("{{ pred_id }}") == -1) {predId.push("{{ pred_id }}");{% endjavascript %}{% for amountholder in r.s[m.rsc[pred_id].title | lower] %}{% if amountholder.amount %}{% javascript %}if ({{ amountholder.amount }} > max) max = {{ amountholder.amount }};{% endjavascript %}{% endif %}{% endfor %}{% javascript %}}{% endjavascript %}{% endfor %}{% endwith %}{% javascript %}document.getElementById("'${class[$i],,}''${dataProp[$j],,}'").value = max;{% endjavascript %}' >> modules/mod_admin/templates/_admin_edit_content_main; 
                    mainuser=$mainuser'<p class="'${class[$i],,}''${dataProp[$j],,}'" id="'${class[$i],,}''${dataProp[$j],,}'"><b>{_ '${dataProp[$j]}' _}</b> : </p>{% with m.rsc[id] as r %}{% javascript %}var predId = [];var max = 0;{% endjavascript %}{% with m.search.paged[{referrers id=id page=page}] as result %}{% for id, pred_id in result %}{% javascript %}if (predId.indexOf("{{ pred_id }}") == -1) {predId.push("{{ pred_id }}");{% endjavascript %}{% for amountholder in r.s[m.rsc[pred_id].title | lower] %}{% if amountholder.amount %}{% javascript %}if ({{ amountholder.amount }} > max) max = {{ amountholder.amount }};{% endjavascript %}{% endif %}{% endfor %}{% javascript %}}{% endjavascript %}{% endfor %}{% endwith %}{% javascript %}if (max == 0) { var d = document.getElementById("'${class[$i],,}''${dataProp[$j],,}'"); d.className += " hidden"; } else { document.getElementById("'${class[$i],,}''${dataProp[$j],,}'").innerHTML += max;}{% endjavascript %}{% endwith %}';
                else 
                    if [[ ${dataProp[$j]} = 'amount' ]]; then 
                        echo '<div class="form-group col-lg-4 col-md-4"><label class="control-label" for="'${dataProp[$j],,}'">{_ '${dataProp[$j]}' _}</label><div><input class="form-control" id="'${dataProp[$j],,}'" type="'$type'" name="'${dataProp[$j],,}'" value="{{ r.'${dataProp[$j],,}' }}" /></div></div>' >> modules/mod_admin/templates/_admin_edit_content_main; 
                        mainuser=$mainuser'{% if m.rsc[id].'${dataProp[$j],,}' %}<p class="'${dataProp[$j],,}'"><b>{_ '${dataProp[$j]}' _}</b> : {{ m.rsc[id].'${dataProp[$j],,}' }}</p>{% endif %}';
                    else
                        echo '<div class="form-group col-lg-4 col-md-4"><label class="control-label" for="'${class[$i],,}''${dataProp[$j],,}'">{_ '${dataProp[$j]}' _}</label><div><input class="form-control" id="'${class[$i],,}''${dataProp[$j],,}'" type="'$type'" name="'${class[$i],,}''${dataProp[$j],,}'" value="{{ r.'${class[$i],,}''${dataProp[$j],,}' }}" /></div></div>' >> modules/mod_admin/templates/_admin_edit_content_main; 
                        mainuser=$mainuser'{% if m.rsc[id].'${class[$i],,}''${dataProp[$j],,}' %}<p class="'${class[$i],,}''${dataProp[$j],,}'"><b>{_ '${dataProp[$j]}' _}</b> : {{ m.rsc[id].'${class[$i],,}''${dataProp[$j],,}' }}</p>{% endif %}';
                    fi;
                fi;
            fi; 
        fi;
    done; 
    echo -n '{% include "_admin_edit_content_'${class[$i],,}'.tpl" %}' > modules/mod_admin/templates/_admin_edit_content.${class[$i],,}.tpl; 
    echo -n > modules/mod_admin/templates/_admin_edit_content_${class[$i],,}.tpl; 
    cat modules/mod_admin/templates/_admin_edit_content_head modules/mod_admin/templates/_admin_edit_content_main modules/mod_admin/templates/_admin_edit_content_foot >> modules/mod_admin/templates/_admin_edit_content_${class[$i],,}.tpl; 
done;
echo $headuser$mainuser$tailuser > user/sites/$SITE/templates/page.tpl; 
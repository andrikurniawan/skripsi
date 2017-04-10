{% extends "admin_edit_widget_std.tpl" %}

{% block widget_title %}{{ _"Data Properties Volunteer"|escapejs }}{% endblock %}{% block widget_show_minimized %}false{% endblock %}{% block widget_id %}content-person{% endblock %}{% block widget_content %}<fieldset>{% with m.rsc[id] as r %}<div class="row">
<div class="form-group col-lg-4 col-md-4"><label class="control-label" for="volunteeraddress">{_ address _}</label><div><input class="form-control" id="volunteeraddress" type="text" name="volunteeraddress" value="{{ r.volunteeraddress }}" /></div></div>
<div class="form-group col-lg-4 col-md-4"><label class="control-label" for="volunteerbirthdate">{_ birthDate _}</label><div><input class="form-control" id="volunteerbirthdate" type="text" name="volunteerbirthdate" value="{{ r.volunteerbirthdate }}" /></div></div>
<div class="form-group col-lg-4 col-md-4"><label class="control-label" for="volunteerbirthplace">{_ birthPlace _}</label><div><input class="form-control" id="volunteerbirthplace" type="text" name="volunteerbirthplace" value="{{ r.volunteerbirthplace }}" /></div></div>
<div class="form-group col-lg-4 col-md-4"><label class="control-label" for="volunteereducationalbackground">{_ educationalBackground _}</label><div><input class="form-control" id="volunteereducationalbackground" type="text" name="volunteereducationalbackground" value="{{ r.volunteereducationalbackground }}" /></div></div>
<div class="form-group col-lg-4 col-md-4"><label class="control-label" for="volunteeremail">{_ email _}</label><div><input class="form-control" id="volunteeremail" type="text" name="volunteeremail" value="{{ r.volunteeremail }}" /></div></div>
<div class="form-group col-lg-4 col-md-4"><label class="control-label" for="volunteerfullname">{_ fullname _}</label><div><input class="form-control" id="volunteerfullname" type="text" name="volunteerfullname" value="{{ r.volunteerfullname }}" /></div></div>
<div class="form-group col-lg-4 col-md-4"><label class="control-label" for="volunteermaritalstatus">{_ maritalStatus _}</label><div><input class="form-control" id="volunteermaritalstatus" type="text" name="volunteermaritalstatus" value="{{ r.volunteermaritalstatus }}" /></div></div>
<div class="form-group col-lg-4 col-md-4"><label class="control-label" for="volunteeroccupation">{_ occupation _}</label><div><input class="form-control" id="volunteeroccupation" type="text" name="volunteeroccupation" value="{{ r.volunteeroccupation }}" /></div></div>
<div class="form-group col-lg-4 col-md-4"><label class="control-label" for="volunteerorganizationalexperience">{_ organizationalExperience _}</label><div><input class="form-control" id="volunteerorganizationalexperience" type="text" name="volunteerorganizationalexperience" value="{{ r.volunteerorganizationalexperience }}" /></div></div>
<div class="form-group col-lg-4 col-md-4"><label class="control-label" for="volunteerparentname">{_ parentName _}</label><div><input class="form-control" id="volunteerparentname" type="text" name="volunteerparentname" value="{{ r.volunteerparentname }}" /></div></div>
<div class="form-group col-lg-4 col-md-4"><label class="control-label" for="volunteerphonenumber">{_ phoneNumber _}</label><div><input class="form-control" id="volunteerphonenumber" type="text" name="volunteerphonenumber" value="{{ r.volunteerphonenumber }}" /></div></div>
<div class="form-group col-lg-4 col-md-4"><label class="control-label" for="volunteerquestionaboutbsmi">{_ questionAboutBSMI _}</label><div><input class="form-control" id="volunteerquestionaboutbsmi" type="text" name="volunteerquestionaboutbsmi" value="{{ r.volunteerquestionaboutbsmi }}" /></div></div>
<div class="form-group col-lg-4 col-md-4"><label class="control-label" for="volunteerquestionaboutvolunteeris">{_ questionAboutVolunteerIs _}</label><div><input class="form-control" id="volunteerquestionaboutvolunteeris" type="text" name="volunteerquestionaboutvolunteeris" value="{{ r.volunteerquestionaboutvolunteeris }}" /></div></div>
<div class="form-group col-lg-4 col-md-4"><label class="control-label" for="volunteerreasonofparticipation">{_ reasonOfParticipation _}</label><div><input class="form-control" id="volunteerreasonofparticipation" type="text" name="volunteerreasonofparticipation" value="{{ r.volunteerreasonofparticipation }}" /></div></div>
<div class="form-group col-lg-4 col-md-4"><label class="control-label" for="volunteerreligion">{_ religion _}</label><div><input class="form-control" id="volunteerreligion" type="text" name="volunteerreligion" value="{{ r.volunteerreligion }}" /></div></div>
<div class="form-group col-lg-4 col-md-4"><label class="control-label" for="volunteersex">{_ sex _}</label><div><input class="form-control" id="volunteersex" type="text" name="volunteersex" value="{{ r.volunteersex }}" /></div></div>
<div class="form-group col-lg-4 col-md-4"><label class="control-label" for="volunteerwebsite">{_ website _}</label><div><input class="form-control" id="volunteerwebsite" type="text" name="volunteerwebsite" value="{{ r.volunteerwebsite }}" /></div></div>
		</div>
    {% endwith %}
</fieldset>
{% endblock %}
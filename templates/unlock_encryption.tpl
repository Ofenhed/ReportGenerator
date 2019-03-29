
{% set title = "Unlock report" %}
{% include "default" %}
<h1>Unlock report {{report.name}}</h1>
<form method="POST" action="/report/unlock/{{report.id}}">
<label>Temporary password:</label><input type="password" name="pass"><br>
<label>Lock other reports</label><input type="checkbox" name="lock_other" checked="checked"><br>
<input type="hidden" name="csrf" value="{{csrf}}">
<input type="submit" value="Unlock">
</form>
{{ endTemplate() }}

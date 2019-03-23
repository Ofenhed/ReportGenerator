{% include "default" %}
<h1>Delete template variable</h1>
<p>This is a potentially <b>very</b> destructive action, since all related data will be deleted, including report content and childs of this variable.</p>
<form method="post">
  <input type="hidden" name="csrf" value="{{csrf}}">
  <a href="/template/{{template_id}}">Go back</a> <input type="submit" value="Delete variable" onclick="javascript:return confirm('WARNING\n\nThis will delete this template variable, all template children AND ALL RELATED REPORT DATA.\n\nOnly perform this action if you know what you are doing.');">
</form>
{{ endTemplate() }}

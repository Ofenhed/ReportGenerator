{% set title = "Add template variable" %}
{% include "default" %}
<form method="post">
  <input type="hidden" name="csrf" value="{{csrf}}">
  <label for="name">Name</label><input type="text" name="name"><br>
  {% if type == "val" %}
    <label for="value">Default value</label><textarea name="value"></textarea><br>
  {% endif %}
  <input type="submit" value="Create {{type}}">
</form>
{{ endTemplate() }}

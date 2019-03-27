{% set title = "test<script>" %}
{% include "default" %}
<table>
<tbody>
{% for t in reports %}
<tr><td><a href="/report/{{ t.id }}">({{ t.name }})</a></td></tr>
{% endfor %}
</tbody>
</table>
<form method="POST">
  <input type="text" name="name">
  <select name="template">
  {% for template in templates %}
    <option value="{{template.id}}">{{template.longName}} ({{template.includeName}})</option>
  {% endfor %}
  </select><br>
  <input type="checkbox" name="encrypted" value="1">
  <label for="encrypted">Encrypted</label><br>
  <input type="hidden" name="csrf" value="{{csrf}}">
  <input type="submit" value="New report">
</form>
{{ endTemplate() }}

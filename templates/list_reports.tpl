{% set title = "test<script>" %}
{% include "default" %}
<table>
<tbody>
{% for t in reports %}
<tr><td><a href="/report/{{ t.id }}">({{ t.name }})</a></td></tr>
{% endfor %}
</tbody>
</table>
{{ endTemplate() }}

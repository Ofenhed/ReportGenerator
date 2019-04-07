{% set title = "Autofill templates" %}
{% include "default" %}
<table>
<tbody>
{% for t in templates %}
<tr><td><a href="/autofills/{{ t.id }}">({{ t.includeName }})</a></td><td>{{ t.longName }}</td></tr>
{% endfor %}
</tbody>
</table>
{{ endTemplate() }}

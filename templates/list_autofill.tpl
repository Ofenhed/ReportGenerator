{% set title = "Autofill templates" %}
{% include "default" %}
<h1>Autofill</h1>
<table>
<tbody>
{% for t in templates %}
<tr><td><a href="/autofills/{{ t.id }}">{{ t.includeName }}</a></td><td>{{ t.longName }}</td></tr>
{% endfor %}
</tbody>
</table>
{{ endTemplate() }}

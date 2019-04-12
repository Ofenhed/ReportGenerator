{% set title = "Templates" %}
{% include "default" %}
<h1>Templates</h1>
<table>
<tbody>
{% for t in templates %}
<tr><td><a href="/template/{{ t.id }}">{{ t.includeName }}</a></td><td>{{ t.longName }}</td></tr>
{% endfor %}
</tbody>
</table>
{{ endTemplate() }}

{% set title = "Error" %}
{% include "default" %}
<h1>Error {{status}}</h1>
<div id="exception">{{exception}}</div>
{{ endTemplate() }}

{% set title = "Autofill templates - %s"|format(template.includeName) %}
{% include "default" %}
{% macro printVars(var) -%}
{%- endmacro %}

{% macro findVars(tree) -%}
  {%- for vv in tree -%}
    {%- for v in vv -%}
      {%- if v.type == "arr" -%}
        <a href="/autofill/{{v.id}}">{{- v.name -}}</a><br>
        <ul>
        {%- for saved in saved_vars[v.id] -%}
          <li><a href="/autofill/{{v.id}}/{{saved.id}}">{{- saved.name -}}</a></li>
        {%- endfor -%}
        </ul>
      {%- endif -%}
      {{- findVars(v.children) -}}
    {%- endfor -%}
  {%- endfor -%}
{%- endmacro %}
<h1>{{ template.longName }}</h1>
{{findVars(variables)}}
{% for v in arrs %}
  <a href="/autofill/{{v.id}}">{{- v.name -}}</a><br>
  <ul>
  {%- for saved in saved_vars[v.id] -%}
    <li><a href="/autofill/{{v.id}}/{{saved.id}}">{{- saved.name -}}</a></li>
  {%- endfor -%}
  </ul>
{% endfor %}
{{ endTemplate() }}

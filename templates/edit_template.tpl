{% set title = "test<script>" %}
{% include "default" %}
{% macro printVariable(var) -%}
  <li class="ul-tree">
  {%- set children = printVarTree(var.children) -%}
  {%- if children != "" -%}
    <input type="checkbox" class="ul-tree">
  {%- endif -%}
  <label>{{- var.name -}}</label>
  {{- children -}}
  </li>
{%- endmacro %}

{% macro printVarTree(tree) -%}
  {%- if tree[0]|length > 0 -%}
    <ul class="ul-tree array">
      {%- for v in tree[0] -%}
        {{- printVariable(v) -}}
      {%- endfor -%}
    </ul>
  {%- endif -%}
  {%- if tree[1]|length > 0 -%}
    <ul class="ul-tree variable">
      {%- for v in tree[1] -%}
        {{- printVariable(v) -}}
      {%- endfor -%}
    </ul>
  {%- endif -%}
{%- endmacro %}

<div class="ul-tree">
{{ printVarTree(variables) }}
</div>

{{ endTemplate() }}

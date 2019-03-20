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

<form method="post" enctype="multipart/form-data">
  <label for="source">Source</label><br>
  <textarea name="source">{{ template.source }}</textarea><br>
  <label for="editor">Editor</label><br>
  <textarea name="editor">{{ template.editor }}</textarea><br>
  <input type="checkbox" name="includable" {{ template.includable == 0 ? "" : "checked=\"checked\"" }} value="1">
  <input type="hidden" name="includable" value="0">
  <label for="includable">Includable</label><br>
  <input type="hidden" name="csrf2" value="{{ csrf }}">
  <input type="submit" value="Save">
</form>

<div class="ul-tree">
{{ printVarTree(variables) }}
</div>

{{ endTemplate() }}

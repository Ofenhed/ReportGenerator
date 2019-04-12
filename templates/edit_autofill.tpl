{%- set title = "Edit autofill - %s"|format(template_vars.name) -%}
{% include "default" %}
<style>
  td.overwrite-checkbox > input[type="checkbox"]:not(:checked) ~ textarea {
    display: none;
  }
</style>
{% macro printVariable(var) -%}
  <tr><td>
  {%- set children = printVarTree(var.children) -%}
  <label>{{- var.name -}}:</label>
    </td><td class="overwrite-checkbox"><input type="checkbox" name="set_{{- var.id -}}"><textarea name="text_{{- var.id -}}"></textarea></td>
    </tr>
  {{- children -}}
{%- endmacro %}

{% macro printVarTree(tree) -%}
  {%- if tree[1]|length > 0 -%}
      {%- for v in tree[1] -%}
        {{- printVariable(v) -}}
      {%- endfor -%}
  {%- endif -%}
{%- endmacro %}
<form method="post">
<table>
<tr><td>Name:</td><td><input type="text" name="name" value="{{ saved_vars.name }}"></td></tr>
<tr><td>Description:</td><td><textarea name="description">{{saved_vars.description}}</textarea></td></tr>
<tr><td colspan="2" style="font-weight: bold">Variables</td></tr>
<tr><td>{{ template_vars.name }}:</td><td class="overwrite-checkbox"><input type="checkbox" name="set_main_val" {% if saved_vars.data|none == False %}checked="checked"{%endif%}><textarea name="main_val">{{saved_vars.data}}</textarea></td></tr>
{% for template in flat_template_vars %}
  <tr><td>
  <label>{{- template.name -}}:</label>
    {% set var = saved_vars.var[template.id] %}
    </td><td class="overwrite-checkbox"><input type="checkbox" name="set_{{- template.id -}}" {%if var|none == False %}checked="checked"{%endif%}><textarea name="text_{{- template.id -}}">{{var.value}}</textarea></td>
    </tr>
{% endfor %}
</table>
<input type="hidden" name="csrf" value="{{csrf}}">
<input type="submit" value="{% if saved_vars|none %}Create{%else%}Save{%endif%}">
</form>

{{ endTemplate() }}

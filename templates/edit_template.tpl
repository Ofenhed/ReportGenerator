{% set title = "Template editor - %s (%s)"|format(template.name, template.includeName) %}
{% include "default" %}
{% macro printVariable(var) -%}
  <li class="ul-tree">
  {%- set children = printVarTree(var.children) -%}
  {%- if children != "" -%}
    <input type="checkbox" class="ul-tree">
  {%- endif -%}
  <label class="ul-tree">{{- var.name -}}
    <div class="ul-control">
      <a href="/template/{{template.id}}/{{var.type}}/{{var.id}}/add/var">Add variable</a>
      <a href="/template/{{template.id}}/{{var.type}}/{{var.id}}/add/arr">Add list</a>
      <a href="/template/{{template.id}}/{{var.type}}/{{var.id}}/delete">Delete</a>
    </div>
  </label>
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

<h1>{{template.includeName}}</h1>
<form method="post">
  <style>
    #editor > textarea {
      width: 100%;
      height: 20em;
      resize: vertical;
    }
    #editor > #show_editor:not(:checked) ~ textarea[name="editor"] {
      display: none;
    }
    #editor > #show_source:not(:checked) ~ textarea[name="source"] {
      display: none;
    }
    #editor > input {
      display: none;
    }
    #editor > label {
      border: 3px #bbb solid;
      border-radius: 5px 5px 0px 0px;
      padding: 5px;
      margin: 1px;
    }
    #editor > #show_source:checked ~ label[for="show_source"] {
      border: 4px #bbb solid;
      padding-top: 8px;
    }
    #editor > #show_editor:checked ~ label[for="show_editor"] {
      border: 4px #bbb solid;
      padding-top: 8px;
    }
    #editor {
      padding-top: 1em;
    }
  </style>
  <input type="text" value="{{ template.longName }}" name="template_name">
  <div id="editor">
  <input type="radio" name="ignore_editor_tab" id="show_source" checked="checked"><label for="show_source">Report</label><input type="radio" name="ignore_editor_tab" id="show_editor"><label for="show_editor">Editor</label><br>
  <textarea name="source">{{ template.source }}</textarea>
  <textarea name="editor">{{ template.editor }}</textarea>
  </div>
  <input type="checkbox" name="main_template" {{ template.includable == 0 ? "" : "checked=\"checked\"" }} value="1">
  <input type="hidden" name="main_template" value="0">
  <label for="includable">Main template</label><br>
  <input type="checkbox" name="includable" {{ template.includable == 0 ? "" : "checked=\"checked\"" }} value="1">
  <input type="hidden" name="includable" value="0">
  <label for="includable">Includable</label><br>
  <input type="hidden" name="csrf" value="{{ csrf }}">
  <input type="submit" value="Save">
</form>

<div class="ul-tree">
  <ul class="ul-tree">
    <li class="ul-tree">
      <label class="ul-tree">root
        <div class="ul-control">
          <a href="/template/{{template.id}}/add/var">Add variable</a>
          <a href="/template/{{template.id}}/add/arr">Add list</a>
        </div>
      </label>
      {{ printVarTree(variables) }}
    </li>
  </ul>
</div>

{{ endTemplate() }}

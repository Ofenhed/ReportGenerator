{% macro make_text(var) %}
  <input type="text" name="{{ add_value(var.idx) }}" value="{{ var.val }}">
{% endmacro %}
{% macro make_textarea(var) -%}
  <textarea name="{{ add_value(var.idx) }}">{{ var.val }}</textarea>
{%- endmacro %}
{% macro renderEditor() -%}
  {{ eval(src=report.editor, context={"variables": variables, "report": report, "make_text": make_text, "make_textarea": make_textarea, "add_checkbox": add_checkbox, "add_file": add_file, "add_value": add_value, "args": args, "rpc": rpc, "csrf": csrf}) }}
{%- endmacro %}
{% if rpc == 0 %}
  {% set title = "test<script>" %}
  {% include "default" %}
  <form method="post" enctype="multipart/form-data" action="/report/{{report.id}}">
  <div style="border: 1px solid #0f0">
    {{ renderEditor() }}
  </div>
  <input type="hidden" name="fields" value="{{signed_fields()}}">
  <input type="hidden" name="csrf" value="{{csrf}}">
  <input type="submit" value="Save">
  </form>
  
  Executed: <pre style="border: 1px solid #000">{{ report.editor }}</pre>
  
  {{ endTemplate() }}
{% else %}
  {{ renderEditor() }}
{% endif %}

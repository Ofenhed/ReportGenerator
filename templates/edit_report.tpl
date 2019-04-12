{% set title = "Edit report - %s"|format(report.name) %}
{% macro make_text(var) %}
  <input type="text" name="{{ add_value(var.idx) }}" value="{{ var.val }}">
{% endmacro %}
{% macro make_textarea(var) -%}
  <textarea name="{{ add_value(var.idx) }}">{{ var.val }}</textarea>
{%- endmacro %}
{% macro add_list_button(list, caption) %}
  <noscript>
    This button requires javascript to work.
  </noscript>
  <button type="button" onclick='javascript:execute_add_list({{list.idx|json|raw}})'>{{caption}}</button>
{% endmacro %}
{% macro remove_list_button(list, caption) %}
  <noscript>
    This button requires javascript to work.
  </noscript>
  <button type="button" onclick='javascript:execute_remove_list({{list.idx|json|raw}})'>{{caption}}</button>
{% endmacro %}
{% macro renderEditor() -%}
  {{ eval(src=report.editor, context={"variables": variables, "report": report, "make_text": make_text, "make_textarea": make_textarea, "add_checkbox": add_checkbox, "add_file": add_file, "add_value": add_value, "args": args, "rpc": rpc, "csrf": csrf, "add_list_button": add_list_button, "remove_list_button": remove_list_button}) }}
{%- endmacro %}
{% if rpc == 0 %}
  {% include "default" %}
  {% if report.encrypted %}{% else %}
    <div class="warning">THIS REPORT IS NOT ENCRYPTED. DO NOT ADD SENSITIVE DATA TO IT!</div>
  {% endif %}
  <form method="post" enctype="multipart/form-data" action="/report/{{report.id}}">
  <h1>{{ report.name }}</h1>
  <div style="border: 1px solid #0f0">
  {% include "template_curr" %}
  </div>
  <input type="hidden" name="fields" value="{{signed_fields()}}">
  <input type="hidden" name="csrf" value="{{csrf}}">
  <input type="submit" value="Save">
  </form>

  <a href="/report/generate/{{report.id}}">Generate report</a>
  
  Executed: <pre style="border: 1px solid #000">{{ report.editor }}</pre>

  <script>
    function execute_add_list(idx) {
      var form = document.createElement("form");
      form.action = "/report/{{report.id}}/list/add";
      form.method = "post";
      [["idx", idx], ["csrf", "{{csrf}}"]].forEach(function (e) {
        var i = document.createElement("input");
        i.type = "hidden";
        i.name = e[0];
        i.value = e[1];
        form.appendChild(i);
      });
      document.body.appendChild(form);
      form.submit();
    }
    function execute_remove_list(idx) {
      var form = document.createElement("form");
      form.action = "/report/{{report.id}}/list/add";
      form.method = "post";
      [["idx", idx], ["csrf", "{{csrf}}"]].forEach(function (e) {
        var i = document.createElement("input");
        i.type = "hidden";
        i.name = e[0];
        i.value = e[1];
        form.appendChild(i);
      });
      document.body.appendChild(form);
      form.submit();
    }
  </script>
  {{ endTemplate() }}
{% else %}
  {% include "template_curr" %}
{% endif %}

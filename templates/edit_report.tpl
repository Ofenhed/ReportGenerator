{% set title = "Edit report - %s"|format(report.name) %}
{% macro make_form_element(name, attrs) -%}
  <{{name}} {% for attr in attrs|keys %}{%if attr == "form" && attrs["form"]|none -%}
    form="main_form"
  {%- else -%}
    {{attr}}="{{attrs[attr]}}"
  {%-endif%} {% if attrs["form"]|none %}form="main_form"{% endif %}{% endfor %}>
{%- endmacro %}
{% macro make_text(var) %}
  {{ make_form_element("input", {"type":"text", "name":add_value(var.idx), "value":var.val, "form":kwargs["form"]}) }}
{% endmacro %}
{% macro make_textarea(var) -%}
  {{- make_form_element("textarea", {"name":add_value(var.idx), "form":kwargs["form"]}) -}}{{- var.val -}}</textarea>
{%- endmacro %}
{% macro make_autofill(var, autofill) %}
  {% set redirect = "" %}
  {% if kwargs["redirect"]|none == False %}
    {% set redirect = "?%s"|format(create_redirect(kwargs["redirect"])) %}
  {% endif %}
  <form action="/report/autofill/{{report.id}}{{redirect}}" method="post" target="_blank">
  <input type="hidden" name="csrf" value="{{csrf}}">
  <input type="hidden" name="path" value="{{var.idx}}">
  <select name="savedVars">
  {% for v in autofill[var.idx] %}
    <option value="{{v.id}}">{{v.name}}</option>
  {% endfor %}
  </select>
  <input type="submit" value="Add from saved">
  </form>
{% endmacro %}
{% macro make_checkbox(var, caption) %}
  {% if var.val == "1" %}
    {% set attrs = {"name":add_checkbox(var.idx), "type":"checkbox", "value":"1", "checked":"checked", "form":kwargs["form"]} %}
  {% else %}
    {% set attrs = {"name":add_checkbox(var.idx), "type":"checkbox", "value":"1", "form":kwargs["form"]} %}
  {% endif %}
  {{ make_form_element("input", attrs) }}<label>{{caption}}</label>
{% endmacro %}
{% macro add_list_button(list, caption) %}
  {% set redirect = "" %}
  {% if kwargs["redirect"]|none == False %}
    {% set redirect = "?%s"|format(create_redirect(kwargs["redirect"])) %}
  {% endif %}
  <form action="/report/{{report.id}}/list/add{{redirect}}" method="post" target="_blank">
  <input type="hidden" name="csrf" value="{{csrf}}">
  <input type="hidden" name="idx" value="{{list.idx}}">
  <input type="submit" value="{{caption}}">
  </form>
{% endmacro %}
{% macro remove_list_button(list, caption) %}
  <noscript>
    This button requires javascript to work.
  </noscript>
  <button type="button" onclick='javascript:execute_remove_list({{list.idx|json|raw}})'>{{caption}}</button>
{% endmacro %}
{% if rpc == 0 %}
  {% include "default" %}
  {% if report.encrypted %}{% else %}
    <div class="warning">THIS REPORT IS NOT ENCRYPTED. DO NOT ADD SENSITIVE DATA TO IT!</div>
  {% endif %}
  <h1>{{ report.name }}</h1>
  <div style="border: 1px solid #0f0">
  {% include "template_curr" %}
  </div>
  <form id="main_form" method="post" enctype="multipart/form-data" action="/report/{{report.id}}">
  <input type="hidden" name="fields" value="{{signed_fields()}}">
  <input type="hidden" name="csrf" value="{{csrf}}">
  <input type="submit" value="Save">
  </form>

  <a href="/report/generate/{{report.id}}">Generate report</a>
  
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

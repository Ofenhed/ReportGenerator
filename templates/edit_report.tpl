{% set title = "test<script>" %}
{% include "default" %}
<form method="post" enctype="multipart/form-data">
<div style="border: 1px solid #0f0">
{{ eval(src=report.editor, context={"variables": variables, "report": report, "add_text": add_text, "add_checkbox": add_checkbox, "add_file": add_file}) }}
</div>
<input type="hidden" name="fields" value="{{signed_fields()}}">
<input type="hidden" name="csrf" value="{{csrf}}">
<input type="submit" value="Save">
</form>

Executed: <pre style="border: 1px solid #000"> {{ report.editor }}</pre>

{{ endTemplate() }}

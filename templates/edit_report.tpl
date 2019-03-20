{% set title = "test<script>" %}
{% include "default" %}
{{ eval(src=template.editor, context={"variables": variables, "template": template}) }}

{{ endTemplate() }}

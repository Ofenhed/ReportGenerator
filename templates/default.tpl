<html>
<head>
<meta charset="UTF-8">
<title>{{ title }}</title>
<style>
div.ul-tree ul.ul-tree > li.ul-tree > ul.ul-tree {
  margin-left: 2em;
}
div.ul-tree li.ul-tree, div.ul-tree ul.ul-tree {
  list-style: none;
  padding: 0;
  margin: 0;
}
div.ul-tree li.ul-tree > label {
  display: inline-block;
  margin-left: 1.5em;
}
div.ul-tree li.ul-tree > input + label {
  margin-left: 0px;
}
div.ul-tree li.ul-tree > input {
  width: 1.5em;
  margin: 0px;
}
div.ul-tree li.ul-tree > input.ul-tree ~ ul.ul-tree
{
  display: none;
}

div.ul-tree li.ul-tree > input.ul-tree:checked ~ ul.ul-tree
{
  display: block;
}

div.ul-tree ul.ul-tree.array > li > label::before {
  content: "\01F4C1"
}

div.ul-tree ul.ul-tree.variable > li > label::before {
  content: "\01F4DD"
}

</style>
</head>
<body>

{% macro endTemplate() -%}
  </body>
</html>
{%- endmacro %}

<html>
  <head>
    <meta charset="UTF-8">
    <title>{{ title }}</title>
    <style>
      div.ul-tree ul.ul-tree > li.ul-tree > ul.ul-tree {
        margin-left: 1em;
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
      div.ul-tree li.ul-tree > label > div.ul-control {
        margin-left: 1em;
        visibility: hidden;
        display: inline-block;
      }
      div.ul-tree li.ul-tree > label > div.ul-control > a {
        color: #000;
      }
      div.ul-tree li.ul-tree > label > div.ul-control:hover > a {
        background: #000;
        text-decoration: none;
      }
      div.ul-tree li.ul-tree > label > div.ul-control:hover > a:hover {
        background: none;
      }
      div.ul-tree li.ul-tree > label:hover > div.ul-control {
        visibility: visible;
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
      body {
        max-width: 100%;
        display: grid;
        grid-template-columns: auto auto auto;
        grid-template-rows: auto;
        grid-template-areas:
          "headerleft . headerright"
          "main main main"
          "help help help"
      }
      .header-left {
        grid-area: headerleft;
      }
      .header-right {
        grid-area: headerright;
        text-align: right;
      } 
      .content {
        grid-area: main;
      }
      pre {
        white-space: pre-wrap;
      }
    </style>
  </head>
  <body>
  {% if user.id %}
    <div class="header-left"><a href="/template">Templates</a> <a href="/report">Reports</a></div>
    <div class="header-right"><a href="/user">{{ user }}</a> <a href="/logout">Log out</a></div>
  {% else %}
    <div class="header-right"><a href="/login">Log in</a></div>
  {% endif %}
    <div class="content">
{% macro endTemplate() -%}
    </div>
  </body>
</html>
{%- endmacro %}

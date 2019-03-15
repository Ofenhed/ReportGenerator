<html>
<head>
<title>{{ title }}</title>
</head>
<body>

{% script %}
macro endTemplate() {
  echo(raw('</body>
</html>'));
}
{% endscript %}

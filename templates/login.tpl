{% set title = "Login" %}
{% include "default" %}
<form method="POST">
  <input type="text" name="username">
  <input type="password" name="password">
  <input type="submit" value="Log in">
  </form>
{{ endTemplate() }}

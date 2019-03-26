{% set title = "Login" %}
{% include "default" %}
<form method="POST">
  <input type="password" name="oldPass">
  <input type="password" name="newPass">
  <input type="password" name="newPassAgain">
  <input type="hidden" name="csrf" value="{{csrf}}">
  <input type="submit" value="Change password">
</form>
{{ endTemplate() }}

{% set title = "Login" %}
{% include "default" %}
<form method="POST">
  <table>
  <tr><td>Username</td><td><input type="text" name="username"></td></tr>
  <tr><td>Password</td><td><input type="password" name="password"></td></tr>
  <tr><td>Temporary password</td><td><input class="visibility_check" type="checkbox" name="use_temp_password">
                                     <input class="visibility_target" type="password" name="temp_password"></td></tr>
  </table>
  <input type="hidden" name="csrf" value="{{csrf}}">
  <input type="submit" value="Log in">
  </form>
{{ endTemplate() }}

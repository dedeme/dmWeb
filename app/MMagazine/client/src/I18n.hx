// Generate by hxi18n. Don't modify

/// I18n management.
class I18n {

  static var enDic = [
    "Accept" => "Accept",
    "Application exit?" => "Application exit?",
    "Cancel" => "Cancel",
    "Cash Prfs." => "Cash Prfs.",
    "Change Language to %0" => "Change Language to %0",
    "Change Password" => "Change Password",
    "Check gray squares" => "Check gray squares",
    "Click %0 to continue." => "Click %0 to continue.",
    "Confirm password is missing" => "'Confirm password' is missing",
    "Current password" => "Current password",
    "Current password is missing" => "'Current password' is missing",
    "Dates of profits and ibex does not match" => "Dates of profits and ibex does not match",
    "Grey squares checks are wrong" => "Grey squares checks are wrong",
    "Home" => "Home",
    "Hot Maps" => "Hot Maps",
    "Ibex" => "Ibex",
    "Investor" => "Investor",
    "Investors average" => "Investors average",
    "Keep connected" => "Keep connected",
    "Login" => "Login",
    "Logout-message" => "<p>%0 has finished.</p><p><b>Good by!</b></p>",
    "Model" => "Model",
    "Models" => "Models",
    "New password" => "New password",
    "New password and confirm password do not match" => "New password and confirm password do not match",
    "New password is missing" => "'New password' is missing",
    "Password" => "Password",
    "Password is missing" => "Password is missing",
    "Password successfully changed" => "Password successfully changed",
    "Percentages" => "Percentages",
    "Points" => "Points",
    "Profits" => "Profits",
    "Pts." => "Pts.",
    "Rankings" => "Rankings",
    "Ref. Prfs." => "Ref. Prfs.",
    "Session is expired." => "Session is expired.",
    "Settings" => "Settings",
    "There is a new best parameter" => "There is a new best parameter",
    "This week continue the same best parameter" => "This week continue the same best parameter",
    "Total Prfs." => "Total Prfs.",
    "User" => "User",
    "User name is missing" => "User name is missing",
    "Wrong password" => "Wrong password",
    "[ See Rankings page ]" => "[ See Rankings page ]",
    "here" => "here",
    "points" => "points",
    "with" => "with"
  ];

  static var esDic = [
    "Accept" => "Aceptar",
    "Application exit?" => "¿Terminar la aplicación?",
    "Cancel" => "Cancelar",
    "Cash Prfs." => "Bfs. de Caja",
    "Change Language to %0" => "Cambiar el lenguage a %0",
    "Change Password" => "Cambiar la contraseña",
    "Check gray squares" => "Marcar los cuadrados grises",
    "Click %0 to continue." => "Hacer click %0 para continuar.",
    "Confirm password is missing" => "Falta la confirmación de la contraseña",
    "Current password" => "Contraseña actual",
    "Current password is missing" => "Falta indicar la contraseña actual",
    "Dates of profits and ibex does not match" => "Fechas de beneficios e Ibex no coinciden",
    "Grey squares checks are wrong" => "Las casillas grises están mal marcadas",
    "Home" => "Inicio",
    "Hot Maps" => "Mapas de calor",
    "Ibex" => "Ibex",
    "Investor" => "Inversor",
    "Investors average" => "Media de los inversores",
    "Keep connected" => "Mantenerse conectado",
    "Login" => "Identificación",
    "Logout-message" => "<p>%0 ha terminado.</p><p><b>¡Hasta pronto!</b></p>",
    "Model" => "Modelo",
    "Models" => "Modelos",
    "New password" => "Nueva contraseña",
    "New password and confirm password do not match" => "La nueva contraseña y su confirmación no coinciden",
    "New password is missing" => "Falta indicar la nueva contraseña",
    "Password" => "Contraseña",
    "Password is missing" => "Falta indicar la contraseña",
    "Password successfully changed" => "La contraseña se cambió correctamente",
    "Percentages" => "Porcentajes",
    "Points" => "Puntos",
    "Profits" => "Beneficios",
    "Pts." => "Pts.",
    "Rankings" => "Rankings",
    "Ref. Prfs." => "Ref. Bfs.",
    "Session is expired." => "La sesión ha expirado.",
    "Settings" => "Configuración",
    "There is a new best parameter" => "Hay un nuevo mejor parámetro",
    "This week continue the same best parameter" => "Esta semana continúa el mismo mejor parámetro",
    "Total Prfs." => "Total Bfs.",
    "User" => "Usuario",
    "User name is missing" => "Falta indicar el nombre del usuario",
    "Wrong password" => "La contraseña es incorrecta",
    "[ See Rankings page ]" => "[ Ver la página de rankings ]",
    "here" => "aquí",
    "points" => "puntos",
    "with" => "con"
  ];

  public static var lang(default, null) = "es";

  public static function en (): Void {
    lang = "en";
  }

  public static function es (): Void {
    lang = "es";
  }

  public static function _(key: String): String {
    final dic = lang == "en" ? enDic : esDic;
    return dic.exists(key) ? dic[key] : key;
  }

  public static function _args(key: String, args: Array<String>): String {
    var bf = "";
    final v = _(key);
    var isCode = false;
    for (i in 0...v.length) {
      final ch = v.charAt(i);
      if (isCode) {
        if (ch >= "0" && ch <= "9") bf += args[Std.parseInt(ch)];
        else bf += "%" + ch;
        isCode = false;
      } else if (ch == "%") {
        isCode = true;
      } else {
        bf += ch;
      }
    }
    return bf;
  }

}

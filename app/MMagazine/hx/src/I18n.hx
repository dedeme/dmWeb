// Generate by hxi18n. Don't modify

/// I18n management.
class I18n {

  static var enDic = [
    "2 Days" => "2 Days",
    "Accept" => "Accept",
    "Accounting" => "Accounting",
    "All" => "All",
    "All log entries will be deleted.\nContinue?" => "All log entries will be deleted.\nContinue?",
    "Application exit?" => "Application exit?",
    "Assets" => "Assets",
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
    "Delete" => "Delete",
    "Errors" => "Errors",
    "Grey squares checks are wrong" => "Grey squares checks are wrong",
    "Home" => "Home",
    "Hot Maps" => "Hot Maps",
    "Ibex" => "Ibex",
    "Investor" => "Investor",
    "Investors - Ibex" => "Investors - Ibex",
    "Investors average" => "Investors average",
    "Keep connected" => "Keep connected",
    "KtMarket" => "KtMarket",
    "Log" => "Log",
    "Login" => "Login",
    "Logout-message" => "<p>%0 has finished.</p><p><b>Good by!</b></p>",
    "MMarket" => "MMarket",
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
    "Reload" => "Reload",
    "Reset must be made from server!" => "Reset must be made from server!",
    "Risk" => "Risk",
    "Session is expired." => "Session is expired.",
    "Settings" => "Settings",
    "User" => "User",
    "User name is missing" => "User name is missing",
    "Wrong password" => "Wrong password",
    "here" => "here"
  ];

  static var esDic = [
    "2 Days" => "2 días",
    "Accept" => "Aceptar",
    "Accounting" => "Contabilidad",
    "All" => "Todo",
    "All log entries will be deleted.\nContinue?" => "\"Todas las entradas serán borradas.\n¿Continuar?",
    "Application exit?" => "¿Terminar la aplicación?",
    "Assets" => "Capital",
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
    "Delete" => "Borrar",
    "Errors" => "Errores",
    "Grey squares checks are wrong" => "Las casillas grises están mal marcadas",
    "Home" => "Inicio",
    "Hot Maps" => "Mapas de calor",
    "Ibex" => "Ibex",
    "Investor" => "Inversor",
    "Investors - Ibex" => "Inversores - Ibex",
    "Investors average" => "Media de los inversores",
    "Keep connected" => "Mantenerse conectado",
    "KtMarket" => "KtMarket",
    "Log" => "Registro",
    "Login" => "Identificación",
    "Logout-message" => "<p>%0 ha terminado.</p><p><b>¡Hasta pronto!</b></p>",
    "MMarket" => "MMarket",
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
    "Reload" => "Recargar",
    "Reset must be made from server!" => "¡El borrado debe ser hecho desde el servidor!",
    "Risk" => "Riesgo",
    "Session is expired." => "La sesión ha expirado.",
    "Settings" => "Configuración",
    "User" => "Usuario",
    "User name is missing" => "Falta indicar el nombre del usuario",
    "Wrong password" => "La contraseña es incorrecta",
    "here" => "aquí"
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

// Generate by hxi18n. Don't modify

/// I18n management.
class I18n {

  static var enDic = [
    "2 Days" => "2 Days",
    "Accept" => "Accept",
    "All" => "All",
    "All log entries will be deleted.\nContinue?" => "All log entries will be deleted.\nContinue?",
    "Application exit?" => "Application exit?",
    "Bets" => "Bets",
    "Cancel" => "Cancel",
    "Change Language to %0" => "Cambiar el idioma a %0",
    "Change Password" => "Change Password",
    "Check gray squares" => "Check gray squares",
    "Click %0 to continue." => "Click %0 to continue.",
    "Confirm password is missing" => "'Confirm password' is missing",
    "Current password" => "Current password",
    "Current password is missing" => "'Current password' is missing",
    "Decisions" => "Decisions",
    "Delete" => "Delete",
    "Description" => "Description",
    "Documentation" => "Documentation",
    "Errors" => "Errors",
    "Fails" => "Fails",
    "From Client" => "From Client",
    "From Server" => "From Server",
    "Grey squares checks are wrong" => "Grey squares checks are wrong",
    "Hits" => "Hits",
    "Home" => "Home",
    "Keep connected" => "Keep connected",
    "Log" => "Log",
    "Login" => "Login",
    "Logout-message" => "<p>%0 has finished.</p><p><b>Good by!</b></p>",
    "New password" => "New password",
    "New password and confirm password do not match" => "New password and confirm password do not match",
    "New password is missing" => "'New password' is missing",
    "Password" => "Password",
    "Password is missing" => "Password is missing",
    "Password successfully changed" => "Password successfully changed",
    "Points" => "Points",
    "Profits" => "Profits",
    "Reload" => "Reload",
    "Reset must be made from server!" => "Reset must be made from server!",
    "Results" => "Results",
    "Session is expired." => "Session is expired.",
    "Settings" => "Settings",
    "Standings" => "Standings",
    "Strategies" => "Strategies",
    "Strategy" => "Strategy",
    "Total" => "Total",
    "User" => "User",
    "User name is missing" => "User name is missing",
    "Wrong password" => "Wrong password",
    "here" => "here"
  ];

  static var esDic = [
    "2 Days" => "2 días",
    "Accept" => "Aceptar",
    "All" => "Todo",
    "All log entries will be deleted.\nContinue?" => "Todas las entradas serán borradas.\n¿Continuar?",
    "Application exit?" => "¿Salir de la aplicación?",
    "Bets" => "Apuestas",
    "Cancel" => "Cancelar",
    "Change Language to %0" => "Change Language to %0",
    "Change Password" => "Cambiar la contraseña",
    "Check gray squares" => "Marcar los cuadrados grises",
    "Click %0 to continue." => "Hacer click %0 para continuar.",
    "Confirm password is missing" => "Falta la confirmación de la contraseña",
    "Current password" => "Contraseña actual",
    "Current password is missing" => "Falta por indicar la contraseña actual",
    "Decisions" => "Decisiones",
    "Delete" => "Borrar",
    "Description" => "Descripción",
    "Documentation" => "Documentación",
    "Errors" => "Errores",
    "Fails" => "Fallos",
    "From Client" => "Calculado por el cliente",
    "From Server" => "Calculado por el servidor",
    "Grey squares checks are wrong" => "Las casillas grises están mal marcadas",
    "Hits" => "Aciertos",
    "Home" => "Inicio",
    "Keep connected" => "Mantenerse conectado",
    "Log" => "Registro",
    "Login" => "Identificación",
    "Logout-message" => "<p>%0 ha terminado.</p><p><b>¡Hasta pronto!</b></p>",
    "New password" => "Nueva contraseña",
    "New password and confirm password do not match" => "La nueva contraseña y su confirmación no coinciden.",
    "New password is missing" => "Falta por indicar la nueva contraseña",
    "Password" => "Contraseña",
    "Password is missing" => "Falta por indicar la contraseña",
    "Password successfully changed" => "La contraseña se cambió adecuadamente",
    "Points" => "Puntos",
    "Profits" => "Beneficios",
    "Reload" => "Recargar",
    "Reset must be made from server!" => "¡El borrado debe ser hecho desde el sevidor!",
    "Results" => "Resultados",
    "Session is expired." => "La sesión ha expirado.",
    "Settings" => "Configuración",
    "Standings" => "Clasificación",
    "Strategies" => "Estrategias",
    "Strategy" => "Estretegia",
    "Total" => "Total",
    "User" => "Usuario",
    "User name is missing" => "Falta por indicar el nombre del usuario",
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

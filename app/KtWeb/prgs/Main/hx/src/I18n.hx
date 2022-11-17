// Generate by hxi18n. Don't modify

/// I18n management.
class I18n {

  static var enDic = [
    "2 Days" => "2 Days",
    "Accept" => "Accept",
    "All" => "All",
    "All log entries will be deleted.\nContinue?" => "All log entries will be deleted.\nContinue?",
    "Application exit?" => "Application exit?",
    "Cancel" => "Cancel",
    "Change Language to %0" => "Change Language to %0",
    "Change Password" => "Change Password",
    "Check gray squares" => "Check gray squares",
    "Click %0 to continue." => "Click %0 to continue.",
    "Confirm password is missing" => "Confirm password is missing",
    "Current password" => "Current password",
    "Current password is missing" => "Current password is missing",
    "Delete" => "Delete",
    "Errors" => "Errors",
    "Grey squares checks are wrong" => "Grey squares checks are wrong",
    "Home" => "Home",
    "Keep connected" => "Keep connected",
    "Log" => "Log",
    "Login" => "Login",
    "Logout-message" => "<p>%0 has finished.</p><p><b>Good by!</b></p>",
    "New password" => "New password",
    "New password and confirm password do not match" => "New password and confirm password do not match",
    "New password is missing" => "New password is missing",
    "Password" => "Password",
    "Password is missing" => "Password is missing",
    "Password successfully changed" => "Password successfully changed",
    "Reload" => "Reload",
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
    "All" => "Todo",
    "All log entries will be deleted.\nContinue?" => "Todas las entradas serán borradas.\n¿Continuar?",
    "Application exit?" => "¿Terminar la aplicación?",
    "Cancel" => "Cancelar",
    "Change Language to %0" => "Cambiar el lenguaje a %0",
    "Change Password" => "Cambiar la contraseña",
    "Check gray squares" => "Marcar las casillas grises",
    "Click %0 to continue." => "Click %0 para continuar.",
    "Confirm password is missing" => "Falta la confirmación de la contraseña",
    "Current password" => "Contraseña actual",
    "Current password is missing" => "Falta la contraseña actual.",
    "Delete" => "Borrar",
    "Errors" => "Errores",
    "Grey squares checks are wrong" => "Las casillas grises está mal marcadas",
    "Home" => "Inicio",
    "Keep connected" => "Mantenerse conectado",
    "Log" => "Registro",
    "Login" => "Identificación",
    "Logout-message" => "<p>%0 ha terminado.</p><p><b>¡Hasta pronto!</b></p>",
    "New password" => "Nueva contraseña",
    "New password and confirm password do not match" => "La contraseña y su confirmación no coinciden",
    "New password is missing" => "Falta la nueva contraseña.",
    "Password" => "Contraseña",
    "Password is missing" => "Falta la contraseña",
    "Password successfully changed" => "Contaseña correctamente cambiada.",
    "Reload" => "Recargar",
    "Session is expired." => "Las sesión ha expirado.",
    "Settings" => "Configuración",
    "User" => "Usuario",
    "User name is missing" => "Falta el nombre del usuario",
    "Wrong password" => "Contraseña incorrecta",
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

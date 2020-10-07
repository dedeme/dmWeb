// Generate by hxi18n. Don't modify

/// I18n management.
class I18n {

  static var enDic = [
    "2 Days" => "2 Days",
    "Accept" => "Accept",
    "All" => "All",
    "All log entries will be deleted.\nContinue?" => "All log entries will be deleted.\nContinue?",
    "Application has been closed." => "Application has been closed.",
    "Cancel" => "Cancel",
    "Change Language to %0" => "Change Language to %0",
    "Change Password" => "Change Password",
    "Check gray squares" => "Check gray squares",
    "Click %0 to continue." => "Click %0 to continue.",
    "Close application?" => "Close application?",
    "Confirm password is missing" => "Confirm password is missing",
    "Current password" => "Current password",
    "Current password is missing" => "Current password is missing",
    "Delete" => "Delete",
    "Directories" => "Directories",
    "Directories in bad condition" => "Directories in bad condition",
    "Errors" => "Errors",
    "Grey squares checks are wrong" => "Grey squares checks are wrong",
    "Keep connected" => "Keep connected",
    "Log" => "Log",
    "Login" => "Login",
    "New password" => "New password",
    "New password and confirm password do not match" => "New password and confirm password do not match",
    "New password is missing" => "New password is missing",
    "Password" => "Password",
    "Password is missing" => "Password is missing",
    "Password successfully changed" => "Password successfully changed",
    "Pools" => "Pools",
    "Pools in bad condition" => "Pools in bad condition",
    "Reload" => "Reload",
    "Server is busy." => "Server is busy.",
    "Session is expired." => "Session is expired.",
    "Settings" => "Settings",
    "Summary" => "Summary",
    "User" => "User",
    "User name is missing" => "User name is missing",
    "Wrong password" => "Wrong password",
    "here" => "here"
  ];

  static var esDic = [
    "2 Days" => "2 días",
    "Accept" => "Aceptar",
    "All" => "Todo",
    "All log entries will be deleted.\nContinue?" => "Todas las anotaciones del registro serán borradas.\n¿Continuar?",
    "Application has been closed." => "La aplicación ha sido cerrada.",
    "Cancel" => "Cancelar",
    "Change Language to %0" => "Cambiar el lenguage a %0",
    "Change Password" => "Cambiar la contraseña",
    "Check gray squares" => "Marcar las casillas grises",
    "Click %0 to continue." => "Hacer click %0 para continuar.",
    "Close application?" => "¿Cerrar la aplicación?",
    "Confirm password is missing" => "Falta por indicar la confirmación de la contraseña",
    "Current password" => "Actual contraseña",
    "Current password is missing" => "Falta por indicar la actual contraseña",
    "Delete" => "Borrar",
    "Directories" => "Directorios",
    "Directories in bad condition" => "Directorios en mal estado",
    "Errors" => "Errores",
    "Grey squares checks are wrong" => "Las casillas grises están mal marcadas",
    "Keep connected" => "Mantenerse conectado",
    "Log" => "Registro",
    "Login" => "Identificación",
    "New password" => "Nueva contraseña",
    "New password and confirm password do not match" => "La nueva contraseña y su confirmación no coinciden",
    "New password is missing" => "Falta indicar la nueva contraseña",
    "Password" => "Contraseña",
    "Password is missing" => "Falta la contraseña",
    "Password successfully changed" => "Las contraseña se cambión con éxito",
    "Pools" => "Depósitos",
    "Pools in bad condition" => "Depósitos en mal estado",
    "Reload" => "Recargar",
    "Server is busy." => "El servidor está ocupado.",
    "Session is expired." => "La sesión ha expirado.",
    "Settings" => "Configuración",
    "Summary" => "Resumen",
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

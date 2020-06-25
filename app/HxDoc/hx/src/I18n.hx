// Generate by hxi18n. Don't modify

/// I18n management.
class I18n {

  static var enDic = [
    "Accept" => "Accept",
    "Application exit?" => "Application exit?",
    "Cancel" => "Cancel",
    "Change Language to %0" => "Change Language to %0",
    "Change Password" => "Change Password",
    "Check gray squares" => "Check gray squares",
    "Click %0 to continue." => "Click %0 to continue.",
    "Confirm password is missing" => "'Confirm password' is missing",
    "Current password" => "Current password",
    "Current password is missing" => "'Current password' is missing",
    "Delete %0?" => "Delete %0?",
    "Grey squares checks are wrong" => "Grey squares checks are wrong",
    "Keep connected" => "Keep connected",
    "Libraries" => "Libraries",
    "Library" => "Library",
    "Library '%0' is duplicated" => "Library '%0' is duplicated",
    "Login" => "Login",
    "Logout-message" => "<p>%0 has finished.</p><p><b>Good by!</b></p>",
    "Name '%0' contains '%1'" => "Name '%0' contains '%1'",
    "Name '%0' contains blanks" => "Name '%0' contains blanks",
    "New password" => "New password",
    "New password and confirm password do not match" => "New password and confirm password do not match",
    "New password is missing" => "'New password' is missing",
    "Password" => "Password",
    "Password is missing" => "Password is missing",
    "Password successfully changed" => "Password successfully changed",
    "Path" => "Path",
    "Path is missing." => "Path is missing",
    "Session is expired." => "Session is expired.",
    "Source is missing." => "Source is missing.",
    "There is no library" => "There is no library",
    "User" => "User",
    "User name is missing" => "User name is missing",
    "Without data" => "Without data",
    "Wrong password" => "Wrong password",
    "here" => "here"
  ];

  static var esDic = [
    "Accept" => "Aceptar",
    "Application exit?" => "¿Terminar la aplicación?",
    "Cancel" => "Cancelar",
    "Change Language to %0" => "Cambiar el lenguage a %0",
    "Change Password" => "Cambiar la contraseña",
    "Check gray squares" => "Marcar los cuadrados grises",
    "Click %0 to continue." => "Hacer click %0 para continuar.",
    "Confirm password is missing" => "Falta la confirmación de la contraseña",
    "Current password" => "Contraseña actual",
    "Current password is missing" => "Falta indicar la contraseña actual",
    "Delete %0?" => "¿Eliminar %0?",
    "Grey squares checks are wrong" => "Las casillas grises están mal marcadas",
    "Keep connected" => "Mantenerse conectado",
    "Libraries" => "Librerías",
    "Library" => "Librería",
    "Library '%0' is duplicated" => "La librería '%0' está repetida.",
    "Login" => "Identificación",
    "Logout-message" => "<p>%0 ha terminado.</p><p><b>¡Hasta pronto!</b></p>",
    "Name '%0' contains '%1'" => "El nombre '%0' contiene '%1'",
    "Name '%0' contains blanks" => "El nombre '%0' contiene blancos",
    "New password" => "Nueva contraseña",
    "New password and confirm password do not match" => "La nueva contraseña y su confirmación no coinciden",
    "New password is missing" => "Falta indicar la nueva contraseña",
    "Password" => "Contraseña",
    "Password is missing" => "Falta indicar la contraseña",
    "Password successfully changed" => "La contraseña se cambió correctamente",
    "Path" => "Ruta",
    "Path is missing." => "No se ha indicado el path",
    "Session is expired." => "La sesión ha expirado.",
    "Source is missing." => "No se ha indicado el nombre de la fuente",
    "There is no library" => "No hay ninguna librería",
    "User" => "Usuario",
    "User name is missing" => "Falta indicar el nombre del usuario",
    "Without data" => "Sin datos",
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

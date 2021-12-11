// Generate by hxi18n. Don't modify

/// I18n management.
class I18n {

  static var enDic = [
    "Accept" => "Accept",
    "Application exit?" => "Application exit?",
    "Cancel" => "Cancel",
    "Change Language to %0" => "Cambiar el idioma a %0",
    "Change Password" => "Change Password",
    "Check gray squares" => "Check gray squares",
    "Click %0 to continue." => "Click %0 to continue.",
    "Confirm password is missing" => "'Confirm password' is missing",
    "Current password" => "Current password",
    "Current password is missing" => "'Current password' is missing",
    "Delete %0?" => "Delete %0?",
    "File" => "File",
    "Grey squares checks are wrong" => "Grey squares checks are wrong",
    "Keep connected" => "Keep connected",
    "Libraries" => "Libraries",
    "Library path not found o not valid." => "Library path not found o not valid.",
    "Login" => "Login",
    "Logout-message" => "<p>%0 has finished.</p><p><b>Good by!</b></p>",
    "Name" => "Name",
    "Name '%0' contains '%1'" => "Name '%0' contains '%1'",
    "Name '%0' contains blanks" => "Name '%0' contains blanks",
    "Name '%0' is repeated" => "Name '%0' is repeated",
    "Name is missing" => "Name is missing",
    "New password" => "New password",
    "New password and confirm password do not match" => "New password and confirm password do not match",
    "New password is missing" => "'New password' is missing",
    "Overview" => "Overview",
    "Password" => "Password",
    "Password is missing" => "Password is missing",
    "Password successfully changed" => "Password successfully changed",
    "Path" => "Path",
    "Path '%0' does not start with '/'" => "Path '%0' does not start with '/'",
    "Path is '/'" => "Path is '/'",
    "Path is missing" => "Path is missing",
    "Session is expired." => "Session is expired.",
    "There are no libraries" => "There are no libraries",
    "This source can not be selected, because it does not exist" => "This source can not be selected, because it does not exist",
    "User" => "User",
    "User name is missing" => "User name is missing",
    "Wrong password" => "Wrong password",
    "[%0] C source file not found." => "[%0] C source file not found.",
    "[%0] Include file not found." => "[%0] Include file not found.",
    "here" => "here"
  ];

  static var esDic = [
    "Accept" => "Aceptar",
    "Application exit?" => "¿Salir de la aplicación?",
    "Cancel" => "Cancelar",
    "Change Language to %0" => "Change Language to %0",
    "Change Password" => "Cambiar la contraseña",
    "Check gray squares" => "Marcar los cuadrados grises",
    "Click %0 to continue." => "Hacer click %0 para continuar.",
    "Confirm password is missing" => "Falta la confirmación de la contraseña",
    "Current password" => "Contraseña actual",
    "Current password is missing" => "Falta por indicar la contraseña actual",
    "Delete %0?" => "¿Eliminar %0?",
    "File" => "Archivo",
    "Grey squares checks are wrong" => "Las casillas grises están mal marcadas",
    "Keep connected" => "Mantenerse conectado",
    "Libraries" => "Librerías",
    "Library path not found o not valid." => "La ruta de la librería no existe o no es válida.",
    "Login" => "Identificación",
    "Logout-message" => "<p>%0 ha terminado.</p><p><b>¡Hasta pronto!</b></p>",
    "Name" => "Nombre",
    "Name '%0' contains '%1'" => "El nombre '%0' contiene '%1'",
    "Name '%0' contains blanks" => "El nombre '%0' contiene espacios en blanco",
    "Name '%0' is repeated" => "El nombre está repetido",
    "Name is missing" => "Falta el nombre",
    "New password" => "Nueva contraseña",
    "New password and confirm password do not match" => "La nueva contraseña y su confirmación no coinciden.",
    "New password is missing" => "Falta por indicar la nueva contraseña",
    "Overview" => "Resumen",
    "Password" => "Contraseña",
    "Password is missing" => "Falta por indicar la contraseña",
    "Password successfully changed" => "La contraseña se cambió adecuadamente",
    "Path" => "Ruta",
    "Path '%0' does not start with '/'" => "El path '%0' no comienza con '/'",
    "Path is '/'" => "El path es '/'",
    "Path is missing" => "Falta la ruta",
    "Session is expired." => "La sesión ha expirado.",
    "There are no libraries" => "No hay librerías",
    "This source can not be selected, because it does not exist" => "Esta librería no puede seleccionarse por que no existe",
    "User" => "Usuario",
    "User name is missing" => "Falta por indicar el nombre del usuario",
    "Wrong password" => "La contraseña es incorrecta",
    "[%0] C source file not found." => "[%0] Archivo fuente C no encontrado.",
    "[%0] Include file not found." => "[%0] Archivo include no encontrado.",
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

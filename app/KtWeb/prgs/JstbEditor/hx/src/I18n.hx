// Generate by hxi18n. Don't modify

/// I18n management.
class I18n {

  static var enDic = [
    "'%0' is not a valid number" => "'%0' is not a valid number",
    "<p>File<br>%0<br>does not contain a JSON table.</p>" => "<p>File<br>%0<br>does not contain a JSON table.</p>",
    "<p>Table<br>%0<br>not found.</p>" => "<p>Table<br>%0<br>not found.</p>",
    "Accept" => "Accept",
    "Change" => "Change",
    "Change field to 'null'" => "Change field to 'null'",
    "Change field value?" => "Change field value?",
    "Check gray squares" => "Check gray squares",
    "Click %0 to continue." => "Click %0 to continue.",
    "Field configuation not valid." => "Field configuation not valid.",
    "Field path not found." => "Field path not found.",
    "Grey squares checks are wrong" => "Grey squares checks are wrong",
    "Keep connected" => "Keep connected",
    "List" => "List",
    "Login" => "Login",
    "Password" => "Password",
    "Password is missing" => "Password is missing",
    "Path is missing" => "Path is missing",
    "Remove table\n%0?" => "Remove table\n%0?",
    "Root" => "Root",
    "Session is expired." => "Session is expired.",
    "Set field to %0?" => "Set field to %0?",
    "Set to" => "Set to",
    "Table List" => "Table List",
    "Table is too big to be modified" => "Table is too big to be modified",
    "User" => "User",
    "User name is missing" => "User name is missing",
    "Without Tables" => "Without Tables",
    "Wrong password" => "Wrong password",
    "here" => "here"
  ];

  static var esDic = [
    "'%0' is not a valid number" => "'%0' no es un número válido",
    "<p>File<br>%0<br>does not contain a JSON table.</p>" => "<p>El archivo<br>%0<br>no contiene una tabla JSON.</p>",
    "<p>Table<br>%0<br>not found.</p>" => "<p>Tabla<br>%0<br>no encontrada.</p>",
    "Accept" => "Aceptar",
    "Change" => "Cambiar",
    "Change field to 'null'" => "Cambiar el campo a 'null'",
    "Change field value?" => "¿Cambiar el valor del campo?",
    "Check gray squares" => "Marcar las casillas grises",
    "Click %0 to continue." => "Click %0 para continuar.",
    "Field configuation not valid." => "La configuración del campo no es válida.",
    "Field path not found." => "Ruta del campo de la tabla no encotrado.",
    "Grey squares checks are wrong" => "Las casillas grises está mal marcadas",
    "Keep connected" => "Mantenerse conectado",
    "List" => "Lista",
    "Login" => "Identificación",
    "Password" => "Contraseña",
    "Password is missing" => "Falta la contraseña",
    "Path is missing" => "No se ha indicado la ruta",
    "Remove table\n%0?" => "¿Eliminar la tabla\n%0?",
    "Root" => "Raíz",
    "Session is expired." => "Las sesión ha expirado.",
    "Set field to %0?" => "¿Cambiar el campo a %0?",
    "Set to" => "Cambiar a",
    "Table List" => "Lista de tablas",
    "Table is too big to be modified" => "La tabla es demasiado grande para poderse cambiar.",
    "User" => "Usuario",
    "User name is missing" => "Falta el nombre del usuario",
    "Without Tables" => "Sin tablas",
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

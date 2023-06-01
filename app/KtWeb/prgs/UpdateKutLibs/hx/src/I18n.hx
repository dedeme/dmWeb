// Generate by hxi18n. Don't modify

/// I18n management.
class I18n {

  static var enDic = [
    "Accept" => "Accept",
    "Check gray squares" => "Check gray squares",
    "Click %0 to continue." => "Click %0 to continue.",
    "Deselect All" => "Deselect All",
    "Grey squares checks are wrong" => "Grey squares checks are wrong",
    "Keep connected" => "Keep connected",
    "Libraries List" => "Libraries List",
    "Library name '%0' is duplicated" => "Library name '%0' is duplicated",
    "Library target is missing" => "Library target is missing",
    "Links to '%0'" => "Links to '%0'",
    "List" => "List",
    "Login" => "Login",
    "New link to" => "New link to",
    "No link has been selected" => "No link has been selected",
    "Password" => "Password",
    "Password is missing" => "Password is missing",
    "Path can not finish at '/'" => "Path can not finish at '/'",
    "Path is missing" => "Path is missing",
    "Path is not absolute" => "Path is not absolute",
    "Remove library '%0'?" => "Remove library '%0'?",
    "Select All" => "Select All",
    "Session is expired." => "Session is expired.",
    "There was errors linking:\n%0" => "There was errors linking:\n%0",
    "User" => "User",
    "User name is missing" => "User name is missing",
    "Without Libraries" => "Without Libraries",
    "Without Links" => "Without Links",
    "Wrong password" => "Wrong password",
    "here" => "here"
  ];

  static var esDic = [
    "Accept" => "Aceptar",
    "Check gray squares" => "Marcar las casillas grises",
    "Click %0 to continue." => "Click %0 para continuar.",
    "Deselect All" => "Quitar selección",
    "Grey squares checks are wrong" => "Las casillas grises está mal marcadas",
    "Keep connected" => "Mantenerse conectado",
    "Libraries List" => "Lista de librerías",
    "Library name '%0' is duplicated" => "El nombre de librería '%0' está repetido",
    "Library target is missing" => "Falta la librería destino",
    "Links to '%0'" => "Enlaces a '%0'",
    "List" => "Lista",
    "Login" => "Identificación",
    "New link to" => "Nuevo enlace a",
    "No link has been selected" => "No se ha seleccionado ningún enlace",
    "Password" => "Contraseña",
    "Password is missing" => "Falta la contraseña",
    "Path can not finish at '/'" => "La ruta no puede terminar en '/'",
    "Path is missing" => "No se ha indicado la ruta",
    "Path is not absolute" => "La ruta no es absoluta",
    "Remove library '%0'?" => "¿Eliminar la librería '%0'?",
    "Select All" => "Seleccionar todo",
    "Session is expired." => "Las sesión ha expirado.",
    "There was errors linking:\n%0" => "Se produjeron errores enlazando:\n%0",
    "User" => "Usuario",
    "User name is missing" => "Falta el nombre del usuario",
    "Without Libraries" => "Sin librerías",
    "Without Links" => "Sin enlaces",
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

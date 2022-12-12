// Generate by hxi18n. Don't modify

/// I18n management.
class I18n {

  static var enDic = [
    "2 Days" => "2 Days",
    "Accept" => "Accept",
    "Activate" => "Activate",
    "Active" => "Active",
    "All" => "All",
    "All log entries will be deleted.\nContinue?" => "All log entries will be deleted.\nContinue?",
    "Check gray squares" => "Check gray squares",
    "Click %0 to continue." => "Click %0 to continue.",
    "Command" => "Command",
    "Command value is missing" => "Command value is missing",
    "Day" => "Day",
    "Day value is missing" => "Day value is missing",
    "Delete" => "Delete",
    "Delete annotation?" => "Delete annotation?",
    "Errors" => "Errors",
    "Fix" => "Fix",
    "Fixed days" => "Fixed days",
    "Grey squares checks are wrong" => "Grey squares checks are wrong",
    "Home" => "Home",
    "Hour - Minute" => "Hour - Minute",
    "Keep connected" => "Keep connected",
    "Log" => "Log",
    "Login" => "Login",
    "MTWRFSU" => "MTWRFSU",
    "Manual" => "Manual",
    "Password" => "Password",
    "Password is missing" => "Password is missing",
    "Periodic" => "Periodic",
    "Periodic days" => "Periodic days",
    "Reactivate" => "Reactivate",
    "Reload" => "Reload",
    "Server" => "Server",
    "Session is expired." => "Session is expired.",
    "Stop" => "Stop",
    "Stopped" => "Stopped",
    "Time is equals or previous to the current one" => "Time is equals or previous to the current one",
    "User" => "User",
    "User name is missing" => "User name is missing",
    "Week days is missing" => "Week days is missing",
    "Without entries" => "Without entries",
    "Wrong password" => "Wrong password",
    "here" => "here"
  ];

  static var esDic = [
    "2 Days" => "2 días",
    "Accept" => "Aceptar",
    "Activate" => "Activar",
    "Active" => "Activo",
    "All" => "Todo",
    "All log entries will be deleted.\nContinue?" => "Todas las entradas serán borradas.\n¿Continuar?",
    "Check gray squares" => "Marcar las casillas grises",
    "Click %0 to continue." => "Click %0 para continuar.",
    "Command" => "Instrucción",
    "Command value is missing" => "No se ha indicado la instrucción a ejecutar",
    "Day" => "Día",
    "Day value is missing" => "Falta indicar el día",
    "Delete" => "Borrar",
    "Delete annotation?" => "¿Elimimar la anotación?",
    "Errors" => "Errores",
    "Fix" => "Fijo",
    "Fixed days" => "Días fijos",
    "Grey squares checks are wrong" => "Las casillas grises está mal marcadas",
    "Home" => "Inicio",
    "Hour - Minute" => "Hora - Minuto",
    "Keep connected" => "Mantenerse conectado",
    "Log" => "Registro",
    "Login" => "Identificación",
    "MTWRFSU" => "LMXJVSD",
    "Manual" => "Manual",
    "Password" => "Contraseña",
    "Password is missing" => "Falta la contraseña",
    "Periodic" => "Periódico",
    "Periodic days" => "Dias periódicos",
    "Reactivate" => "Reactivar",
    "Reload" => "Recargar",
    "Server" => "Servidor",
    "Session is expired." => "Las sesión ha expirado.",
    "Stop" => "Detener",
    "Stopped" => "Detenido",
    "Time is equals or previous to the current one" => "El tiempo es igual o anterir al actual.",
    "User" => "Usuario",
    "User name is missing" => "Falta el nombre del usuario",
    "Week days is missing" => "No se ha indicado ningún dia de la semana",
    "Without entries" => "Sin entradas",
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

// Generate by hxi18n. Don't modify

/// I18n management.
class I18n {

  static var enDic = [
    "Accept" => "Accept",
    "Add" => "Add",
    "Bad Format. Use" => "Bad Format. Use",
    "Change Language <small>(External)</small>" => "Change Language <small>(External)</small>",
    "Change Password <small>(External)</small>" => "Change Password <small>(External)</small>",
    "Check gray squares" => "Check gray squares",
    "Click %0 to continue." => "Click %0 to continue.",
    "Duplicated alarm" => "Duplicated alarm",
    "Grey squares checks are wrong" => "Grey squares checks are wrong",
    "Home" => "Home",
    "Hour out of range" => "Hour out of range",
    "Keep connected" => "Keep connected",
    "Login" => "Login",
    "Minutes out of range" => "Minutes out of range",
    "New Alarm" => "New Alarm",
    "Password" => "Password",
    "Password is missing" => "Password is missing",
    "Programmed Alarms" => "Programmed Alarms",
    "Remove the alarm" => "Remove the alarm",
    "Separator is missing" => "Separator is missing",
    "Session is expired." => "Session is expired.",
    "Settings" => "Settings",
    "Today" => "Today",
    "Tomorrow" => "Tomorrow",
    "User" => "User",
    "User name is missing" => "User name is missing",
    "Without Alarms" => "Without Alarms",
    "Wrong password" => "Wrong password",
    "at" => "at",
    "here" => "here",
    "or" => "or"
  ];

  static var esDic = [
    "Accept" => "Aceptar",
    "Add" => "Añadir",
    "Bad Format. Use" => "Mal formato. Use:",
    "Change Language <small>(External)</small>" => "Cambiar lenguage <small>(Externo)</small>",
    "Change Password <small>(External)</small>" => "Cambiar contraseña <small>(Externo)</small>",
    "Check gray squares" => "Marcar las casillas grises",
    "Click %0 to continue." => "Click %0 para continuar.",
    "Duplicated alarm" => "Alarma ya existente",
    "Grey squares checks are wrong" => "Las casillas grises está mal marcadas",
    "Home" => "Inicio",
    "Hour out of range" => "La hora está fuera de rango",
    "Keep connected" => "Mantenerse conectado",
    "Login" => "Identificación",
    "Minutes out of range" => "Los minutos están fuera de rango.",
    "New Alarm" => "Nueva alarma",
    "Password" => "Contraseña",
    "Password is missing" => "Falta la contraseña",
    "Programmed Alarms" => "Alarmas programadas",
    "Remove the alarm" => "Eliminar la alarma",
    "Separator is missing" => "Falta el separador",
    "Session is expired." => "Las sesión ha expirado.",
    "Settings" => "Configuración",
    "Today" => "Hoy",
    "Tomorrow" => "Mañana",
    "User" => "Usuario",
    "User name is missing" => "Falta el nombre del usuario",
    "Without Alarms" => "Sin alarmas",
    "Wrong password" => "Contraseña incorrecta",
    "at" => "a las",
    "here" => "aquí",
    "or" => "o"
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

// Generate by hxi18n. Don't modify

/// I18n management.
class I18n {

  static var enDic = [
    "Accept" => "Accept",
    "Application exit?" => "Application exit?",
    "Bills" => "Bills",
    "Change Language <small>(External)</small>" => "Change Language <small>(External)</small>",
    "Change Password <small>(External)</small>" => "Change Password <small>(External)</small>",
    "Check gray squares" => "Check gray squares",
    "Click %0 to continue." => "Click %0 to continue.",
    "Grey squares checks are wrong" => "Grey squares checks are wrong",
    "Keep connected" => "Keep connected",
    "Login" => "Login",
    "Logout-message" => "<p>%0 has finished.</p><p><b>Good by!</b></p>",
    "Password" => "Password",
    "Password is missing" => "Password is missing",
    "Session is expired." => "Session is expired.",
    "Settings" => "Settings",
    "Stays" => "Stays",
    "User" => "User",
    "User name is missing" => "User name is missing",
    "Wrong password" => "Wrong password",
    "here" => "here"
  ];

  static var esDic = [
    "Accept" => "Aceptar",
    "Application exit?" => "¿Terminar la aplicación?",
    "Bills" => "Gastos",
    "Change Language <small>(External)</small>" => "Cambiar lenguage <small>(Externo)</small>",
    "Change Password <small>(External)</small>" => "Cambiar contraseña <small>(Externo)</small>",
    "Check gray squares" => "Marcar las casillas grises",
    "Click %0 to continue." => "Click %0 para continuar.",
    "Grey squares checks are wrong" => "Las casillas grises está mal marcadas",
    "Keep connected" => "Mantenerse conectado",
    "Login" => "Identificación",
    "Logout-message" => "<p>%0 ha terminado.</p><p><b>¡Hasta pronto!</b></p>",
    "Password" => "Contraseña",
    "Password is missing" => "Falta la contraseña",
    "Session is expired." => "Las sesión ha expirado.",
    "Settings" => "Configuración",
    "Stays" => "Estancias",
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

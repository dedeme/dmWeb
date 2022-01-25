// Generate by hxi18n. Don't modify

/// I18n management.
class I18n {

  static var enDic = [
    "Accept" => "Accept",
    "Application exit?" => "Application exit?",
    "Assets" => "Assets",
    "Base" => "Base",
    "Base Increment" => "Base Increment",
    "Cancel" => "Cancel",
    "Change Language to %0" => "Change Language to %0",
    "Change Password" => "Change Password",
    "Charts" => "Charts",
    "Check gray squares" => "Check gray squares",
    "Click %0 to continue." => "Click %0 to continue.",
    "Companies" => "Companies",
    "Confirm password is missing" => "'Confirm password' is missing",
    "Current password" => "Current password",
    "Current password is missing" => "'Current password' is missing",
    "Description" => "Description",
    "Environment Increment" => "Environment Increment",
    "Eval." => "Eval.",
    "Grey squares checks are wrong" => "Grey squares checks are wrong",
    "H. Eval." => "H. Eval.",
    "H. Sales" => "H. Sales",
    "Historic" => "Historic",
    "Hot Map" => "Hot Map",
    "Id" => "Id",
    "Keep connected" => "Keep connected",
    "Login" => "Login",
    "Logout-message" => "<p>%0 has finished.</p><p><b>Good by!</b></p>",
    "Model" => "Model",
    "Models" => "Models",
    "Name" => "Name",
    "New password" => "New password",
    "New password and confirm password do not match" => "New password and confirm password do not match",
    "New password is missing" => "'New password' is missing",
    "Operations" => "Operations",
    "Password" => "Password",
    "Password is missing" => "Password is missing",
    "Password successfully changed" => "Password successfully changed",
    "Profits (%)" => "Profits (%)",
    "Results" => "Results",
    "Sales" => "Sales",
    "Session is expired." => "Session is expired.",
    "Settings" => "Settings",
    "There is no hot map for %0 parameters" => "There is no hot map for %0 parameters",
    "User" => "User",
    "User name is missing" => "User name is missing",
    "Wrong password" => "Wrong password",
    "here" => "here"
  ];

  static var esDic = [
    "Accept" => "Aceptar",
    "Application exit?" => "¿Terminar la aplicación?",
    "Assets" => "Activo",
    "Base" => "Base",
    "Base Increment" => "Incremento de la base",
    "Cancel" => "Cancelar",
    "Change Language to %0" => "Cambiar el lenguage a %0",
    "Change Password" => "Cambiar la contraseña",
    "Charts" => "Gráficos",
    "Check gray squares" => "Marcar los cuadrados grises",
    "Click %0 to continue." => "Hacer click %0 para continuar.",
    "Companies" => "Compañías",
    "Confirm password is missing" => "Falta la confirmación de la contraseña",
    "Current password" => "Contraseña actual",
    "Current password is missing" => "Falta indicar la contraseña actual",
    "Description" => "Descripción",
    "Environment Increment" => "Incremento del entorno",
    "Eval." => "Eval.",
    "Grey squares checks are wrong" => "Las casillas grises están mal marcadas",
    "H. Eval." => "H. Eval.",
    "H. Sales" => "H. Ventas",
    "Historic" => "Histórico",
    "Hot Map" => "Mapa de calor",
    "Id" => "Id",
    "Keep connected" => "Mantenerse conectado",
    "Login" => "Identificación",
    "Logout-message" => "<p>%0 ha terminado.</p><p><b>¡Hasta pronto!</b></p>",
    "Model" => "Modelo",
    "Models" => "Modelos",
    "Name" => "Nombre",
    "New password" => "Nueva contraseña",
    "New password and confirm password do not match" => "La nueva contraseña y su confirmación no coinciden",
    "New password is missing" => "Falta indicar la nueva contraseña",
    "Operations" => "Operaciones",
    "Password" => "Contraseña",
    "Password is missing" => "Falta indicar la contraseña",
    "Password successfully changed" => "La contraseña se cambió correctamente",
    "Profits (%)" => "Beneficios (%)",
    "Results" => "Resultados",
    "Sales" => "Ventas",
    "Session is expired." => "La sesión ha expirado.",
    "Settings" => "Configuración",
    "There is no hot map for %0 parameters" => "No hay mapa de calor para %0 parámetros",
    "User" => "Usuario",
    "User name is missing" => "Falta indicar el nombre del usuario",
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

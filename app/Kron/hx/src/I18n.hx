// Generate by hxi18n. Don't modify

/// I18n management.
class I18n {

  static var enDic = [
    "'Command / Message' value is missing" => "'Command / Message' value is missing",
    "'Command' value is missing" => "'Command' value is missing",
    "'Text' value is missing" => "'Text' value is missing",
    "2 Days" => "2 Days",
    "Accept" => "Accept",
    "All" => "All",
    "All log entries will be deleted.\nContinue?" => "All log entries will be deleted.\nContinue?",
    "Application exit?" => "Application exit?",
    "Cancel" => "Cancel",
    "Change Language to %0" => "Cambiar el idioma a %0",
    "Change Password" => "Change Password",
    "Check gray squares" => "Check gray squares",
    "Click %0 to continue." => "Click %0 to continue.",
    "Command" => "Command",
    "Command / Message" => "Command / Message",
    "Confirm password is missing" => "'Confirm password' is missing",
    "Current password" => "Current password",
    "Current password is missing" => "'Current password' is missing",
    "Date is equals or less than today" => "Date is equals or less than today",
    "Day" => "Day",
    "Day value is missing" => "Day value is missing",
    "Delete" => "Delete",
    "Delete annotation?" => "Delete annotation?",
    "Errors" => "Errors",
    "Fix" => "Fix",
    "Fixed days" => "Fixed days",
    "Grey squares checks are wrong" => "Grey squares checks are wrong",
    "Group" => "Group",
    "Group is missing" => "Group is missing",
    "Home" => "Home",
    "Hour - Minute" => "Hour - Minute",
    "Initialization" => "Initialization",
    "Keep connected" => "Keep connected",
    "Log" => "Log",
    "Login" => "Login",
    "Logout-message" => "<p>%0 has finished.</p><p><b>Good by!</b></p>",
    "MESSAGE" => "MESSAGE",
    "MTWRFSU" => "MTWRFSU",
    "Message" => "Message",
    "New password" => "New password",
    "New password and confirm password do not match" => "New password and confirm password do not match",
    "New password is missing" => "'New password' is missing",
    "Notes" => "Notes",
    "Password" => "Password",
    "Password is missing" => "Password is missing",
    "Password successfully changed" => "Password successfully changed",
    "Periodic" => "Periodic",
    "Periodic days" => "Periodic days",
    "Reload" => "Reload",
    "Session is expired." => "Session is expired.",
    "Settings" => "Settings",
    "Text" => "Text",
    "Type" => "Type",
    "User" => "User",
    "User name is missing" => "User name is missing",
    "Week days is missing" => "Week days is missing",
    "Without entries" => "Without entries",
    "Wrong password" => "Wrong password",
    "here" => "here"
  ];

  static var esDic = [
    "'Command / Message' value is missing" => "Falta el valor de 'Comando / Mensaje'",
    "'Command' value is missing" => "No se ha indicado el commando",
    "'Text' value is missing" => "No se ha indicado el texto",
    "2 Days" => "2 Días",
    "Accept" => "Aceptar",
    "All" => "Todo",
    "All log entries will be deleted.\nContinue?" => "Todas las entradas serán eliminadas.\n¿Continuar?",
    "Application exit?" => "¿Salir de la aplicación?",
    "Cancel" => "Cancelar",
    "Change Language to %0" => "Change Language to %0",
    "Change Password" => "Cambiar la contraseña",
    "Check gray squares" => "Marcar los cuadrados grises",
    "Click %0 to continue." => "Hacer click %0 para continuar.",
    "Command" => "Comando",
    "Command / Message" => "Comando / Mensaje",
    "Confirm password is missing" => "Falta la confirmación de la contraseña",
    "Current password" => "Contraseña actual",
    "Current password is missing" => "Falta por indicar la contraseña actual",
    "Date is equals or less than today" => "La fecha es igual o anterior a hoy",
    "Day" => "Día",
    "Day value is missing" => "Falta el valor del Día.",
    "Delete" => "Eliminar",
    "Delete annotation?" => "¿Eliminar la anotación?",
    "Errors" => "Errores",
    "Fix" => "Fijo",
    "Fixed days" => "Días fijos",
    "Grey squares checks are wrong" => "Las casillas grises están mal marcadas",
    "Group" => "Grupo",
    "Group is missing" => "No se ha indicado el grupo",
    "Home" => "Inicio",
    "Hour - Minute" => "Hora - Minuto",
    "Initialization" => "Inicialización",
    "Keep connected" => "Mantenerse conectado",
    "Log" => "Registro",
    "Login" => "Identificación",
    "Logout-message" => "<p>%0 ha terminado.</p><p><b>¡Hasta pronto!</b></p>",
    "MESSAGE" => "MENSAJE",
    "MTWRFSU" => "LMXJVSD",
    "Message" => "Mensaje",
    "New password" => "Nueva contraseña",
    "New password and confirm password do not match" => "La nueva contraseña y su confirmación no coinciden.",
    "New password is missing" => "Falta por indicar la nueva contraseña",
    "Notes" => "Notas",
    "Password" => "Contraseña",
    "Password is missing" => "Falta por indicar la contraseña",
    "Password successfully changed" => "La contraseña se cambió adecuadamente",
    "Periodic" => "Periódico",
    "Periodic days" => "Días periódicos",
    "Reload" => "Recargar",
    "Session is expired." => "La sesión ha expirado.",
    "Settings" => "Configuración",
    "Text" => "Texto",
    "Type" => "Tipo",
    "User" => "Usuario",
    "User name is missing" => "Falta por indicar el nombre del usuario",
    "Week days is missing" => "No se ha indicado el día de la semana",
    "Without entries" => "Sin entradas",
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

// Generate by hxi18n. Don't modify

/// I18n management.
class I18n {

  static var enDic = [
    "2 Days" => "2 Days",
    "Accept" => "Accept",
    "Accounting" => "Accounting",
    "All" => "All",
    "All log entries will be deleted.\nContinue?" => "All log entries will be deleted.\nContinue?",
    "Annotations" => "Annotations",
    "Annotations & Settings" => "Annotations & Settings",
    "Application exit?" => "Application exit?",
    "Calendar" => "Calendar",
    "Cancel" => "Cancel",
    "Change Language to %0" => "Change Language to %0",
    "Change Password" => "Change Password",
    "Check gray squares" => "Check gray squares",
    "Click %0 to continue." => "Click %0 to continue.",
    "Close" => "Close",
    "CloseN" => "Close",
    "Confirm password is missing" => "'Confirm password' is missing",
    "Current password" => "Current password",
    "Current password is missing" => "'Current password' is missing",
    "Daily Quotes" => "Daily Quotes",
    "Date" => "Date",
    "Date '%0' is duplicated" => "Date '%0' is duplicated",
    "Date before today" => "Date before today",
    "Delete" => "Delete",
    "Delete '%0'?" => "Delete '%0'?",
    "Errors" => "Errors",
    "Fleas" => "Fleas",
    "General Time Table" => "General Time Table",
    "Grey squares checks are wrong" => "Grey squares checks are wrong",
    "Holidays" => "Holidays",
    "Home" => "Home",
    "Keep connected" => "Keep connected",
    "Log" => "Log",
    "Login" => "Login",
    "Logout-message" => "<p>%0 has finished.</p><p><b>Good by!</b></p>",
    "Models" => "Models",
    "New password" => "New password",
    "New password and confirm password do not match" => "New password and confirm password do not match",
    "New password is missing" => "'New password' is missing",
    "Nicks" => "Nicks",
    "Open" => "Open",
    "Operation failed.\nSee log." => "Operation failed.\nSee log.",
    "Operation successfully done." => "Operation successfully done.",
    "Password" => "Password",
    "Password is missing" => "Password is missing",
    "Password successfully changed" => "Password successfully changed",
    "Ranking" => "Ranking",
    "Reload" => "Reload",
    "Servers" => "Servers",
    "Session is expired." => "Session is expired.",
    "Settings" => "Settings",
    "Special days" => "Special days",
    "User" => "User",
    "User name is missing" => "User name is missing",
    "Without dates" => "Without dates",
    "Wrong password" => "Wrong password",
    "here" => "here"
  ];

  static var esDic = [
    "2 Days" => "2 Días",
    "Accept" => "Aceptar",
    "Accounting" => "Contabilidad",
    "All" => "Todo",
    "All log entries will be deleted.\nContinue?" => "Todas las entradas del registro será borradas.\n¿Continuar?",
    "Annotations" => "Asientos",
    "Annotations & Settings" => "Anotaciones & configuración",
    "Application exit?" => "¿Salir de la aplicación?",
    "Calendar" => "Calendario",
    "Cancel" => "Cancelar",
    "Change Language to %0" => "Cambiar el idioma a %0",
    "Change Password" => "Cambiar la contraseña",
    "Check gray squares" => "Marcar los cuadrados grises",
    "Click %0 to continue." => "Hacer click %0 para continuar.",
    "Close" => "Cerrar",
    "CloseN" => "Cierre",
    "Confirm password is missing" => "Falta la confirmación de la contraseña",
    "Current password" => "Contraseña actual",
    "Current password is missing" => "Falta indicar la contraseña actual",
    "Daily Quotes" => "Cotizaciones del día",
    "Date" => "Fecha",
    "Date '%0' is duplicated" => "La fecha '%0' está repetida",
    "Date before today" => "Fecha anterior a hoy",
    "Delete" => "Eliminar",
    "Delete '%0'?" => "¿Eliminar '%0'?",
    "Errors" => "Errores",
    "Fleas" => "Pulgas",
    "General Time Table" => "Horario general",
    "Grey squares checks are wrong" => "Las casillas grises están mal marcadas",
    "Holidays" => "Fiestas",
    "Home" => "Inicio",
    "Keep connected" => "Mantenerse conectado",
    "Log" => "Registro",
    "Login" => "Identificación",
    "Logout-message" => "<p>%0 ha terminado.</p><p><b>¡Hasta pronto!</b></p>",
    "Models" => "Modelos",
    "New password" => "Nueva contraseña",
    "New password and confirm password do not match" => "La nueva contraseña y su confirmación no coinciden",
    "New password is missing" => "Falta indicar la nueva contraseña",
    "Nicks" => "Nicks",
    "Open" => "Apertura",
    "Operation failed.\nSee log." => "La operación ha fallado.\nVer el registro.",
    "Operation successfully done." => "Operatión terminada correctamente.",
    "Password" => "Contraseña",
    "Password is missing" => "Falta indicar la contraseña",
    "Password successfully changed" => "La contraseña se cambió correctamente",
    "Ranking" => "Clasificación",
    "Reload" => "Recargar",
    "Servers" => "Servidores",
    "Session is expired." => "La sesión ha expirado.",
    "Settings" => "Configuración",
    "Special days" => "Días especiales",
    "User" => "Usuario",
    "User name is missing" => "Falta indicar el nombre del usuario",
    "Without dates" => "Sin fechas",
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

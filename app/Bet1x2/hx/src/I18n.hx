// Generate by hxi18n. Don't modify

/// I18n management.
class I18n {

  static var enDic = [
    "'%0' is not a valid number" => "'%0' is not a valid number",
    "Accept" => "Accept",
    "Add" => "Add",
    "Add matchday %0" => "Add matchday %0",
    "All Clubs" => "All Clubs",
    "Application exit?" => "Application exit?",
    "Away team is missing" => "Away team is missing",
    "Bet." => "Bet",
    "Bets" => "Bets",
    "Cancel" => "Cancel",
    "Change Language to %0" => "Change Language to %0",
    "Change Password" => "Change Password",
    "Change of Year" => "Change of Year",
    "Check gray squares" => "Check gray squares",
    "Click %0 to continue." => "Click %0 to continue.",
    "Clubs" => "Clubs",
    "Confirm password is missing" => "'Confirm password' is missing",
    "Current password" => "Current password",
    "Current password is missing" => "'Current password' is missing",
    "Date" => "Date",
    "Date does not match to the selected year" => "Date does not match to the selected year",
    "Delete '%0'?" => "Delete '%0'?",
    "Fees." => "Fees",
    "Grey squares checks are wrong" => "Grey squares checks are wrong",
    "Home and away teams are the same" => "Home and away teams are the same",
    "Home team is missing" => "Home team is missing",
    "Identifier duplicated" => "Identifier duplicated",
    "Identifier is missing" => "Identifier is missing",
    "Keep connected" => "Keep connected",
    "Login" => "Login",
    "Logout-message" => "<p>%0 has finished.</p><p><b>Good by!</b></p>",
    "Lost" => "Lost",
    "Match" => "Match",
    "Matchday" => "Matchday",
    "Modify" => "Modify",
    "Name is missing" => "Name is missing",
    "New password" => "New password",
    "New password and confirm password do not match" => "New password and confirm password do not match",
    "New password is missing" => "'New password' is missing",
    "Password" => "Password",
    "Password is missing" => "Password is missing",
    "Password successfully changed" => "Password successfully changed",
    "Payments" => "Payments",
    "Prof." => "Prof.",
    "Profits" => "Profits",
    "Quit '%0'?" => "Quit '%0'?",
    "Res." => "Res.",
    "Results" => "Results",
    "Session is expired." => "Session is expired.",
    "Settings" => "Settings",
    "User" => "User",
    "User name is missing" => "User name is missing",
    "Without clubs" => "Without clubs",
    "Without matchdays" => "Without matchdays",
    "Won" => "Won",
    "Wrong password" => "Wrong password",
    "Year Clubs" => "Year Clubs",
    "here" => "here"
  ];

  static var esDic = [
    "'%0' is not a valid number" => "'%0' no es un número válido",
    "Accept" => "Aceptar",
    "Add" => "Añadir",
    "Add matchday %0" => "Añadir %0ª jornada",
    "All Clubs" => "Todos los clubs",
    "Application exit?" => "¿Terminar la aplicación?",
    "Away team is missing" => "Falta el equipo que juega fuera",
    "Bet." => "Apues.",
    "Bets" => "Apuestas",
    "Cancel" => "Cancelar",
    "Change Language to %0" => "Cambiar el lenguage a %0",
    "Change Password" => "Cambiar la contraseña",
    "Change of Year" => "Cambio de año",
    "Check gray squares" => "Marcar los cuadrados grises",
    "Click %0 to continue." => "Hacer click %0 para continuar.",
    "Clubs" => "Clubs",
    "Confirm password is missing" => "Falta la confirmación de la contraseña",
    "Current password" => "Contraseña actual",
    "Current password is missing" => "Falta indicar la contraseña actual",
    "Date" => "Fecha",
    "Date does not match to the selected year" => "La fecha no corresponde al año seleccionado",
    "Delete '%0'?" => "¿Eliminar '%0'?",
    "Fees." => "Comis.",
    "Grey squares checks are wrong" => "Las casillas grises están mal marcadas",
    "Home and away teams are the same" => "El equipo que juega fuera y en casa es el mismo",
    "Home team is missing" => "Falta el equipo que juega en casa",
    "Identifier duplicated" => "Identificador repetido",
    "Identifier is missing" => "Falta el identificador",
    "Keep connected" => "Mantenerse conectado",
    "Login" => "Identificación",
    "Logout-message" => "<p>%0 ha terminado.</p><p><b>¡Hasta pronto!</b></p>",
    "Lost" => "Perdidas",
    "Match" => "Partido",
    "Matchday" => "Jornada",
    "Modify" => "Modificar",
    "Name is missing" => "Falta el nombre",
    "New password" => "Nueva contraseña",
    "New password and confirm password do not match" => "La nueva contraseña y su confirmación no coinciden",
    "New password is missing" => "Falta indicar la nueva contraseña",
    "Password" => "Contraseña",
    "Password is missing" => "Falta indicar la contraseña",
    "Password successfully changed" => "La contraseña se cambió correctamente",
    "Payments" => "Pagos",
    "Prof." => "Benef.",
    "Profits" => "Beneficios",
    "Quit '%0'?" => "¿Quitar '%0'?",
    "Res." => "Res.",
    "Results" => "Resultados",
    "Session is expired." => "La sesión ha expirado.",
    "Settings" => "Configuración",
    "User" => "Usuario",
    "User name is missing" => "Falta indicar el nombre del usuario",
    "Without clubs" => "Sin clubs",
    "Without matchdays" => "Sin jornadas",
    "Won" => "Ganadas",
    "Wrong password" => "La contraseña es incorrecta",
    "Year Clubs" => "Clubs de la temporada",
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

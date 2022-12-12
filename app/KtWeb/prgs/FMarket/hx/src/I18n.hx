// Generate by hxi18n. Don't modify

/// I18n management.
class I18n {

  static var enDic = [
    "2 Days" => "2 Days",
    "Accept" => "Accept",
    "All" => "All",
    "All log entries will be deleted.\nContinue?" => "All log entries will be deleted.\nContinue?",
    "AllRankings" => "All",
    "Assets" => "Assets",
    "Assets average" => "Assets average",
    "Bests" => "Bests",
    "Change Language <small>(External)</small>" => "Change Language <small>(External)</small>",
    "Change Password <small>(External)</small>" => "Change Password <small>(External)</small>",
    "Check gray squares" => "Check gray squares",
    "Click %0 to continue." => "Click %0 to continue.",
    "Current" => "Current",
    "Cycle" => "Cycle",
    "Delete" => "Delete",
    "Duplicates" => "Duplicates",
    "Errors" => "Errors",
    "Evaluating" => "Evaluating",
    "Fleas number" => "Fleas number",
    "Generating" => "Generating",
    "Grey squares checks are wrong" => "Grey squares checks are wrong",
    "Home" => "Home",
    "Id" => "Id",
    "Keep connected" => "Keep connected",
    "Log" => "Log",
    "Login" => "Login",
    "Models" => "Models",
    "Ms." => "Ms.",
    "Password" => "Password",
    "Password is missing" => "Password is missing",
    "Pos." => "Pos.",
    "Position average" => "Position average",
    "Ranking" => "Ranking",
    "Rankings" => "Rankings",
    "Reload" => "Reload",
    "Saving" => "Saving",
    "Selecting" => "Selecting",
    "Session is expired." => "Session is expired.",
    "Settings" => "Settings",
    "Start" => "Start",
    "Stopped" => "Stopped",
    "User" => "User",
    "User name is missing" => "User name is missing",
    "Worsts" => "Worsts",
    "Wrong password" => "Wrong password",
    "here" => "here"
  ];

  static var esDic = [
    "2 Days" => "2 días",
    "Accept" => "Aceptar",
    "All" => "Todo",
    "All log entries will be deleted.\nContinue?" => "Todas las entradas serán borradas.\n¿Continuar?",
    "AllRankings" => "Todas",
    "Assets" => "Patrimonio",
    "Assets average" => "Media del patrimonio",
    "Bests" => "Mejores",
    "Change Language <small>(External)</small>" => "Cambiar lenguage <small>(Externo)</small>",
    "Change Password <small>(External)</small>" => "Cambiar contraseña <small>(Externo)</small>",
    "Check gray squares" => "Marcar las casillas grises",
    "Click %0 to continue." => "Click %0 para continuar.",
    "Current" => "Actual",
    "Cycle" => "Ciclo",
    "Delete" => "Borrar",
    "Duplicates" => "Repeticiones",
    "Errors" => "Errores",
    "Evaluating" => "Evaluando",
    "Fleas number" => "Numero de pulgas",
    "Generating" => "Generando",
    "Grey squares checks are wrong" => "Las casillas grises está mal marcadas",
    "Home" => "Inicio",
    "Id" => "Id",
    "Keep connected" => "Mantenerse conectado",
    "Log" => "Registro",
    "Login" => "Identificación",
    "Models" => "Modelos",
    "Ms." => "Ms.",
    "Password" => "Contraseña",
    "Password is missing" => "Falta la contraseña",
    "Pos." => "Pos.",
    "Position average" => "Posición media",
    "Ranking" => "Clasificación",
    "Rankings" => "Clasificaciones",
    "Reload" => "Recargar",
    "Saving" => "Guardando",
    "Selecting" => "Seleccionando",
    "Session is expired." => "Las sesión ha expirado.",
    "Settings" => "Configuración",
    "Start" => "Comenzar",
    "Stopped" => "Detenido",
    "User" => "Usuario",
    "User name is missing" => "Falta el nombre del usuario",
    "Worsts" => "Peores",
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

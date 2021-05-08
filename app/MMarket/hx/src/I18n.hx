// Generate by hxi18n. Don't modify

/// I18n management.
class I18n {

  static var enDic = [
    "Operation failed.\nSee log." => "Operation failed.\nSee log.",
    "Operation successfully done." => "Operation successfully done.",
    "Session is expired." => "Session is expired."
  ];

  static var esDic = [
    "Operation failed.\nSee log." => "La operación ha fallado.\nVer el registro.",
    "Operation successfully done." => "Operatión terminada correctamente.",
    "Session is expired." => "La sesión ha expirado."
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

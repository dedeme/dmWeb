// Generate by hxi18n. Don't modify

/// I18n management.
class I18n {

  static var enDic = [
    "???" => "???",
    "Accept" => "Accept",
    "Application exit?" => "Application exit?",
    "Back" => "Back",
    "Cancel" => "Cancel",
    "Change Language" => "Change Language",
    "Change Password" => "Change Password",
    "Check gray squares" => "Check gray squares",
    "Click %0 to continue." => "Click %0 to continue.",
    "Close" => "Close",
    "Confirm password is missing" => "'Confirm password' is missing",
    "Current password" => "Current password",
    "Current password is missing" => "'Current password' is missing",
    "Dance Management" => "Dance Management",
    "Dance Songs Management" => "Dance Songs Management",
    "Fast" => "Fast",
    "Good" => "Good",
    "Grey squares checks are wrong" => "Grey squares checks are wrong",
    "Group" => "Group",
    "Groups" => "Groups",
    "Keep connected" => "Keep connected",
    "Login" => "Login",
    "Logout-message" => "<p>%0 has finished.</p><p><b>Good by!</b></p>",
    "Long Dance" => "Long Dance",
    "Long Dance Songs" => "Long Dance Songs",
    "More" => "More",
    "Name" => "Name",
    "New password" => "New password",
    "New password and confirm password do not match" => "New password and confirm password do not match",
    "New password is missing" => "'New password' is missing",
    "Ok" => "Ok",
    "Password" => "Password",
    "Password is missing" => "Password is missing",
    "Password successfully changed" => "Password successfully changed",
    "Picture" => "Picture",
    "Pictures Management" => "Pictures Management",
    "Quality" => "Quality",
    "Session is expired." => "Session is expired.",
    "Short Dance" => "Short Dance",
    "Short Dance Songs" => "Short Dance Songs",
    "Sights" => "Sights",
    "Slow" => "Slow",
    "Song" => "Song",
    "Songs Management" => "Songs Management",
    "Speed" => "Speed",
    "Stand By" => "Stand By",
    "Times Management" => "Times Management",
    "User" => "User",
    "User name is missing" => "User name is missing",
    "Wallpapers" => "Wallpapers",
    "Wallpapers with Music" => "Wallpapers with Music",
    "Wrong password" => "Wrong password",
    "here" => "here"
  ];

  static var esDic = [
    "???" => "???",
    "Accept" => "Aceptar",
    "Application exit?" => "¿Terminar la aplicación?",
    "Back" => "Atrás",
    "Cancel" => "Cancelar",
    "Change Language" => "Cambiar idioma",
    "Change Password" => "Cambiar la contraseña",
    "Check gray squares" => "Marcar los cuadrados grises",
    "Click %0 to continue." => "Hacer click %0 para continuar.",
    "Close" => "Cerrar",
    "Confirm password is missing" => "Falta la confirmación de la contraseña",
    "Current password" => "Contraseña actual",
    "Current password is missing" => "Falta indicar la contraseña actual",
    "Dance Management" => "Gestión de música dance",
    "Dance Songs Management" => "Gestión de música dance",
    "Fast" => "Rápido",
    "Good" => "Bueno",
    "Grey squares checks are wrong" => "Las casillas grises están mal marcadas",
    "Group" => "Grupo",
    "Groups" => "Grupos",
    "Keep connected" => "Mantenerse conectado",
    "Login" => "Identificación",
    "Logout-message" => "<p>%0 ha terminado.</p><p><b>¡Hasta pronto!</b></p>",
    "Long Dance" => "Dance (largo)",
    "Long Dance Songs" => "Dance (Temas largos)",
    "More" => "Más",
    "Name" => "Nombre",
    "New password" => "Nueva contraseña",
    "New password and confirm password do not match" => "La nueva contraseña y su confirmación no coinciden",
    "New password is missing" => "Falta indicar la nueva contraseña",
    "Ok" => "Normal",
    "Password" => "Contraseña",
    "Password is missing" => "Falta indicar la contraseña",
    "Password successfully changed" => "La contraseña se cambió correctamente",
    "Picture" => "Imagen",
    "Pictures Management" => "Gestión de imágenes",
    "Quality" => "Calidad",
    "Session is expired." => "La sesión ha expirado.",
    "Short Dance" => "Dance (corto)",
    "Short Dance Songs" => "Dance (Temas cortos)",
    "Sights" => "Vistas",
    "Slow" => "Lento",
    "Song" => "Tema",
    "Songs Management" => "Gestión de música",
    "Speed" => "Velocidad",
    "Stand By" => "Espera",
    "Times Management" => "Gestión de tiempos",
    "User" => "Usuario",
    "User name is missing" => "Falta indicar el nombre del usuario",
    "Wallpapers" => "Fondos de Pantalla",
    "Wallpapers with Music" => "Fondos con música",
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

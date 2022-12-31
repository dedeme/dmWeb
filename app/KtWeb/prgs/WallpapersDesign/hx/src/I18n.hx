// Generate by hxi18n. Don't modify

/// I18n management.
class I18n {

  static var enDic = [
    "Accept" => "Accept",
    "Activate" => "Activate",
    "Adjustment" => "Adjustment",
    "Background" => "Background",
    "Blur" => "Blur",
    "Blur (0 - 100)" => "Blur (0 - 100)",
    "Bottom" => "Bottom",
    "Cancel" => "Cancel",
    "Check gray squares" => "Check gray squares",
    "Click %0 to continue." => "Click %0 to continue.",
    "Close" => "Close",
    "Color" => "Color",
    "Cut" => "Cut",
    "Deactivate" => "Deactivate",
    "Grey squares checks are wrong" => "Grey squares checks are wrong",
    "Image '%0' can not be processed" => "Image '%0' can not be processed",
    "Keep connected" => "Keep connected",
    "Left" => "Left",
    "Light (0 - 100)" => "Light (0 - 100)",
    "Login" => "Login",
    "Password" => "Password",
    "Password is missing" => "Password is missing",
    "Percentage (0 - 100)" => "Percentage (0 - 100)",
    "Pixels from top / left" => "Pixels from top / left",
    "Pixels to sample" => "Pixels to sample",
    "Restore" => "Restore",
    "Right" => "Right",
    "Save in Group %0" => "Save in Group %0",
    "Save pictures in group %0?" => "Save pictures in group %0?",
    "Session is expired." => "Session is expired.",
    "Stretch" => "Stretch",
    "There are duplicated pictures" => "There are duplicated pictures",
    "There are no pictures to adjust" => "There are no pictures to adjust",
    "Top" => "Top",
    "Update" => "Update",
    "User" => "User",
    "User name is missing" => "User name is missing",
    "Wrong password" => "Wrong password",
    "here" => "here"
  ];

  static var esDic = [
    "Accept" => "Aceptar",
    "Activate" => "Activar",
    "Adjustment" => "Ajuste",
    "Background" => "Fondo",
    "Blur" => "Desenfoque",
    "Blur (0 - 100)" => "Desenfoque (0 - 100)",
    "Bottom" => "Abajo",
    "Cancel" => "Cancelar",
    "Check gray squares" => "Marcar las casillas grises",
    "Click %0 to continue." => "Click %0 para continuar.",
    "Close" => "Cerrar",
    "Color" => "Color",
    "Cut" => "Recorte",
    "Deactivate" => "Desactivar",
    "Grey squares checks are wrong" => "Las casillas grises está mal marcadas",
    "Image '%0' can not be processed" => "La imagen '%0' no puede ser procesada",
    "Keep connected" => "Mantenerse conectado",
    "Left" => "Izquierda",
    "Light (0 - 100)" => "Luz (0 - 100)",
    "Login" => "Identificación",
    "Password" => "Contraseña",
    "Password is missing" => "Falta la contraseña",
    "Percentage (0 - 100)" => "Porcentaje (0 - 100)",
    "Pixels from top / left" => "Pixles desde arriba / izquierda",
    "Pixels to sample" => "Pixels para muestra",
    "Restore" => "Restaurar",
    "Right" => "Derecha",
    "Save in Group %0" => "Guadar en el grupo %0",
    "Save pictures in group %0?" => "¿Guardar las imágenes en el grupo %0?",
    "Session is expired." => "Las sesión ha expirado.",
    "Stretch" => "Estirar",
    "There are duplicated pictures" => "Hay imágenes repetidas",
    "There are no pictures to adjust" => "No hay imágenes para ajustar",
    "Top" => "Arriba",
    "Update" => "Actualizar",
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

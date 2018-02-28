// Generate by jsi18n. Don't modify

goog.provide("I18n");

{
  let lang = {};

  const en = {
    "'%0' can not be read": "'%0' can not be read",
    "'%0' is an empty file": "'%0' is an empty file",
    "Accept": "Accept",
    "All the data will be replaced": "ATENTION!: All the data will be replaced.\nContinue?",
    "Backs": "Backups",
    "Backup file is missing": "Backup file is missin",
    "Backups": "Backups",
    "Cancel": "Cancel",
    "Change Language to %0": "Change Language to %0",
    "Change Password": "Change Password",
    "Check gray squares": "Check gray squares",
    "Clear": "Clear",
    "Clear trash?": "Clear trash?",
    "Click %0 to continue.": "Click %0 to continue.",
    "Confirm password": "Confirm password",
    "Confirm password is missing": "'Confirm password' is missing",
    "Current password": "Current password",
    "Current password is missing": "'Current password' is missing",
    "Download": "Download",
    "Fail trying to change password": "Fail trying to change password",
    "Fail unzipping backup": "Fail unzipping backup",
    "File is not a Selectividad backup": "File is not a Selectividad backup",
    "Grey squares checks are wrong": "Grey squares checks are wrong",
    "Help & Credits": "Help & Credits",
    "Login": "Login",
    "Logout-message": "<p>%0 has finished.</p><p><b>Good by!</b></p>",
    "Make backup": "Make backup",
    "New password": "New password",
    "New password and confirm password do not match": "New password and confirm password do not match",
    "New password is missing": "'New password' is missing",
    "Only one file can be selected": "Only one file can be selected",
    "Password": "Password",
    "Password is missing": "Password is missing",
    "Password successfully changed": "Password successfully changed",
    "Restore": "Restore",
    "Restore backup": "Restore backup",
    "Session is expired.": "Session is expired.",
    "Settings": "Settings",
    "Trash": "Trash",
    "User": "User",
    "User name is missing": "User name is missing",
    "Wrong password": "Wrong password",
    "here": "here"
  };

  const es = {
    "'%0' can not be read": "'%0' no puede ser leido",
    "'%0' is an empty file": "'%0' es un archivo vacío",
    "Accept": "Aceptar",
    "All the data will be replaced": "¡ATENCIÓN!. Todos los datos serán reemplazados.\n¿Continuar?",
    "Backs": "Copias",
    "Backup file is missing": "No se ha indicado el archivo de backup",
    "Backups": "Copias de seguridad",
    "Cancel": "Cancelar",
    "Change Language to %0": "Cambiar el idioma a %0",
    "Change Password": "Cambiar la contraseña",
    "Check gray squares": "Marcar los cuadrados grises",
    "Clear": "Eliminar",
    "Clear trash?": "¿Vaciar la papelera?",
    "Click %0 to continue.": "Hacer click %0 para continuar.",
    "Confirm password": "Confirmar la contraseña",
    "Confirm password is missing": "Falta la confirmación de la contraseña",
    "Current password": "Contraseña actual",
    "Current password is missing": "Falta indicar la contraseña actual",
    "Download": "Descarga",
    "Fail trying to change password": "Fallo intentando cambiar la contraseña",
    "Fail unzipping backup": "Fallo descomprimiendo la copia de seguridad",
    "File is not a Selectividad backup": "El archivo no es una copia de seguridad de Selectividad",
    "Grey squares checks are wrong": "Las casillas grises están mal marcadas",
    "Help & Credits": "Ayuda & créditos",
    "Login": "Identificación",
    "Logout-message": "<p>%0 ha terminado.</p><p><b>¡Hasta pronto!</b></p>",
    "Make backup": "Hacer copia",
    "New password": "Nueva contraseña",
    "New password and confirm password do not match": "La nueva contraseña y su confirmación no coinciden",
    "New password is missing": "Falta indicar la nueva contraseña",
    "Only one file can be selected": "Sólo se puede seleccionar un archivo",
    "Password": "Contraseña",
    "Password is missing": "Falta indicar la contraseña",
    "Password successfully changed": "La contraseña se cambió correctamente",
    "Restore": "Restauración",
    "Restore backup": "Restaurar copia",
    "Session is expired.": "La sesión ha expirado.",
    "Settings": "Configuración",
    "Trash": "Papelera",
    "User": "Usuario",
    "User name is missing": "Falta indicar el nombre del usuario",
    "Wrong password": "La contraseña es incorrecta",
    "here": "aquí"
  };

  I18n = class {
  /** @return {void} */
  static en () {
    lang = en;
  }

  /** @return {void} */
  static es () {
    lang = es;
  }

  /**
   * @private
   * @return {!Object<string, string>}
   */
  static lang () {
    return lang;
  }
}}

function _(key) {
  let v = I18n.lang()[key];
  if (v !== undefined) {
    return v;
  }
  return key;
}

function _args(key, ...args) {
  let bf = "";
  let v = _(key);
  let isCode = false;
  for (let i = 0; i < v.length; ++i) {
    let ch = v.charAt(i);
    if (isCode) {
      bf += ch === "0" ? args[0]
        : ch === "1" ? args[1]
        : ch === "2" ? args[2]
        : ch === "3" ? args[3]
        : ch === "4" ? args[4]
        : ch === "5" ? args[5]
        : ch === "6" ? args[6]
        : ch === "7" ? args[7]
        : ch === "8" ? args[8]
        : ch === "9" ? args[9]
        : ch === "%" ? "%"
        : "%" + ch;
      isCode = false;
    } else {
      if (ch === "%") {
        isCode = true;
      } else {
        bf += ch
      }
    }
  }
  return bf;
}

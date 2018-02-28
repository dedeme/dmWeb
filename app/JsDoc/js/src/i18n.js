// Generate by jsi18n. Don't modify

goog.provide("I18n");

{
  let lang = {};

  const en = {
    "Accept": "Accept",
    "Cancel": "Cancel",
    "Change Language to %0": "Cambiar el idioma a %0",
    "Change Password": "Change Password",
    "Check gray squares": "Check gray squares",
    "Click %0 to continue.": "Click %0 to continue.",
    "Confirm password": "Confirm password",
    "Confirm password is missing": "'Confirm password' is missing",
    "Current password": "Current password",
    "Current password is missing": "'Current password' is missing",
    "Delete %0?": "Delete %0?",
    "Fail trying to change password": "Fail trying to change password",
    "Grey squares checks are wrong": "Grey squares checks are wrong",
    "Keep connected": "Keep connected",
    "Libraries": "Libraries",
    "Login": "Login",
    "Name": "Name",
    "Name '%0' contains '%1'": "Name '%0' contains '%1'",
    "Name '%0' contains blanks": "Name '%0' contains blanks",
    "Name '%0' is repeated": "Name '%0' is repeated",
    "Name is missing": "Name is missing",
    "New password": "New password",
    "New password and confirm password do not match": "New password and confirm password do not match",
    "New password is missing": "'New password' is missing",
    "Password": "Password",
    "Password is missing": "Password is missing",
    "Password successfully changed": "Password successfully changed",
    "Path": "Path",
    "Path is missing": "Path is missing",
    "Session is expired.": "Session is expired.",
    "There are no libraries": "There are no libraries",
    "This source can not be selected, because it does not exist": "This source can not be selected, because it does not exist",
    "User": "User",
    "User name is missing": "User name is missing",
    "Wrong password": "Wrong password",
    "here": "here"
  };

  const es = {
    "Accept": "Aceptar",
    "Cancel": "Cancelar",
    "Change Language to %0": "Change Language to %0",
    "Change Password": "Cambiar la contraseña",
    "Check gray squares": "Marcar los cuadrados grises",
    "Click %0 to continue.": "Hacer click %0 para continuar.",
    "Confirm password": "Confirmar la contraseña",
    "Confirm password is missing": "Falta la confirmación de la contraseña",
    "Current password": "Contraseña actual",
    "Current password is missing": "Falta por indicar la contraseña actual",
    "Delete %0?": "¿Eliminar %0?",
    "Fail trying to change password": "Fallo intentando cambiar la contraseña",
    "Grey squares checks are wrong": "Las casillas grises están mal marcadas",
    "Keep connected": "Mantenerse conectado",
    "Libraries": "Librerías",
    "Login": "Identificación",
    "Name": "Nombre",
    "Name '%0' contains '%1'": "El nombre '%0' contiene '%1'",
    "Name '%0' contains blanks": "El nombre '%0' contiene espacios en blanco",
    "Name '%0' is repeated": "El nombre está repetido",
    "Name is missing": "Falta el nombre",
    "New password": "Nueva contraseña",
    "New password and confirm password do not match": "La nueva contraseña y su confirmación no coinciden.",
    "New password is missing": "Falta por indicar la nueva contraseña",
    "Password": "Contraseña",
    "Password is missing": "Falta por indicar la contraseña",
    "Password successfully changed": "La contraseña se cambió adecuadamente",
    "Path": "Ruta",
    "Path is missing": "Falta la ruta",
    "Session is expired.": "La sesión ha expirado.",
    "There are no libraries": "No hay librerías",
    "This source can not be selected, because it does not exist": "Esta fuente no puede ser seleccionada por que no existe",
    "User": "Usuario",
    "User name is missing": "Falta por indicar el nombre del usuario",
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

// Generate by jsi18n. Don't modify

/** I18n management. */

const en = {
  "Accept": "Accept",
  "Application exit?": "Application exit?",
  "Cancel": "Cancel",
  "Change Language to %0": "Cambiar el idioma a %0",
  "Change Password": "Change Password",
  "Check gray squares": "Check gray squares",
  "Click %0 to continue.": "Click %0 to continue.",
  "Confirm password is missing": "'Confirm password' is missing",
  "Current password": "Current password",
  "Current password is missing": "'Current password' is missing",
  "Delete '%0'?": "Delete '%0'?",
  "Grey squares checks are wrong": "Grey squares checks are wrong",
  "Help & Credits": "Help & Credits",
  "Keep connected": "Keep connected",
  "Libraries": "Libraries",
  "Library is empty": "Library is empty",
  "Library name is duplicated": "Library name is duplicated",
  "Login": "Login",
  "Logout-message": "<p>%0 has finished.</p><p><b>Good by!</b></p>",
  "Name": "Name",
  "Name contains '%0'": "Name contains '%0'",
  "Name is duplicate": "Name is duplicate",
  "Name is missing": "Name is missing",
  "New password": "New password",
  "New password and confirm password do not match": "New password and confirm password do not match",
  "New password is missing": "'New password' is missing",
  "Password": "Password",
  "Password Change": "Password Change",
  "Password is missing": "Password is missing",
  "Password successfully changed": "Password successfully changed",
  "Path": "Path",
  "Path is missing": "Path is missing",
  "Session is expired.": "Session is expired.",
  "There are no libraries": "There are no libraries",
  "User": "User",
  "User name is missing": "User name is missing",
  "Wrong password": "Wrong password",
  "here": "here"
};

const es = {
  "Accept": "Aceptar",
  "Application exit?": "¿Salir de la aplicación?",
  "Cancel": "Cancelar",
  "Change Language to %0": "Change Language to %0",
  "Change Password": "Cambiar la contraseña",
  "Check gray squares": "Marcar los cuadrados grises",
  "Click %0 to continue.": "Hacer click %0 para continuar.",
  "Confirm password is missing": "Falta la confirmación de la contraseña",
  "Current password": "Contraseña actual",
  "Current password is missing": "Falta por indicar la contraseña actual",
  "Delete '%0'?": "¿Eliminar '%0'?",
  "Grey squares checks are wrong": "Las casillas grises están mal marcadas",
  "Help & Credits": "Ayuda & créditos",
  "Keep connected": "Mantenerse conectado",
  "Libraries": "Librerías",
  "Library is empty": "La librería está vacía",
  "Library name is duplicated": "El nombre de la librería está repetido",
  "Login": "Identificación",
  "Logout-message": "<p>%0 ha terminado.</p><p><b>¡Hasta pronto!</b></p>",
  "Name": "Nombre",
  "Name contains '%0'": "El nombre contiene '%0'",
  "Name is duplicate": "El nombre está repetido",
  "Name is missing": "Falta el nombre",
  "New password": "Nueva contraseña",
  "New password and confirm password do not match": "La nueva contraseña y su confirmación no coinciden.",
  "New password is missing": "Falta por indicar la nueva contraseña",
  "Password": "Contraseña",
  "Password Change": "Cambiar contraseña",
  "Password is missing": "Falta por indicar la contraseña",
  "Password successfully changed": "La contraseña se cambió adecuadamente",
  "Path": "Ruta",
  "Path is missing": "Falta la ruta",
  "Session is expired.": "La sesión ha expirado.",
  "There are no libraries": "No hay librerías",
  "User": "Usuario",
  "User name is missing": "Falta por indicar el nombre del usuario",
  "Wrong password": "La contraseña es incorrecta",
  "here": "aquí"
};

let lang = es;

export class I18n {
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
   * @return {!Object<string, string>} Dictionary
   */
  static lang () {
    return lang;
  }
}


/**
 * @param {string} key Value
 * @return {string} Translation
 */
export function _ (key) {
  const v = I18n.lang()[key];
  if (v !== undefined) {
    return v;
  }
  return key;
}

/**
 * @param {string} key Template
 * @param {...string} args Values
 * @return {string} Translation
 */
export function _args (key, ...args) {
  let bf = "";
  const v = _(key);
  let isCode = false;
  for (let i = 0; i < v.length; ++i) {
    const ch = v.charAt(i);
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
    } else if (ch === "%") {
      isCode = true;
    } else {
      bf += ch;
    }
  }
  return bf;
}

// Generate by jsi18n. Don't modify

/** I18n management. */

let lang = {};

const en = {
  "Accept": "Accept",
  "Application exit?": "Application exit?",
  "Cancel": "Cancel",
  "Change Language to %0": "Change Language to %0",
  "Change Password": "Change Password",
  "Check gray squares": "Check gray squares",
  "Click %0 to continue.": "Click %0 to continue.",
  "Confirm password": "Confirm password",
  "Confirm password is missing": "Confirm password is missing",
  "Current password": "Current password",
  "Current password is missing": "Current password is missing",
  "Fail trying to change password": "Fail trying to change password",
  "Grey squares checks are wrong": "Grey squares checks are wrong",
  "Help & Credits": "Help & Credits",
  "Keep connected": "Keep connected",
  "Login": "Login",
  "Logout-message": "<p>%0 has finished.</p><p><b>Good by!</b></p>",
  "New password": "New password",
  "New password and confirm password do not match": "New password and confirm password do not match",
  "New password is missing": "New password is missing",
  "Password": "Password",
  "Password is missing": "Password is missing",
  "Password successfully changed": "Password successfully changed",
  "Session is expired.": "Session is expired.",
  "Settings": "Settings",
  "User": "User",
  "User name is missing": "User name is missing",
  "Wrong password": "Wrong password",
  "here": "here"
};

const es = {
  "Accept": "Aceptar",
  "Application exit?": "¿Terminar la aplicación?",
  "Cancel": "Cancelar",
  "Change Language to %0": "Cambiar el lenguaje a %0",
  "Change Password": "Cambiar la contraseña",
  "Check gray squares": "Marcar las casillas grises",
  "Click %0 to continue.": "Click %0 para continuar.",
  "Confirm password": "Confirmar la contraseña",
  "Confirm password is missing": "Falta la confirmación de la contraseña",
  "Current password": "Actual contraseña",
  "Current password is missing": "Falta la actual contraseña",
  "Fail trying to change password": "Fallo cambiando la contraseña",
  "Grey squares checks are wrong": "Las casillas grises están mal marcadas",
  "Help & Credits": "Ayuda & créditos",
  "Keep connected": "Mantenerse conectado",
  "Login": "Identificación",
  "Logout-message": "<p>%0 ha terminado.</p><p><b>¡Hasta pronto!</b></p>",
  "New password": "Nueva contraseña",
  "New password and confirm password do not match": "La nueva contraseña y su confirmación no coinciden",
  "New password is missing": "Falta la nueva contraseña",
  "Password": "Contraseña",
  "Password is missing": "Falta la contraseña",
  "Password successfully changed": "La contraseña se cambió correctamente",
  "Session is expired.": "La sesión ha terminado.",
  "Settings": "Configuración",
  "User": "Usuario",
  "User name is missing": "Falta el nombre del usuario",
  "Wrong password": "La contraseña es incorrecta",
  "here": "aquí"
};

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

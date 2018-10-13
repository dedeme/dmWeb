// Generate by jsi18n. Don't modify

/** I18n management. */

let lang = {};

const en = {
  "'%0' can not be read": "'%0' can not be read",
  "'%0' is an empty file": "'%0' is an empty file",
  "All the data will be replaced": "ATENTION!: All the data will be replaced.\nContinue?",
  "Annotations": "Annotations",
  "Auto-backup successfully restored": "Auto-backup successfully restored",
  "Automatic Backups": "Automatic Backups",
  "Backs": "Backups",
  "Backup file is missing": "Backup file is missin",
  "Backup successfully restored": "Backup successfully restored",
  "Backups": "Backups",
  "Change Language to %0": "Change Language to %0",
  "Clear": "Clear",
  "Clear trash?": "Clear trash?",
  "Click %0 to continue.": "Click %0 to continue.",
  "Download": "Download",
  "Fail unzipping backup": "Fail unzipping backup",
  "File is not a valid version backup": "File is not a valid version backup",
  "File is not an application backup": "File is not an application backup",
  "Help & Credits": "Help & Credits",
  "Home": "Home",
  "Logout-message": "<p>%0 has finished.</p><p><b>Good by!</b></p>",
  "Make backup": "Make backup",
  "Only one file can be selected": "Only one file can be selected",
  "Restore": "Restore",
  "Restore backup": "Restore backup",
  "Session is expired.": "Session is expired.",
  "Settings": "Settings",
  "Trash": "Trash",
  "Trash backup successfully restored": "Trash backup successfully restored",
  "here": "here"
};

const es = {
  "'%0' can not be read": "'%0' no puede ser leido",
  "'%0' is an empty file": "'%0' es un archivo vacío",
  "All the data will be replaced": "¡ATENCIÓN!. Todos los datos serán reemplazados.\n¿Continuar?",
  "Annotations": "Asientos",
  "Auto-backup successfully restored": "La copia de seguridad automática fue restaurada",
  "Automatic Backups": "Backups automáticos",
  "Backs": "Copias",
  "Backup file is missing": "No se ha indicado el archivo de backup",
  "Backup successfully restored": "La copia de seguridad fue restaurada",
  "Backups": "Copias de seguridad",
  "Change Language to %0": "Cambiar el idioma a %0",
  "Clear": "Eliminar",
  "Clear trash?": "¿Vaciar la papelera?",
  "Click %0 to continue.": "Hacer click %0 para continuar.",
  "Download": "Descarga",
  "Fail unzipping backup": "Fallo descomprimiendo la copia de seguridad",
  "File is not a valid version backup": "El archivo no contiene una válida versión de la copia de seguridad",
  "File is not an application backup": "El archivo no es una copia de seguridad de la aplicación",
  "Help & Credits": "Ayuda & créditos",
  "Home": "Inicio",
  "Logout-message": "<p>%0 ha terminado.</p><p><b>¡Hasta pronto!</b></p>",
  "Make backup": "Hacer copia",
  "Only one file can be selected": "Sólo se puede seleccionar un archivo",
  "Restore": "Restauración",
  "Restore backup": "Restaurar copia",
  "Session is expired.": "La sesión ha expirado.",
  "Settings": "Configuración",
  "Trash": "Papelera",
  "Trash backup successfully restored": "Copia de seguridad de la papelera restaurada",
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

// Copyright 24-Sep-2017 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

goog.provide("db_PyG");

db_PyG = class {
  /**
   * Fields:
   *    id
   *    description
   * PyG groups
   * @return {!Array<!Array<string>>}
   */
  static groups () {
    return [
      ["A", "RESULTADO DE EXPLOTACIÓN"],
      ["B", "RESULTADO FINANCIERO"],
      ["C", "RESULTADO ANTES DE IMPUESTOS"],
      ["D", "RESULTADO DEL EJERCICIO"]
    ];
  }

  /**
   * @param {string} id
   * @return {string}
   */
  static groupsGet (id) {
    const gs = db_PyG.groups();
    for (let i = 0; i < gs.length; ++i) {
      const g = gs[i];
      if (g[0] === id) {
        return g[1];
      }
    }
    throw ("Group " + id + " is missing");
  }

  /**
   * Fields:
   *    id
   *    description
   * P y G entries
   * @return {!Array<!Array<string>>}
   */
  static entries () {
    return [
      ["01", "Importe neto de la cifra de negocios"],
      ["02", "Variación de existencias"],
      ["03", "Trabajos realizados por la empresa para su activo"],
      ["04", "Aprovisionamientos"],
      ["05", "Otros ingresos de explotación"],
      ["06", "Gastos de personal"],
      ["07", "Otros gastos de explotación"],
      ["08", "Amortización del inmovilizado"],
      ["09", "Imputación de subvenciones "],
      ["10", "Excesos de provisiones"],
      ["11", "Deterioro y resultado por enajenaciones del inmovilizado"],
      ["12", "Otros resultados"],
      ["13", "Ingresos financieros"],
      ["14", "Gastos financieros"],
      ["15", "Variación de valor razonable en instrumentos financieros"],
      ["16", "Diferencias de cambio"],
      ["17", "Deterioro y resultado por enajenaciones de instr. financieros"],
      ["18", "Impuestos"]
    ];
  }

  /**
   * Test if 'key' is a valid P y G id.
   * @param {string} id
   * @return {boolean}
   */
  static validId(id) {
    return id.length === 3 &&
      id.charAt(0) === "P" && ((
          id.charAt(2) === "0" &&
          id.charAt(3) >= "1" &&
          id.charAt(3) <= "9"
        ) || (
          id.charAt(2) === "1" &&
          id.charAt(3) >= "0" &&
          id.charAt(3) <= "8"
        ));
  }

  /**
   * Returns the group of a P y G id.
   * @param {string} id
   * @return {string}
   */
  static groupId(id) {
    return id < "13" ? "A" : id < "18" ? "B" : "C";
  }
}


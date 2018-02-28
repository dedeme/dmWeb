// Copyright 24-Sep-2017 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

goog.provide("db_Balance");

db_Balance = class {
  /**
   * Fields:
   *    id
   *    description
   * Balance groups
   * @return {!Array<!Array<string>>}
   */
  static groups () {
    return [
      ["AA", "ACTIVO NO CORRIENTE"],
      ["AB", "ACTIVO CORRIENTE"],
      ["PA", "FONDOS PROPIOS"],
      ["PB", "PASIVO NO CORRIENTE"],
      ["PC", "PASIVO CORRIENTE"]
    ];
  }

  /**
   * @param {string} id
   * @return {string}
   */
  static groupsGet (id) {
    const gs = db_Balance.groups();
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
   * Balance entries
   * @return {!Array<!Array<string>>}
   */
  static entries () {
    return [
      ["AAI", "Inmovilizado intangible"],
      ["AAII", "Inmovilizado material"],
      ["AAIII", "Inversiones inmobiliarias"],
      ["AAV", "Inversiones financieras a largo plazo"],
      ["AAVI", "Activos por impuesto diferido"],
      ["AAVII", "Deudores comerciales no corrientes"],
      ["ABI", "Existencias"],
      ["ABII", "Deudores comerciales y otras cuentas a cobrar"],
      ["ABIV", "Inversiones financieras a corto plazo"],
      ["ABV", "Periodificaciones"],
      ["ABVI", "Efectivo y otros activos liquidos equivalentes"],

      ["PAI", "Capital"],
      ["PAIII", "Reservas"],
      ["PAVII", "Resultado del ejercicio"],

      ["PBI", "Provisiones a largo plazo"],
      ["PBII", "Deudas a largo plazo"],
      ["PBIV", "Pasivos por impuesto diferido"],
      ["PBV", "Periodificaciones a largo plazo"],
      ["PBVI", "Acreedores comerciales no corrientes"],
      ["PBVII", "Deuda con características especiales a largo plazo"],

      ["PCI", "Provisiones a corto plazo"],
      ["PCII", "Deudas a corto plazo"],
      ["PCIV", "Acreedores comerciales y otras cuentas a pagar"],
      ["PCV", "Periodificaciones a corto plazo"],
      ["PCVI", "Deuda con características especiales a corto plazo"]
    ];
  }

  /**
   * Test if 'key' is a valid Balance id.
   * @param {string} id
   * @return {boolean}
   */
  static validId(id) {
    return It.from(db_Balance.entries()).containsf(e => e[0] === id);
  }

  /**
   * Returns the group of a Balance id.
   * @param {string} id
   * @return {string}
   */
  static groupId(id) {
    return id.substring(0, 2);
  }
}


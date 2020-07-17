// Copyright 28-Jun-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

class Profits {
  /// Balance groups
  public static function groups (): Map<String, String> {
    return [
      "A" => "RESULTADO DE EXPLOTACIÓN",
      "B" => "RESULTADO FINANCIERO",
      "C" => "RESULTADO ANTES DE IMPUESTOS",
      "D" => "RESULTADO DEL EJERCICIO"
    ];
  }

  /// Balance entries
  public static function entries (): Map<String, String> {
    return [
      "01" => "Importe neto de la cifra de negocios",
      "02" => "Variación de existencias",
      "03" => "Trabajos realizados por la empresa para su activo",
      "04" => "Aprovisionamientos",
      "05" => "Otros ingresos de explotación",
      "06" => "Gastos de personal",
      "07" => "Otros gastos de explotación",
      "08" => "Amortización del inmovilizado",
      "09" => "Imputación de subvenciones ",
      "10" => "Excesos de provisiones",
      "11" => "Deterioro y resultado por enajenaciones del inmovilizado",
      "12" => "Otros resultados",
      "13" => "Ingresos financieros",
      "14" => "Gastos financieros",
      "15" => "Variación de valor razonable en instrumentos financieros",
      "16" => "Diferencias de cambio",
      "17" => "Deterioro y resultado por enajenaciones de instr. financieros",
      "18" => "Impuestos"
    ];
  }

  /// Test if 'id' is a valid Profits id.
  public static function isValid (id: String): Bool {
    return id.length == 3 &&
      id.charAt(0) == "P" && ((
          id.charAt(2) == "0" &&
          id.charAt(3) >= "1" &&
          id.charAt(3) <= "9"
        ) || (
          id.charAt(2) == "1" &&
          id.charAt(3) >= "0" &&
          id.charAt(3) <= "8"
        ));
  }

  /// Returns the group of a Profit entry.
  /// For example groupOf("12") returns "A".
  public static function groupOf (id: String): String {
    return id < "13" ? "A" : id < "18" ? "B" : "C";
  }

}

// Copyright 28-Jun-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

class Balance {
  /// Balance groups
  public static function groups (): Map<String, String> {
    return [
      "AA" => "ACTIVO NO CORRIENTE",
      "AB" => "ACTIVO CORRIENTE",
      "PA" => "FONDOS PROPIOS",
      "PB" => "PASIVO NO CORRIENTE",
      "PC" => "PASIVO CORRIENTE"
    ];
  }

  /// Balance entries
  public static function entries (): Map<String, String> {
    return [
      "AAI" => "Inmovilizado intangible",
      "AAII" => "Inmovilizado material",
      "AAIII" => "Inversiones inmobiliarias",
      "AAV" => "Inversiones financieras a largo plazo",
      "AAVI" => "Activos por impuesto diferido",
      "AAVII" => "Deudores comerciales no corrientes",
      "ABI" => "Existencias",
      "ABII" => "Deudores comerciales y otras cuentas a cobrar",
      "ABIV" => "Inversiones financieras a corto plazo",
      "ABV" => "Periodificaciones",
      "ABVI" => "Efectivo y otros activos liquidos equivalentes",

      "PAI" => "Capital",
      "PAIII" => "Reservas",
      "PAVII" => "Resultado del ejercicio",

      "PBI" => "Provisiones a largo plazo",
      "PBII" => "Deudas a largo plazo",
      "PBIV" => "Pasivos por impuesto diferido",
      "PBV" => "Periodificaciones a largo plazo",
      "PBVI" => "Acreedores comerciales no corrientes",
      "PBVII" => "Deuda con características especiales a largo plazo",

      "PCI" => "Provisiones a corto plazo",
      "PCII" => "Deudas a corto plazo",
      "PCIV" => "Acreedores comerciales y otras cuentas a pagar",
      "PCV" => "Periodificaciones a corto plazo",
      "PCVI" => "Deuda con características especiales a corto plazo"
    ];
  }

  /// Returns the group of a Balance entry.
  /// For example groupOf("PBII") returns "PB".
  public static function groupOf (id: String): String {
    return id.substring(0, 2);
  }

}

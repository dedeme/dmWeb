// Copyright 08-Dic-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import dm.Dec;
import I18n._;

/// Functions.
class Fns {
  public static function nFormat (value: Float, decs: Int): String {
    if (I18n.lang == "es") return Dec.toIso(value, decs);
    else return Dec.toEn(value, decs);
  }

  public static function nFormat2 (value: Float, decs: Int): String {
    final point = I18n.lang == "es" ? "," : ".";
    var r = nFormat(value, decs);
    while (true) {
      final lg1 = r.length - 1;
      final ch = r.charAt(lg1);
      if (ch == "0") {
        r = r.substring(0, lg1);
      } else {
        if (ch == point) r = r.substring(0, lg1);
        break;
      }
    }
    return r;
  }

  public static function dFormat (value: String): String {
    final y = value.substring(0, 4);
    final m = value.substring(4, 6);
    final d = value.substring(6);
    return I18n.lang == "es"
      ? d + "/" + m +"/" + y
      : m + "-" + d +"-" + y
    ;
  }
}

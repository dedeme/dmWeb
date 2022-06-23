// Copyright 03-Jan-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package cm;

import dm.Dt;

class Fns {
  /// Formats a integer with two digits.
  public static function format00 (n: Int): String {
    var r = "" + n;
    if (r.length < 2) r = "0" + r;
    return r;
  }

  /// Returns the last sunday (including today) in format YYYYMMDD
  public static function lastSunday (): String {
    var d = Date.now();
    d = Dt.add(d, -Dt.weekDay(d));
    return Dt.to(d);
  }
}

// Copyright 02-Sep-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Dt;
import dm.It;
import I18n._;

class Month {
  /// Returns the index (1-12) of 'm'. If validation fails, it returns the
  /// current month.
  public static function toIx (m: String): Int {
    final i = Std.parseInt(m);
    if (i == null) return Dt.month(Date.now());
    return i;
  }
  /// Returns the index of month 'n' (from 1 to 12) formated.
  public static function format (n: Int): String {
    return n < 10 ? "0" + n : "" + n;
  }
  /// Returns the name of month 'n' (from 1 to 12)
  public static function name (n: Int): String {
    return _("months").split(",")[n - 1];
  }
}

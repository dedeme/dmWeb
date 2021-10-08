// Copyright 17-Sep-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

class Year {
  /// Returns the current sportive year.
  public static function current (): String {
    final d = Date.now();
    var y = d.getFullYear();
    if (d.getMonth() < 7) --y;
    return "" + y;
  }

  /// Validate a year. If the validation fails, it returns the current year,
  /// otherwise return 'y'.
  public static function validate (y: String): String {
    final current = current();
    try {
      final yn = Std.parseInt(y);
      if (yn < 2000 || yn > 3000) {
        return current;
      }
      return y;
    } catch (e) {
      return current;
    }
  }

  /// Returns year (2010) formated (2010/11)
  public static function format (year: String) {
    final y1 = "" + (Std.parseInt(year) + 1);
    return year + "/" + y1.substring(y1.length - 2);
  }

}

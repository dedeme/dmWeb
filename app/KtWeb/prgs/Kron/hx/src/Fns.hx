// Copyright 21-Nov-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Global functions.

class Fns {
  public static function formatN00 (n: Int): String {
    final r = "" + n;
    return r.length < 2 ? "0" + r : r;
  }
}

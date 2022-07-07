// Copyright 01-Jul-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import dm.Dec;

/// Global functions.
class Fns {
  public static function paramFormatter (inc: Float): Float -> String {
    if (inc > 0.99) return (n) -> Dec.toIso(n, 0);
    if (inc > 0.099) return (n) -> Dec.toIso(n, 1);
    if (inc > 0.0099) return (n) -> Dec.toIso(n, 2);
    if (inc > 0.00099) return (n) -> Dec.toIso(n, 3);
    else return (n) -> Dec.toIso(n, 4);
  }
}

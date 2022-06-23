// Copyright 25-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

using StringTools;

import dm.Dec;

/// Global functions.
class Fns {
  /// Returns function to format parameters using 2 examples of successive
  /// values.
  public static function paramFormatter (
    n1: Float, n2: Float
  ): Float -> String {
    var r = (n) -> Dec.toIso(n, 4);
    if (!r(n1).endsWith("0") || !r(n2).endsWith("0")) return r;
    var r = (n) -> Dec.toIso(n, 3);
    if (!r(n1).endsWith("0") || !r(n2).endsWith("0")) return r;
    var r = (n) -> Dec.toIso(n, 2);
    if (!r(n1).endsWith("0") || !r(n2).endsWith("0")) return r;
    var r = (n) -> Dec.toIso(n, 1);
    if (!r(n1).endsWith("0") || !r(n2).endsWith("0")) return r;
    return (n) -> Dec.toIso(n, 0);
  }

  /// Returns a function to get a color from a value between 'max' and 'min'.
  public static function valueColor (
    max: Float, min: Float
  ): Float -> String {
    final df = max - min;
    return value -> {
      final red = Std.int((max - value) * 256 / df );
      final blue = Std.int((value - min) * 256 / df );
      return "rgb(" + red + ",80," + blue + ")";
    };
  }

}

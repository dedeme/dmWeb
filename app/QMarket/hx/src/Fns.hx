// Copyright 16-Oct-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import dm.Dec;

/// Global functions
class Fns {
  /// Format for numeric flea parameters.
  ///   v: Value
  ///   f: Format. Expected:
  ///     0 - Integer
  ///     4, 6 - Percentage.
  ///     Other - Normal number with 'other' decimals positions.
  public static function nformat (v: Float, f: Int): String {
    return f == 4 || f == 6
      ? Dec.toIso(v * 100, f - 2) + "%"
      : Dec.toIso(v, f)
    ;
  }

  /// Format for integers < 100
  ///   v: Value
  public static function format00(n: Int): String {
    if (n < 10) return "0" + Std.string(n);
    return Std.string(n);
  }

  /// Returns 'true' if 'q' > 0
  public static function isValidQuote (q: Float): Bool {
    return q > 0;
  }

  /// Return qs['i'] if it is > 0.
  /// Otherwise return the last previous valid quote or 'def' if every previous
  /// quote is <= 0.
  public static function validQuote (qs: Array<Float>, i: Int, def: Float) {
    var q = qs[i];
    if (isValidQuote(q)) return q;
    --i;
    while (i >= 0) {
      q = qs[i];
      if (isValidQuote(q)) return q;
      --i;
    }
    return def;
  }
}

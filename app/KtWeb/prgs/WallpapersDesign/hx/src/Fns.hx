// Copyright 15-Nov-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Functions.
class Fns {
  /// Returns a RGB color type "FFFFFF".
  public static function intToColor (c: Int): String {
    return StringTools.hex(c, 6);
  }

  /// 'c' is a RGB color type "ffffff".
  public static function colorToInt (c: String): Int {
    return Std.parseInt("0x" + c);
  }
}

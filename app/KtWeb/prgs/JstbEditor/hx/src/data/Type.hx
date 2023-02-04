// Copyright 23-Jan-2023 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// JSON type constants.

package data;

class Type {
  public static final NULL = 0;
  public static final BOOLEAN = 1;
  public static final NUMBER = 2;
  public static final STRING = 3;
  public static final ARRAY = 4;
  public static final MAP = 5;

  static final values = [
    "Null", "Boolean", "Number", "String", "Array", "Map"
  ];

  public static function toString(type: Int): String {
    return values[type];
  }
}

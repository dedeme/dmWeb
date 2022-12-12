// Copyright 19-Nov-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Functions.

import dm.Dec;

class Fns {
  /// Currency format.
  public static function cFmt (value: Float) {
    return "&nbsp;" +
      (I18n.lang == "es" ? Dec.toIso(value, 2) : Dec.toEn(value, 2));
  }
}

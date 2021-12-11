// Copyright 18-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.daily;

import dm.It;
import dm.Js;
import dm.Store;

/// Storage management of daily charts selection.
class Selection {
  static final key = "MultiMarket_daily_selection";

  /// Returns nick selected list.
  public static function nicks (): Array<String> {
    switch (Store.get(key)) {
      case Some(v): return Js.from(v).ra().map(e -> e.rs());
      case None: return [];
    }
  }

  /// Returns 'true' if 'Selection' contains 'nick'.
  public static function contains (nick: String): Bool {
    return It.from(nicks()).contains(nick);
  }

  /// Adds a company to 'Selection'.
  public static function add (nick: String): Void {
    final nks = nicks();
    if (!It.from(nks).contains(nick)) {
      nks.push(nick);
      Store.put(key, Js.wa(nks.map(e -> Js.ws(e))).to());
    }
  }

  /// Removes a company from 'Selection'.
  public static function remove (nick: String): Void {
    final nks = nicks();
    Store.put(key, Js.wa(nks.filter(e -> e != nick).map(e -> Js.ws(e))).to());
  }

}

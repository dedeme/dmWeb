// Copyright 18-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.daily;

import dm.It;
import dm.Js;
import dm.Tp;
import dm.Store;

/// Storage management of daily charts selection.
class Selection {
  static final key = "MultiMarket_daily_selection";

  /// Returns nick selected list.
  public static function nicks (): Array<Tp<String, Int>> {
    switch (Store.get(key)) {
      case Some(v): return Js.from(v).ra().map(e -> {
        final tpJs = e.ra();
        return new Tp(tpJs[0].rs(), tpJs[1].ri());
      });
      case None: return [];
    }
  }

  /// Returns 'true' if 'Selection' contains 'nick'.
  public static function contains (nick: String): Bool {
    return It.from(nicks()).indexf(tp -> tp.e1 == nick) != -1;
  }

  /// Returns the investor selected for 'nick'.
  public static function investor (nick: String): Int {
    return It.from(nicks()).reduce(-1, (r, tp) -> tp.e1 == nick ? tp.e2 : r);
  }

  /// Adds a company to 'Selection'.
  ///   nick: Company nick.
  ///   inv : Investor to sort charts or -1 to select that of more percentage.
  public static function add (nick: String, inv: Int): Void {
    final nks = nicks().filter(tp -> tp.e1 != nick);
    nks.push(new Tp(nick, inv));
    Store.put(
      key,
      Js.wa(nks.map(e -> Js.wa([Js.ws(e.e1), Js.wi(e.e2)]))).to()
    );
  }

  /// Removes a company from 'Selection'.
  public static function remove (nick: String): Void {
    final nks = nicks();
    Store.put(key, Js.wa(nks.filter(e -> e.e1 != nick)
      .map(e -> Js.wa([Js.ws(e.e1), Js.wi(e.e2)]))
    ).to());
  }

}

// Copyright 15-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data.flea;

import dm.Js;

/// Investor (Manager) ranking data.

class Irank {
  public var date(default, null): String;
  public var invs(default, null): Array<Investor>;

  function new (date: String, invs: Array<Investor>) {
    this.date = date;
    this.invs = invs;
  }

  public static function fromJs (js: Js): Irank {
    final a = js.ra();
    return new Irank(
      a[0].rs(),
      a[1].ra().map(e -> Investor.fromJs(e))
    );
  }
}


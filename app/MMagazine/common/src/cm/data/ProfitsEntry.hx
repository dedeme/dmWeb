// Copyright 06-Jan-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package cm.data;

import dm.Js;

/// Profits data of all the investors.
class ProfitsEntry {
  public final date: String;
  public final profits: Array<Float>;

  public function new (date: String, profits: Array<Float>) {
    this.date = date;
    this.profits = profits;
  }

  public function toJs (): Js {
    return Js.wa([
      Js.ws(date),
      Js.wa(profits.map(a -> Js.wf(a)))
    ]);
  }

  public static function fromJs (js: Js): ProfitsEntry {
    final a = js.ra();
    return new ProfitsEntry(
      a[0].rs(),
      a[1].ra().map(a -> a.rf())
    );
  }
}

// Copyright 14-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data.acc;

import dm.Js;
import dm.It;
import dm.Dec;

/// Sorted diary book from before to after.
class Diary {
  public final nextId: Int;
  public final anns: Array<Ann>;

  function new (nextId: Int, anns: Array<Ann>) {
    this.nextId = nextId;
    this.anns = anns;
  }

  /// Returns 'initial capital' + 'stock values' at the beginning of the year.
  public function initialAssets (): Float {
    return It.from(anns)
      .takeWhile(a ->
          a.date.substring(4) == "0101"
            ? switch (a.op) {
                case OpIn(_) | OpSt(_, _, _): true;
                default: false;
              }
            : false
        )
      .reduce(0.0, (s, a) -> switch (a.op) {
          case OpIn(amount): s + amount;
          case OpSt(_, stocks, price): s + Dec.round(stocks * price, 2);
          default: s;
        })
    ;
  }

  public static function fromJs (js: Js): Diary {
    final a = js.ra();
    final anns = a[1].ra().map(e -> Ann.fromJs(e));
    anns.sort((a1, a2) -> return a1.date > a2.date ? 1 : -1);
    return new Diary(
      a[0].ri(),
      anns
    );
  }
}

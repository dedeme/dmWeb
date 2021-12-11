// Copyright 29-Oct-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data.model;

import dm.Js;

// Last evaluation data.
class Eval {
  public final buys: Int;
  public final sales: Int;
  public final assets: Float;
  public final profitsAvg: Float;
  public final value: Float;

  function new (
    buys: Int, sales: Int, assets: Float, profitsAvg: Float, value: Float
  ) {
    this.buys = buys;
    this.sales = sales;
    this.assets = assets;
    this.profitsAvg = profitsAvg;
    this.value = value;
  }

  public function toJs (): Js {
    return Js.wa([
      Js.wi(buys),
      Js.wi(sales),
      Js.wf(assets),
      Js.wf(profitsAvg),
      Js.wf(value)
    ]);
  }

  public static function fromJs(js: Js): Eval {
    final a = js.ra();
    return new Eval(
      a[0].ri(),
      a[1].ri(),
      a[2].rf(),
      a[3].rf(),
      a[4].rf()
    );
  }
}

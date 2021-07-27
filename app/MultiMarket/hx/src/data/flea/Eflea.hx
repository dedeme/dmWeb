// Copyright 14-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data.flea;

import dm.Js;

/// Evaluated flea record.
class Eflea {
  public final flea: Flea;
  public final buys: Int;
  public final sales: Int;
  public final assets: Float;
  public final profitsAvg: Float;
  public final profitsVa: Float;
  public final eval: Float;
  public final historicSales: Float;
  public final historicEval: Float;

  function new (
    flea: Flea, buys: Int, sales: Int,
    assets: Float, profitsAvg: Float, profitsVa: Float, eval: Float,
    historicSales: Float, historicEval: Float
  ) {
    this.flea = flea;
    this.buys = buys;
    this.sales = sales;
    this.assets = assets;
    this.profitsAvg = profitsAvg;
    this.profitsVa = profitsVa;
    this.eval = eval;
    this.historicSales = historicSales;
    this.historicEval = historicEval;
  }

  public function toJs(): Js {
    return Js.wa([
      flea.toJs(),
      Js.wi(buys),
      Js.wi(sales),
      Js.wf(assets),
      Js.wf(profitsAvg),
      Js.wf(profitsVa),
      Js.wf(eval),
      Js.wf(historicSales),
      Js.wf(historicEval)
    ]);
  }

  public static function fromJs(js: Js): Eflea {
    final a = js.ra();
    return new Eflea(
      Flea.fromJs(a[0]),
      a[1].ri(),
      a[2].ri(),
      a[3].rf(),
      a[4].rf(),
      a[5].rf(),
      a[6].rf(),
      a[7].rf(),
      a[8].rf()
    );
  }
}


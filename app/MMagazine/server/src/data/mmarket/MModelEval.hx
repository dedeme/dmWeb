// Copyright 19-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data.mmarket;

import dm.Js;

/// Model evaluation data of MMarket
class MModelEval {
  public final params: Array<Float>;
  public final weeks: Int;
  public final hvalue: Float;
  public final hsales: Float;
  public final value: Float;
  public final sales: Float;

  function new (
    params: Array<Float>, weeks: Int,
    hvalue: Float, hsales: Float, value: Float, sales: Float
  ) {
    this.params = params;
    this.weeks = weeks;
    this.hvalue = hvalue;
    this.hsales = hsales;
    this.value = value;
    this.sales = sales;
  }

  public static function fromJs (js: Js): MModelEval {
    final a = js.ra();
    return new MModelEval(
      a[0].ra().map(e -> e.rf()),
      a[1].ri(),
      a[2].rf(),
      a[3].rf(),
      a[4].rf(),
      a[5].rf()
    );
  }
}

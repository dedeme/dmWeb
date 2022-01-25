// Copyright 19-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data.eval;

import dm.Js;

/// Result data.
class Result {
  public final assets: Float;
  public final profits: Float;
  public final sales: Float;

  function new (assets: Float, profits: Float, sales: Float) {
    this.assets = assets;
    this.profits = profits;
    this.sales = sales;
  }

  public function eval (): Float {
    final a = assets > Cts.maxAssetsRatio ? Cts.maxAssetsRatio : assets;
    final p = profits > Cts.maxProfitsAvgRatio ? Cts.maxProfitsAvgRatio : profits;
    return
      ( a * Cts.assetsRatio / Cts.maxAssetsRatio +
        (1 + p) * Cts.profitsAvgRatio / (1 + Cts.maxProfitsAvgRatio)
      ) / 2
    ;
  }

  public static function fromJs (js: Js): Result {
    final a = js.ra();
    return new Result(
      a[0].rf(),
      a[1].rf(),
      a[2].rf()
    );
  }
}

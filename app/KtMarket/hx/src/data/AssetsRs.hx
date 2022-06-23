// Copyright 16-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Js;

/// Results received in Orders page test.
class AssetsRs {
  public var assets(default, null): Float;
  public var buys(default, null): Int;
  public var sells(default, null): Int;

  function new (assets: Float, buys: Int, sells: Int) {
    this.assets = assets;
    this.buys = buys;
    this.sells = sells;
  }

  public static function fromJs(js: Js): AssetsRs {
    final a = js.ra();
    return new AssetsRs(
      a[0].rf(),
      a[1].ri(),
      a[2].ri()
    );
  }

}

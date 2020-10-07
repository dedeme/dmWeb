// Copyright 14-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data.flea;

import dm.Js;

/// Evaluated flea record.
class Eflea {
  public var flea(default, null): Flea;
  public var buys(default, null): Int;
  public var sells(default, null): Int;
  public var assets(default, null): Float;
  public var profitsAvg(default, null): Float;
  public var profitsVa(default, null): Float;
  public var ev(default, null): Float;

  function new (
    flea: Flea, buys: Int, sells: Int,
    assets: Float, profitsAvg: Float, profitsVa: Float, ev: Float
  ) {
    this.flea = flea;
    this.buys = buys;
    this.sells = sells;
    this.assets = assets;
    this.profitsAvg = profitsAvg;
    this.profitsVa = profitsVa;
    this.ev = ev;
  }

  public function toJs(): Js {
    return Js.wa([
      flea.toJs(),
      Js.wi(buys),
      Js.wi(sells),
      Js.wf(assets),
      Js.wf(profitsAvg),
      Js.wf(profitsVa),
      Js.wf(ev)
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
      a[6].rf()
    );
  }
}


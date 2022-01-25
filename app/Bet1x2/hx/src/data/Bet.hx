// Copyright 14-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Js;

/// Bet data.
class Bet {
  public final r1: Float;
  public final rx: Float;
  public final r2: Float;

  function new (r1: Float, rx: Float, r2: Float) {
    this.r1 = r1;
    this.rx = rx;
    this.r2 = r2;
  }

  public static function fromJs (js: Js): Bet {
    final a = js.ra();
    return new Bet (
      a[0].rf(),
      a[1].rf(),
      a[2].rf()
    );
  }
}

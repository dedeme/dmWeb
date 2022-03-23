// Copyright 29-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data.simulation;

import dm.Js;

/// Simulation profits result
class SimProfits {
  /// Total profits.
  public final total: Float;
  /// Cash profits.
  public final cash: Float;
  /// Reference (risk) profits.
  public final ref: Float;

  function new (total: Float, cash: Float, ref: Float) {
    this.total = total;
    this.cash = cash;
    this.ref = ref;
  }

  public static function fromJs (js: Js): SimProfits {
    final a = js.ra();
    return new SimProfits(
      a[0].rf(),
      a[1].rf(),
      a[2].rf()
    );
  }
}

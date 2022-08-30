// Copyright 21-Aug-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Js;

/// Daily results.
class DayRs {
  /// Current savings.
  public final savings: Float;
  /// Current assets.
  public final assets: Float;

  function new (savings: Float, assets: Float) {
    this.savings = savings;
    this.assets = assets;
  }

  public static function fromJs (js: Js): DayRs {
    final a = js.ra();
    return new DayRs(
      a[0].rf(),
      a[1].rf()
    );
  }
}

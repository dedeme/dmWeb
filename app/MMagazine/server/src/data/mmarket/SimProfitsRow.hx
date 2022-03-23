// Copyright 29-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data.mmarket;

import dm.Js;

class SimProfitsRow {
  /// Parameters evaluated.
  public final params: Array<Float>;
  /// Weeks weight to calculate historic values.
  public final weeks: Int;
  /// Historic values.
  public final hprofits: SimProfits;
  /// Last values.
  public final profits: SimProfits;

  function new (
    params: Array<Float>, weeks: Int, hprofits: SimProfits, profits: SimProfits
  ) {
    this.params = params;
    this.weeks = weeks;
    this.hprofits = hprofits;
    this.profits = profits;
  }

  public static function fromJs (js: Js): SimProfitsRow {
    final a = js.ra();
    return new SimProfitsRow(
      a[0].ra().map(e -> e.rf()),
      a[1].ri(),
      SimProfits.fromJs(a[2]),
      SimProfits.fromJs(a[3])
    );
  }
}

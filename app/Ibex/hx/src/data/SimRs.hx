// Copyright 21-Aug-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Js;

/// Simulation results data
class SimRs {
  /// Daily results.
  public final results: Array<DayRs>;
  /// Orders.
  public final orders: Array<Order>;

  function new (results: Array<DayRs>, orders: Array<Order>) {
    this.results = results;
    this.orders = orders;
  }

  public static function fromJs (js: Js): SimRs {
    final a = js.ra();
    return new SimRs(
      a[0].ra().map(DayRs.fromJs),
      a[1].ra().map(Order.fromJs)
    );
  }
}

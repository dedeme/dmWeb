// Copyright 22-Jul-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Js;

/// Indexes chart entry data.
class IxsChartEntry {
  public final hour: Int;
  public final invs: Array<Float>;
  public final ixs: Array<Float>;

  function new (hour: Int, invs: Array<Float>, ixs: Array<Float>) {
    this.hour = hour;
    this.invs = invs;
    this.ixs = ixs;
  }

  public static function fromJs (js: Js): IxsChartEntry {
    final a = js.ra();
    return new IxsChartEntry(
      a[0].ri(),
      a[1].ra().map(e -> e.rf()),
      a[2].ra().map(e -> e.rf())
    );
  }
}


// Copyright 01-Sep-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

/// Company (& IBEX) chart data.
class CoChart {
  public final nick: String;
  /// Ponderation assets.
  public final pond: Float;
  /// Proportional assets.
  public final prop: Float;
  /// Sale order number.
  public final sales: Int;
  public final dates: Array<String>;
  public final closes: Array<Float>;
  /// References.
  public final refs: Array<Float>;

  public function new (
    nick: String, pond: Float, prop: Float, sales: Int,
    dates: Array<String>, closes: Array<Float>, refs: Array<Float>
  ) {
    this.nick = nick;
    this.pond = pond;
    this.prop = prop;
    this.sales = sales;
    this.dates = dates;
    this.closes = closes;
    this.refs = refs;
  }
}

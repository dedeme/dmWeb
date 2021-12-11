// Copyright 13-Oct-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data.flea;

import dm.Js;

/// Evaluation of a paremeter for a flea of one parameter.

class JumpResult {
  public final param: Float;
  public final eval: Float;
  public final sales: Float;
  public final investors: Array<Investor>;

  function new (
    param: Float, eval: Float, sales: Float, investors: Array<Investor>
  ) {
    this.param = param;
    this.eval = eval;
    this.sales = sales;
    this.investors = investors;
  }

  public static function fromJs (js: Js): JumpResult {
    final a = js.ra();
    return new JumpResult(
      a[0].rf(),
      a[1].rf(),
      a[2].rf(),
      a[3].ra().map(e -> Investor.fromJs(e))
    );
  }
}

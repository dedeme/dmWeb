// Copyright 31-Jul-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.It;
import dm.Dec;
import I18n._;
import I18n._args;

/// Deduction page.
class Deduction {
  final wg: Domo;

  function new (wg: Domo) {
    this.wg = wg;
  }

  // View ----------------------------------------------------------------------

  public function show (): Void {
    wg
      .removeAll()
      .add(Q("P")
        .text("here"))
    ;
  }

  // Static --------------------------------------------------------------------

  public static function mk (wg: Domo): Void {
    Cts.client.send([
      "module" => Js.ws("Main"),
      "source" => Js.ws("Deduction"),
      "rq" => Js.ws("idata"),
    ], rp -> {
      new Deduction(wg).show();
    });
  }
}

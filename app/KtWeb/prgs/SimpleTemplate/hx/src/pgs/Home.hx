// Copyright 01-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import I18n._;

/// Home page.
class Home {
  final wg: Domo;
  final text: String;

  function new (wg: Domo, text: String) {
    this.wg = wg;
    this.text = text;
  }

  // View ----------------------------------------------------------------------

  public function show (): Void {
    wg
      .removeAll()
      .add(Q("div")
        .text(text))
    ;
  }

  // Control -------------------------------------------------------------------

  // Static --------------------------------------------------------------------

  public static function mk (wg: Domo): Void {
    Global.client.send([
      "prg" => Js.ws("SimpleTemplate"),
      "source" => Js.ws("Home"),
      "rq" => Js.ws("idata")
    ], rp -> {
      final text = rp["text"].rs();
      new Home(wg, text).show();
    });
  }
}

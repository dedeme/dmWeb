// Copyright 21-Aug-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.Log;
import I18n._;

/// Home page.
class Home {
  final wg: Domo;
  final logDiv = Q("div");

  public function new (wg: Domo) {
    this.wg = wg;
  }

  // View ----------------------------------------------------------------------

  public function show () {
    function load (fn: Array<LogRow> -> Void) {
      Cts.client.send([
        "source" => Js.ws("Home"),
        "rq" => Js.ws("getLog")
      ], rp -> {
        final log = rp["log"].ra();
        fn(log.map(js -> LogRow.fromJs(js)));
      });
    }

    function reset (fn: () -> Void) {
      Ui.alert(_("Reset must be made from server!") +
        "\n \nIbex resetLog\n ");
      fn();
    }
    Log.mk(logDiv, load, reset, _, true);

    final body = Q("div");

    wg
      .removeAll()
      .add(Q("table")
        .klass("main")
        .add(Q("tr")
          .add(Q("td")
            .add(body)
            .add(logDiv))))
    ;

  }

  // Static --------------------------------------------------------------------

  public static function mk (wg: Domo) {
    new Home(wg).show();
  }

}

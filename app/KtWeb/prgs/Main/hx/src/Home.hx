// Copyright 01-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.It;
import dm.Dec;
import dm.Log;
import I18n._;
import I18n._args;

/// Home page.
class Home {
  final wg: Domo;

  function new (wg: Domo) {
    this.wg = wg;
  }

  // View ----------------------------------------------------------------------

  public function show (): Void {
    final logDiv = Q("div");

    function load (fn: Array<LogRow> -> Void) {
      Global.client.send([
        "prg" => Js.ws("Main"),
        "source" => Js.ws("Home"),
        "rq" => Js.ws("getLog")
      ], rp -> {
        final log = rp["log"].ra();
        fn(log.map(js -> LogRow.fromJs(js)));
      });
    }

    function reset (fn: () -> Void) {
      Global.client.ssend([
        "prg" => Js.ws("Main"),
        "source" => Js.ws("Home"),
        "rq" => Js.ws("resetLog")
      ], rp -> {
        fn();
      });
    }

    Log.mk(logDiv, load, reset, _, true);

    wg
      .removeAll()
      .add(logDiv)
    ;
  }

  // Static --------------------------------------------------------------------

  public static function mk (wg: Domo): Void {
    new Home(wg).show();
  }
}

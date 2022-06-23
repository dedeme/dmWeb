// Copyright 16-Oct-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.home;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.It;
import dm.Dec;
import dm.Log;
import I18n._;

/// Home page.
class Home {
  final wg: Domo;

  function new (wg: Domo) {
    this.wg = wg;
    view();
  }

  // view ----------------------------------------------------------------------

  function view (): Void {
    final logDiv = Q("div");

    function load (fn: Array<LogRow> -> Void) {
      Cts.client.send([
        "module" => Js.ws("home"),
        "rq" => Js.ws("log")
      ], rp -> {
        final log = rp["log"].ra();
        fn(log.map(js -> LogRow.fromJs(js)));
      });
    }

    function reset (fn: () -> Void) {
      Cts.client.send([
        "module" => Js.ws("home"),
        "rq" => Js.ws("resetLog")
      ], rp -> {
        fn();
      });
    }
    Log.mk(logDiv, load, reset, _, false);

    wg
      .removeAll()
      .add(logDiv)
    ;
  }

  // static --------------------------------------------------------------------

  /// Constructor.
  ///   wg: Container.
  public static function mk (wg: Domo): Void {
    new Home(wg);
  }
}

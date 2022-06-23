// Copyright 01-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.main;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.It;
import dm.Dec;
import dm.Log;
import cm.data.RankingEntry;
import I18n._;
import I18n._args;

/// Home page.
class Home {
  final wg: Domo;
  final ok: Bool;

  function new (wg: Domo, ok: Bool) {
    this.wg = wg;
    this.ok = ok;
  }

  // View ----------------------------------------------------------------------

  public function show (): Void {
    final logDiv = Q("div");

    function load (fn: Array<LogRow> -> Void) {
      Cts.client.send([
        "module" => Js.ws("Main"),
        "source" => Js.ws("Home"),
        "rq" => Js.ws("getLog")
      ], rp -> {
        final log = rp["log"].ra();
        fn(log.map(js -> LogRow.fromJs(js)));
      });
    }

    function reset (fn: () -> Void) {
      Ui.alert(_("Reset must be made from server!") +
        "\n \nMMagazine resetLog\n ");
      fn();
    }
    Log.mk(logDiv, load, reset, _, true);

    wg
      .removeAll()
      .adds(It.range(2).map(i -> Q("div").klass("separator")).to())
      .add(Q("table")
        .klass("frame")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td")
            .add(Q("span")
              .text(ok ? "Ok" : "Fail")))))
      .add(logDiv)
    ;
  }

  // Static --------------------------------------------------------------------

  public static function mk (wg: Domo): Void {
    Cts.client.send([
      "module" => Js.ws("Main"),
      "source" => Js.ws("Home"),
      "rq" => Js.ws("idata"),
    ], rp -> {
      final ok = rp["ok"].rb();
      new Home(wg, ok).show();
    });
  }
}

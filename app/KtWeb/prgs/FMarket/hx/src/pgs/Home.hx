// Copyright 02-Dic-2022 ºDeme
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
  static final activities = [
    "stopped" => _("Stopped"),
    "Generating" => _("Generating"),
    "Evaluating" => _("Evaluating"),
    "Selecting" => _("Selecting"),
    "Saving" => _("Saving")
  ];
  final wg: Domo;
  final activity: String;

  final logDiv = Q("div");

  function new (wg: Domo, activity: String) {
    this.wg = wg;
    this.activity = activity;
  }

  // View ----------------------------------------------------------------------

  public function show (): Void {
    function load (fn: Array<LogRow> -> Void) {
      Global.client.send([
        "prg" => Js.ws("FMarket"),
        "source" => Js.ws("Home"),
        "rq" => Js.ws("getLog")
      ], rp -> {
        final log = rp["log"].ra();
        fn(log.map(js -> LogRow.fromJs(js)));
      });
    }

    function reset (fn: () -> Void) {
      Global.client.ssend([
        "prg" => Js.ws("FMarket"),
        "source" => Js.ws("Home"),
        "rq" => Js.ws("resetLog")
      ], rp -> {
        fn();
      });
    }

    Log.mk(logDiv, load, reset, _, true, 100);

    wg
      .removeAll()
      .add(Q("div")
        .klass("head")
        .text(_("Cycle")))
      .add(Q("table")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td")
            .klass("frame")
            .style("text-align: center")
            .text(activities[activity])))
        .add(Q("tr")
          .add(Q("td")
            .style("width:100px;text-align:center")
            .add(activity != "stopped"
                ? Q("span")
                  .text("* * *")
                : Ui.link(activate)
                  .klass("link")
                  .text(_("Start")
              )))))
      .add(Q("hr"))
      .add(logDiv)
    ;
  }

  // Control -------------------------------------------------------------------

  function activate () {
    logDiv
      .removeAll()
      .add(Q("table")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td")
            .add(Ui.img("wait.gif")))))
    ;
    Global.client.ssend([
      "prg" => Js.ws("FMarket"),
      "source" => Js.ws("Home"),
      "rq" => Js.ws("activate")
    ], rp -> {
      mk(wg);
    });
  }

  // Static --------------------------------------------------------------------

  public static function mk (wg: Domo): Void {
    Global.client.send([
      "prg" => Js.ws("FMarket"),
      "source" => Js.ws("Home"),
      "rq" => Js.ws("idata")
    ], rp -> {
      final activity = rp["activity"].rs();
      new Home(wg, activity).show();
    });
  }
}

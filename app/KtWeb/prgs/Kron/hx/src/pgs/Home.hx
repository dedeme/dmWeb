// Copyright 21-Nov-2022 ºDeme
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
  final isActive: Bool;

  final logDiv = Q("div");

  function new (wg: Domo, isActive: Bool) {
    this.wg = wg;
    this.isActive = isActive;
  }

  // View ----------------------------------------------------------------------

  public function show (): Void {
    function load (fn: Array<LogRow> -> Void) {
      Global.client.send([
        "prg" => Js.ws("Kron"),
        "source" => Js.ws("Home"),
        "rq" => Js.ws("getLog")
      ], rp -> {
        final log = rp["log"].ra();
        fn(log.map(js -> LogRow.fromJs(js)));
      });
    }

    function reset (fn: () -> Void) {
      Global.client.ssend([
        "prg" => Js.ws("Kron"),
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
        .text(_("Server")))
      .add(Q("table")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td")
            .klass("frame")
            .att("colspan", 3)
            .style("text-align: center")
            .text(isActive ? _("Active") : _("Stopped"))))
        .add(Q("tr")
          .add(Q("td")
            .style("width:100px;text-align:center")
            .add(Ui.link(activate)
              .klass("link")
              .text(isActive ? _("Reactivate"): _("Activate"))))
          .add(Q("td")
            .text(" "))
          .add(Q("td")
            .style("width:100px;text-align:center")
            .add(isActive
              ? Q("span")
                .add(Ui.link(stop)
                  .klass("link")
                  .text(_("Stop")))
              : Q("span")
                .text("* * *")))))
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
      "prg" => Js.ws("Kron"),
      "source" => Js.ws("Home"),
      "rq" => Js.ws("activate")
    ], rp -> {
      mk(wg);
    });
  }

  function stop () {
    Global.client.ssend([
      "prg" => Js.ws("Kron"),
      "source" => Js.ws("Home"),
      "rq" => Js.ws("stop")
    ], rp -> {
      mk(wg);
    });
  }

  // Static --------------------------------------------------------------------

  public static function mk (wg: Domo): Void {
    Global.client.send([
      "prg" => Js.ws("Kron"),
      "source" => Js.ws("Home"),
      "rq" => Js.ws("idata")
    ], rp -> {
      final isActive = rp["isActive"].rb();
      new Home(wg, isActive).show();
    });
  }
}

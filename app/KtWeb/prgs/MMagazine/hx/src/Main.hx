// Copyright 07-Nov-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.Menu;
import I18n._;
import I18n._args;
import wgs.Dmenu;

/// Applicatoin entry.
class Main {
  final wg: Domo;

  function new (wg: Domo, lang: String) {
    this.wg = wg;
    if (lang == "es") I18n.es();
    else I18n.en();
  }

  // View ----------------------------------------------------------------------

  public function show (): Void {
    final search = js.Browser.location.search;
    final lcPath = search == ""
      ? []
      : search.substring(1).split("&")
    ;
    if (lcPath.length == 0) lcPath.push("home");

    final target =
      switch (lcPath[0]) {
        case "models" | "ktmarket" | "mmarket" | "acc":
          lcPath[0];
        default:
          "home";
      }
    lcPath.shift();

    final menuDiv = Q("div");
    final bodyDiv = Q("div");
    final menu = new Dmenu(menuDiv, target);

    switch (target) {
      case "home":
        pgs.main.Home.mk(bodyDiv);
      case "ktmarket":
        pgs.Ktmarket.mk(bodyDiv, menu, lcPath);
      case "mmarket":
        pgs.Mmarket.mk(bodyDiv, menu, lcPath);
      case "acc":
        pgs.main.Home.mk(bodyDiv);
      default:
        pgs.main.Home.mk(bodyDiv);
    }

    wg
      .removeAll()
      .add(menuDiv)
      .add(bodyDiv)
    ;

  }

  // Control -------------------------------------------------------------------

  // Static --------------------------------------------------------------------

  static function mk (wg: Domo, fn: () -> Void): Void {
    Global.client.connect(ok -> {
      if (ok) {
        Global.client.send([
          "prg" => Js.ws("Main"), // Call to KtWeb:Main
          "source" => Js.ws("Main"),
          "rq" => Js.ws("lang")
        ], rp -> {
          final lang = rp["lang"].rs();
          new Main(wg, lang).show();
          fn();
        });
      } else {
        new Authentication(wg, Cts.appName, () -> mk(wg, fn));
        fn();
      }
    });
  }

  /// Application entry.
  static public function main (): Void {
    var wg = Q("div");
    mk(wg, () -> {
      Q("@body")
        .removeAll()
        .add(wg)
        .add(Cts.foot)
      ;
    });
  }

}

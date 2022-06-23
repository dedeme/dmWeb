// Copyright 31-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

using StringTools;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.Menu;
import dm.Store;
import I18n._;
import I18n._args;
import pgs.main.Authentication;
import wgs.Dmenu;

/// Applicatoin entry.
class Main {
  final wg: Domo;
  final lang: String;

  function new (wg: Domo) {
    this.wg = wg;
    lang = switch (Store.get(Cts.langKey)) {
      case Some(l): l;
      case None: "es";
    }
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
        case "models" | "ktmarket" | "mmarket" |
             "acc" | "settings":
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
      case "settings":
        new pgs.main.Settings(bodyDiv, lang).show();
      default:
        pgs.main.Home.mk(bodyDiv);
    }

    wg
      .removeAll()
      .add(menuDiv)
      .add(bodyDiv)
    ;
  }

  // Static --------------------------------------------------------------------

  static function mk (wg: Domo, fn: () -> Void): Void {
    Cts.client.connect(ok -> {
      if (ok) {
        new Main(wg).show();
        fn();
      } else {
        new Authentication(wg, cm.Cts.appName, () -> mk(wg, fn));
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
        .add(Ui.upTop("up"))
      ;
      var fc = Q("#autofocus");
      if (fc.e != null) fc.e.focus();
    });
  }

}

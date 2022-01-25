// Copyright 16-Oct-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import data.Cts;
import wgs.Msg;
import wgs.Dmenu;
import pgs.settings.SettingsMenu;
import pgs.home.Home;
import pgs.models.Models;
import pgs.ranking.Ranking;
import pgs.acc.Acc;
import pgs.performance.Performance;
import pgs.daily.Daily;


/// Application entry.
class Main {
  // View ----------------------------------------------------------------------

  // Main constructor.
  //   wg: Container.
  //   fn: Function to call at end of function.
  static function mk (wg: Domo, fn: () -> Void) {
    Cts.client.connect(ok -> {
      if (ok) {
        Cts.client.send([
          "module" => Js.ws("main"),
          "rq" => Js.ws("lang")
        ], rp -> {
          final lang = rp["lang"].rs();
          if (lang == "es") I18n.es();
          else I18n.en();

          final search = js.Browser.location.search;
          final lcPath = search == ""
            ? []
            : search.substring(1).split("&")
          ;
          if (lcPath.length == 0) lcPath.push("home");

          final target =
            switch (lcPath[0]) {
              case "models" | "performance" | "ranking" |
                   "daily" | "acc" | "settings":
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
              Home.mk(bodyDiv);
            case "models":
              Models.mk(bodyDiv, menu, lcPath);
            case "performance":
              Performance.mk(bodyDiv);
            case "ranking":
              Ranking.mk(bodyDiv);
            case "acc":
              new Acc(bodyDiv, menu, lcPath);
            case "daily":
              Daily.mk(bodyDiv, menu, lcPath, SIGNAL, SELL, false);
            case "settings":
              SettingsMenu.mk(bodyDiv, menu, lcPath, lang);
            default:
              Home.mk(bodyDiv);
          }

          wg
            .removeAll()
            .add(menuDiv)
            .add(bodyDiv)
          ;
          fn();
        });
      } else {
        Authentication.mk(wg, Cts.appName, () -> mk(wg, fn));
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
        .add(Msg.wg)
      ;

      var fc = Q("#autofocus");
      if (fc.e != null) fc.e.focus();
    });
  }
}

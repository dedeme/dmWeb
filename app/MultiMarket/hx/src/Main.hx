// Copyright 15-Aug-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import data.Cts;
import wgs.Msg;
import wgs.Dmenu;
import pgs.settings.Settings;
import pgs.home.Home;
import pgs.fleas.Fleas;
import pgs.performance.Performance;
import pgs.ranking.Ranking;
import pgs.daily.Daily;
import pgs.acc.Acc;

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
              case "fleas" | "performance" | "ranking" |
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
            case "fleas":
              Fleas.mk(bodyDiv, menu, lcPath);
            case "performance":
              Performance.mk(bodyDiv);
            case "ranking":
              Ranking.mk(bodyDiv);
            case "acc":
              new Acc(bodyDiv, menu, lcPath);
            case "daily":
              Daily.mk(bodyDiv, menu, lcPath, SIGNAL, false);
            case "settings":
              Settings.mk(bodyDiv, menu, lcPath, lang);
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

// Copyright 25-Jun-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.Menu;
import I18n._;
import I18n._args;
import data.All;

/// Applicatoin entry.
class Main {

  // Control -------------------------------------------------------------------

  static function close () {
    if (!Ui.confirm(_("Application exit?"))) {
      return;
    }
    Cts.client.send([
      "source" => Js.ws("Main"),
      "rq" => Js.ws("close"),
      "sessionId" => Js.ws(Cts.client.sessionId())
    ], rp -> {
      final wg = Q("div");
      MsgPage.mk(wg , Cts.app, _args(_("Logout-message"), [Cts.app]), false);
      Q("@body")
        .removeAll()
        .add(wg)
        .add(Cts.foot)
      ;
    });
  }

  // View ----------------------------------------------------------------------

  static function mkMenu (isLastYear, year, sel: String): Menu {
    var myear = Menu.tlink(year, year);
    if (!isLastYear) myear.wg.setStyle("color", "#800000");
    var lopts = [
      myear,
      Menu.separator(),
      Menu.tlink("diary", _("Diary")),
      Menu.separator(),
      Menu.tlink("cash", _("Cash")),
      Menu.separator(),
      Menu.tlink("accs", _("Accs")),
      Menu.separator(),
      Menu.tlink("summaries", _("Summaries")),
      Menu.separator(),
      Menu.tlink("plan", _("Plan")),
    ];
    var ropts = [
      Menu.tlink("settings", _("Settings")),
      Menu.separator(),
      Menu.close(close)
    ];
    return new Menu(lopts, ropts, sel);
  }

  // Static --------------------------------------------------------------------

  static function mk (wg: Domo, fn: () -> Void) {
    Cts.client.connect(ok -> {
      if (ok) {
        All.request(data -> {
          if (data.conf.language == "es") I18n.es();
          else I18n.en();

          final url = Ui.url();
          final page = url.exists("0") ? url.get("0") : "cash";

          final menuDiv = Q("div")
            .add(mkMenu(
              data.conf.isLastYear(), data.conf.currentYear, page
            ).wg);
          final body = Q("div");

          switch (page) {
          case "diary":
            final acc = url.exists("1") ? url.get("1") : "";
            final ix = url.exists("2") ? url.get("2"): "-";
            Diary.mk(body, data, acc, ix);
          case "cash":
            final acc = url.exists("1") ? url.get("1") : "";
            final ix = url.exists("2") ? url.get("2"): "-";
            Cash.mk(body, data, acc, ix);
          case "accs":
            final acc = url.exists("1") ? url.get("1") : "";
            final ix = url.exists("2") ? url.get("2"): "-";
            Accs.mk(body, data, acc, ix);
          case "summaries":
            final type = url.exists("1") ? url.get("1") : "";
            final deep = url.exists("2") ? url.get("2"): "";
            Summary.mk(body, data, type, deep);
          case "plan":
            final acc = url.exists("1") ? url.get("1") : "";
            PlanPage.mk(body, data, acc);
          case "settings":
            SettingsPage.mk(body, data);
          default:
            if (page == data.conf.currentYear) {
              Year.mk(body, data);
            } else {
              final acc = url.exists("1") ? url.get("1") : "";
              final ix = url.exists("2") ? url.get("2"): "-";
              Cash.mk(body, data, acc, ix);
            }
          }

          wg.removeAll()
            .add(menuDiv)
            .add(body)
          ;
          fn();
        });
      } else {
        Authentication.mk(wg, Cts.app, () -> mk(wg, fn));
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

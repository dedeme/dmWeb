// Copyright 14-Sep-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.Dt;
import dm.Menu;
import I18n._;
import I18n._args;
import pgs.Authentication;
import pgs.SettingsPage;
import pgs.Year;

import data.Matchday;

/// Applicatoin entry.
class Main {

  // Static --------------------------------------------------------------------

  static function mkMenu (
    isCurrentYear: Bool, year: String, page: String
  ): Menu {
    function close () {
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

    final selLink = page + "&" + year;
    var myear = Menu.tlink("year&" + year, year);
    if (!isCurrentYear) myear.wg.setStyle("color", "#800000");
    var lopts = [
      myear,
      Menu.separator(),
      Menu.tlink("budget&" + year, _("Budget")),
      Menu.separator(),
      Menu.tlink("plan&" + year, _("Plan")),
    ];
    var ropts = [
      Menu.tlink("settings&" + year, _("Settings")),
      Menu.separator(),
      Menu.close(close)
    ];
    return new Menu(lopts, ropts, selLink);
  }

  static function mk (wg: Domo, fn: () -> Void) {
    Cts.client.connect(ok -> {
      if (ok) {
        if (Storage.getLang() == "en") I18n.en()
        else I18n.es();
        final url = Ui.url();

        final now = Date.now();

        final urlPage = url["0"];
        final page = urlPage == null ? "settings" : urlPage;

        final currentYear = Std.string(now.getFullYear());
        final urlSelectedYear = url["1"];
        final selectedYear = urlSelectedYear == null
          ? currentYear
          : Cts.validateYear(urlSelectedYear)
        ;

        final body = Q("div");
        switch (page) {
          case "year": Year.mk(body, selectedYear);
          case "settings": SettingsPage.mk(body);
          default: SettingsPage.mk(body);
        }

        wg
          .removeAll()
          .add(mkMenu(currentYear == selectedYear, selectedYear, page).wg)
          .add(body)
        ;
        fn();
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
    });
  }
}

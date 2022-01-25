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
import pgs.Authentication;
import pgs.MsgPg;

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
    var pg = "home";
    final url = Ui.url();
    if (url.exists("0")) {
      final p = url.get("0").trim();
      switch (p) {
        case "home", "rankings", "settings", "profits",
             "percents", "models": pg = p;
      }
    }

    final lopts = [
      Menu.tlink("home", _("Home")),
      Menu.separator2(),
      Menu.tlink("rankings", _("Rankings")),
      Menu.separator(),
      Menu.tlink("profits", _("Profits")),
      Menu.separator(),
      Menu.tlink("percents", _("Percentages")),
      Menu.separator2(),
      Menu.tlink("models", _("Models")),
      Menu.separator(),
      Menu.tlink("hotmaps", _("Hot Maps"))
    ];
    final ropts = [
      Menu.tlink("settings", _("Settings")),
      Menu.separator(),
      Menu.close(close)
    ];
    final menu = new Menu(lopts, ropts, pg);

    final body = Q("div");
    switch (pg) {
      case "home": pgs.Home.mk(body);
      case "rankings": pgs.RankingsPg.mk(body);
      case "profits": pgs.ProfitsPg.mk(body);
      case "percents": pgs.PercentsPg.mk(body);
      case "models": pgs.ModelsPg.mk(body);
      case "settings": new pgs.Settings(body, lang).show();
      default: pgs.Home.mk(body);
    }

    wg
      .removeAll()
      .add(menu.wg)
      .add(body)
    ;
  }

  // Control -------------------------------------------------------------------

  function close (): Void {
    if (!Ui.confirm(_("Application exit?"))) {
      return;
    }
    Cts.client.send([
      "source" => Js.ws("Main"),
      "rq" => Js.ws("close"),
      "sessionId" => Js.ws(Cts.client.sessionId())
    ], rp -> {
      new MsgPg(
        Q("@body"), _args(_("Logout-message"), [cm.Cts.appName]), false
      ).show();
    });
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

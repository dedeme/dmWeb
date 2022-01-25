// Copyright 12-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.Opt;
import dm.Menu;
import data.Conf;
import pgs.MsgPg;
import pgs.Authentication;
import pgs.Settings;
import pgs.Home;
import pgs.Periodic;
import pgs.Fix;
import pgs.Init;
import pgs.Notes;
import I18n._;
import I18n._args;

/// Applicatoin entry.
class Main {
  final wg: Domo;

  function new (wg: Domo) {
    this.wg = wg;
  }

  // View ----------------------------------------------------------------------

  function show () {
    final url = Ui.url();
    final page = url.exists("0") ? url.get("0") : "home";

    final menu = new Menu(
      [ Menu.tlink("home", _("Home")),
        Menu.separator2(),
        Menu.tlink("periodic", _("Periodic")),
        Menu.separator(),
        Menu.tlink("fix", _("Fix")),
        Menu.separator(),
        Menu.tlink("init", _("Initialization")),
        Menu.separator2(),
        Menu.tlink("notes", _("Notes")),
      ],
      [ Menu.tlink("settings", _("Settings")),
        Menu.separator(),
        Menu.close(close)
      ],
      page
    );

    final body = Q("div");

    switch (page) {
      case "home": Home.mk(body);
      case "periodic": Periodic.mk(body);
      case "fix": Fix.mk(body);
      case "init": Init.mk(body);
      case "notes": Notes.mk(body);
      case "settings": Settings.mk(body);
      default: Home.mk(body);
    }

    wg
      .removeAll()
      .add(menu.wg)
      .add(body)
    ;
  }

  // Control -------------------------------------------------------------------

  function close () {
    if (!Ui.confirm(_("Application exit?"))) {
      return;
    }
    Cts.client.send([
      "source" => Js.ws("Main"),
      "rq" => Js.ws("close"),
      "sessionId" => Js.ws(Cts.client.sessionId())
    ], rp -> {
      new MsgPg(wg , _args(_("Logout-message"), [Cts.appName]), false).show();
    });
  }

  // Static --------------------------------------------------------------------

  static function mk (wg: Domo, fn: Void -> Void) {
    Cts.client.connect(ok -> {
      if (ok) {
        Cts.client.send([
          "source" => Js.ws("Main"),
          "rq" => Js.ws("idata")
        ], rp -> {
          final conf = Conf.fromJs(rp["conf"]);
          if (conf.lang == "es") I18n.es();
          else I18n.en();
          new Main(wg).show();
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
        .add(Ui.upTop("up"))
      ;

      var fc = Q("#autofocus");
      if (fc.e != null) fc.e.focus();
    });
  }
}

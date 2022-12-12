// Copyright 20-Nov-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.Menu;
import I18n._;
import I18n._args;
import pgs.Home;
import pgs.Periodic;
import pgs.Fix;
import pgs.Manual;

import data.Ann;

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
    final url = Ui.url();
    final page = url.exists("0") ? url.get("0") : "home";

    final menu = new Menu(
      [ Menu.tlink("home", _("Home")),
        Menu.separator2(),
        Menu.tlink("periodic", _("Periodic")),
        Menu.separator(),
        Menu.tlink("fix", _("Fix")),
        Menu.separator(),
        Menu.tlink("manual", _("Manual"))
      ],
      [ ],
      page
    );

    final body = Q("div");

    switch (page) {
      case "home": Home.mk(body);
      case "periodic": Periodic.mk(body);
      case "fix": Fix.mk(body);
      case "manual": Manual.mk(body);
      default: Home.mk(body);
    }

    wg
      .removeAll()
      .add(menu.wg)
      .add(body)
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
        .add(Ui.upTop("up"))
      ;
      var fc = Q("#autofocus");
      if (fc.e != null) fc.e.focus();
    });
  }

}

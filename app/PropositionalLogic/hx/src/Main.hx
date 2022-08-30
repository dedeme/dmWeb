// Copyright 31-Jul-2022 ºDeme
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
import data.Book;

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
    final target = switch (search) {
      case "?deduction" | "?theorems" | "?settings" : search.substring(1);
      default: "deduction";
    }

    final menuDiv = Q("div");
    final bodyDiv = Q("div");

    final lopts = [
      Menu.tlink("deduction", _("Deduction")),
      Menu.separator(),
      Menu.tlink("theorems", _("Theorems"))
    ];

    final ropts = [
      Menu.tlink("settings", _("Settings")),
      Menu.separator(),
      Menu.close(() -> close())
    ];

    menuDiv
      .removeAll()
      .add(new Menu(lopts, ropts, target).wg)
    ;

    switch (target) {
      case "deduction":
        pgs.Deduction.mk(bodyDiv);
      case "theorems":
        pgs.TheoremsPg.mk(bodyDiv);
      case "settings":
        new pgs.Settings(bodyDiv, lang).show();
      default:
        throw(target + ": Unknown target");
    }

    wg
      .removeAll()
      .add(menuDiv)
      .add(Q("table")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td")
            .add(bodyDiv))))
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
      new MsgPg(
        Q("@body"), _args(_("Logout-message"), [Cts.appName]), false
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

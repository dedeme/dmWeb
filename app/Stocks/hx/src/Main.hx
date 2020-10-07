// Copyright 22-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.Store;
import dm.Menu;
import I18n._;
import I18n._args;

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

  static function mkMenu (sel: String): Menu {
    var lopts = [
      Menu.tlink("annotations", _("Annotations")),
      Menu.separator(),
      Menu.tlink("treasury", _("Treasury")),
      Menu.separator(),
      Menu.tlink("forms", _("Forms")),
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
        final lang = switch (Store.get(Cts.langKey)) {
          case Some(l): l;
          case None: "es";
        }
        if (lang == "es") I18n.es();
        else I18n.en();

        final url = Ui.url();
        final page = url.exists("0") ? url.get("0") : "annotations";

        final menuDiv = Q("div")
          .add(mkMenu(page).wg);
        final body = Q("div");

        switch (page) {
        case "treasury":
          Treasury.mk(body);
        case "forms":
          Form.mk(body);
        case "settings":
          SettingsPage.mk(body);
        default:
          Annotations.mk(body);
        }

        wg.removeAll()
          .add(menuDiv)
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

      var fc = Q("#autofocus");
      if (fc.e != null) fc.e.focus();
    });
  }
}

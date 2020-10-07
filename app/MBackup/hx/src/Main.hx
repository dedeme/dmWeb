// Copyright 04-Oct-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.Menu;
import I18n._;

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
          "page" => Js.ws("main"),
          "rq" => Js.ws("lang")
        ], rp -> {
          final lang = rp["lang"].rs();
          if (lang == "es") I18n.es();
          else I18n.en();

          final bodyDiv = Q("div");
          final lopts = [
            Menu.toption("summary", _("Summary"), () -> summary(bodyDiv)),
            Menu.separator(),
            Menu.toption("dirs", _("Directories"), () -> dirs(bodyDiv))
          ];
          final ropts = [
            Menu.toption(
              "settings", _("Settings"), () -> settings(bodyDiv, lang)
            ),
            Menu.separator(),
            Menu.close(() -> close(wg))
          ];
          final menu = new Menu(lopts, ropts, "summary");

          wg
            .removeAll()
            .add(menu.wg)
            .add(bodyDiv)
          ;
          summary(bodyDiv);
          fn();
        });
      } else {
        Authentication.mk(wg, Cts.appName, () -> mk(wg, fn));
        fn();
      }
    });
  }

  // Control -------------------------------------------------------------------

  static function summary(wg: Domo): Void {
    new Summary(wg);
  }

  static function dirs(wg: Domo): Void {
    wg
      .removeAll()
      .add(Q("p").text("Dirs"))
    ;
  }

  static function settings(wg: Domo, lang: String): Void {
    Settings.mk(wg, lang);
  }

  static function close(wg: Domo): Void {
    if (Ui.confirm(_("Close application?"))) {
      Cts.client.send([
        "page" => Js.ws("main"),
        "rq" => Js.ws("bye"),
        "sessionId" => Js.ws(Cts.client.sessionId())
      ], rp -> {
        MsgPage.mk(wg, Cts.appName, _("Application has been closed."));
      });
    }
  }

  // Main ----------------------------------------------------------------------

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

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
          "rq" => Js.ws("conf")
        ], rp -> {
          final lang = rp["lang"].rs();
          if (lang == "es") I18n.es();
          else I18n.en();
          var option = rp["menu"].rs();
          if (option == "") option = "summary";

          final bodyDiv = Q("div");
          final lopts = [
            Menu.toption("summary", _("Summary"), () -> summary(wg, fn)),
            Menu.separator(),
            Menu.toption("dirs", _("Directories"), () -> dirs(wg, fn))
          ];
          final ropts = [
            Menu.toption(
              "settings", _("Settings"), () -> settings(wg, fn)
            ),
            Menu.separator(),
            Menu.close(() -> close(wg))
          ];
          final menu = new Menu(lopts, ropts, option);

          wg
            .removeAll()
            .add(menu.wg)
            .add(bodyDiv)
          ;
          switch (option) {
            case "dirs"    : Directories.mk(bodyDiv);
            case "settings": Settings.mk(bodyDiv, lang);
            default        : new Summary(bodyDiv);
          }
          fn();
        });
      } else {
        Authentication.mk(wg, Cts.appName, () -> mk(wg, fn));
        fn();
      }
    });
  }

  // Control -------------------------------------------------------------------

  static function summary(wg: Domo, fn: () -> Void): Void {
    Cts.client.send([
      "page" => Js.ws("main"),
      "rq" => Js.ws("menuOption"),
      "option" => Js.ws("summary")
    ], rp -> {
      mk(wg, fn);
    });
  }

  static function dirs(wg: Domo, fn: () -> Void): Void {
    Cts.client.send([
      "page" => Js.ws("main"),
      "rq" => Js.ws("menuOption"),
      "option" => Js.ws("dirs")
    ], rp -> {
      mk(wg, fn);
    });
  }

  static function settings(wg: Domo, fn: () -> Void): Void {
    Cts.client.send([
      "page" => Js.ws("main"),
      "rq" => Js.ws("menuOption"),
      "option" => Js.ws("settings")
    ], rp -> {
      mk(wg, fn);
    });
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
        .add(Cts.measureWg)
      ;

      var fc = Q("#autofocus");
      if (fc.e != null) fc.e.focus();
    });
  }
}

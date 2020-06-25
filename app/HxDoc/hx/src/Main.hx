// Copyright 18-Jun-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.It;
import dm.Menu;
import I18n._;
import I18n._args;
import data.Settings;

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
      MsgPage.mk(
        Q("@body"), Cts.app, _args(_("Logout-message"), [Cts.app]), false
      );
    });
  }

  // View ----------------------------------------------------------------------

  static function mkMenu (sett: Settings, selection: String): Menu {
    var lopts = [Menu.ilink("@", "asterisk")];
    for (e in sett.paths) {
      if (e.isShown) {
        lopts.push(Menu.separator());
        lopts.push(Menu.tlink(e.lib, e.lib));
      }
    }

    var ropts = [Menu.close(close)];
    return new Menu(lopts, ropts, selection);
  }

  // Static --------------------------------------------------------------------

  static function mk (wg: Domo, fn: () -> Void) {
    Cts.client.connect(ok -> {
      if (ok) {
        Cts.client.send([
          "source" => Js.ws("Main"),
          "rq" => Js.ws("read")
        ], rp -> {
          final sett = Settings.fromJs(rp["data"]);

          final url = Ui.url();
          var parts = ["@"];
          if (url.exists("0")) {
            final v = url.get("0");
            if (v != "@") parts = v.split("@");
          }

          var selection = null;
          final v = parts[0];
          if (It.from(sett.paths).some(p -> p.lib == v)) selection = v;
          if (selection == null) {
            selection = "@";
            parts = ["@"];
          }

          final menuDiv = Q("div")
            .add(mkMenu(sett, selection).wg);
          final body = Q("div");
          if (parts.length == 1) {
            if (parts[0] == "@") {
              Paths.mk(
                body, sett,
                (sett) -> menuDiv.removeAll().add(mkMenu(sett, "").wg)
              );
            } else {
              Index.mk(body, sett, parts[0]);
            }
          } else if (parts.length == 2) {
            if (url.exists("1")) {
              Code.mk(
                body, sett, parts[0], parts[1], Std.parseInt(url.get("1"))
              );
            } else {
              Module.mk(body, sett, parts[0], parts[1]);
            }
          }

          wg.removeAll()
            .add(menuDiv)
            .add(body)
          ;
        });
      } else {
        Authentication.mk(wg, Cts.app, () -> mk(wg, fn));
      }
      fn();
    });
  }

  /// Application entry.
  static public function main (): Void {
    var wg = Q("div");
    mk(wg, () -> {
      Q("@body")
        .removeAll()
        .add(wg)
        .add(Ui.upTop("up"))
      ;
      var fc = Q("#autofocus");
      if (fc.e != null) fc.e.focus();
    });
  }
}

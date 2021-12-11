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
import cm.data.Conf;
import cm.data.Paths;
import cm.data.Lpath;
import pgs.MsgPage;
import pgs.Authentication;
import pgs.PathsPg;
import pgs.Index;
import pgs.Module;
import pgs.Code;

/// Applicatoin entry.
class Main {

  // Static --------------------------------------------------------------------

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

  static function mk (wg: Domo, fn: () -> Void) {
    Cts.client.connect(ok -> {
      if (ok) {
        Cts.client.send([
          "source" => Js.ws("Main"),
          "rq" => Js.ws("read")
        ], rp -> {
          final conf = Conf.fromJs(rp["conf"]);
          final paths = Paths.fromJs(rp["paths"]);

          final url = Ui.url();
          var parts = ["@"];
          if (url.exists("0")) {
            final v = url.get("0");
            if (v != "@") parts = v.split("@");
          }

          var selection = null;
          final v = parts[0];
          if (It.from(paths.list).some(p -> p.lib == v)) selection = v;
          if (selection == null) {
            selection = "@";
            parts = ["@"];
          }

          final menuDiv = Q("div")
            .add(mkMenu(paths, selection).wg);
          final body = Q("div");
          if (parts.length == 1) {
            if (parts[0] == "@") {
              new PathsPg(
                body, conf, paths,
                paths -> menuDiv.removeAll().add(mkMenu(paths, selection).wg)
              );
            } else {
              Index.mk(body, paths, parts[0]);
            }
          } else if (parts.length == 2) {
            if (url.exists("1")) {
              Code.mk(
                body, paths, parts[0], parts[1],
                Std.parseInt(url.get("1"))
              );
            } else {
              Module.mk(body, paths, parts[0], parts[1]);
            }
          }

          wg.removeAll()
            .add(menuDiv)
            .add(body)
          ;
          fn();
        });
      } else {
        new Authentication(wg, cm.Cts.app, () -> mk(wg, fn));
        fn();
      }
    });
  }

  // View ----------------------------------------------------------------------

  static function mkMenu (paths: Paths, selection: String): Menu {
    var lopts = [Menu.ilink("@", "asterisk")];
    for (e in paths.list) {
      if (e.isShown) {
        lopts.push(Menu.separator());
        lopts.push(Menu.tlink(e.lib, e.lib));
      }
    }

    var ropts = [Menu.close(close)];
    return new Menu(lopts, ropts, selection);
  }

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
        Q("@body"), cm.Cts.app, _args(_("Logout-message"), [cm.Cts.app]), false
      );
    });
  }

}

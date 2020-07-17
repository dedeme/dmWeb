// Copyright 25-Jun-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.Menu;
import data.Conf;
import I18n._;
import I18n._args;
import pages.News;
import pages.Weights;
import pages.Sources;
import pages.Authors;
import pages.Words;
import pages.Settings;

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
      Menu.tlink("news", _("News")),
      Menu.separator2(),
      Menu.tlink("weights", _("Weights")),
      Menu.separator2(),
      Menu.tlink("sources", _("Sources")),
      Menu.separator(),
      Menu.tlink("authors", _("Authors")),
      Menu.separator(),
      Menu.tlink("words", _("Words"))
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
        Cts.client.send([
          "source" => Js.ws("Main"),
          "rq" => Js.ws("read")
        ], rp -> {
          final conf = Conf.fromJs(rp["conf"]);
          if (conf.lang == "es") I18n.es();
          else I18n.en();

          final url = Ui.url();
          final page = url.exists("0") ? url.get("0") : "local";

          final menuDiv = Q("div")
            .add(mkMenu(page).wg);
          final body = Q("div");

          switch (page) {
          case "local":
            News.mk(body);
          case "weights":
            Weights.mk(body);
          case "sources":
            Sources.mk(body);
          case "authors":
            Authors.mk(body);
          case "words":
            Words.mk(body);
          case "settings":
            Settings.mk(body, conf);
          default:
            News.mk(body);
          }

          wg.removeAll()
            .add(menuDiv)
            .add(body)
          ;
          fn();
        });
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

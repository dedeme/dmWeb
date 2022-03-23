// Copyright 03-Mar-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.Opt;
import dm.Menu;
import pgs.MsgPg;
import pgs.Authentication;
import pgs.PathsPg;
import pgs.IndexPg;
import pgs.ModulePg;
import pgs.CodePg;
import data.Conf;
import data.Dpath;
import I18n._;
import I18n._args;

/// Applicatoin entry.
class Main {
  final wg: Domo;
  final cf: Conf;
  final paths: Array<Dpath>;

  final pack: String;
  final pkPath: Option<String>;
  final anchor: Option<String>;

  function new (wg: Domo, cf: Conf, paths: Array<Dpath>) {
    this.wg = wg;
    this.cf = cf;
    this.paths = paths;

    if (cf.lang == "es") I18n.es() else I18n.en();

    final url = Ui.url();
    final page = url.exists("0") ? url["0"] : cf.path;

    final parts = page.split("@");
    pack = page == "@" ? page : parts[0];
    pkPath = parts.length > 1 && pack != "@" ? Some(parts[1]) : None;

    anchor = url.exists("1") ? Some(url["1"]) : None;

    savePath();
  }

  // View ----------------------------------------------------------------------

  function show () {
    final lopts = [ Menu.ilink("@", "asterisk") ];
    paths.sort((p1, p2) -> p1.id > p2.id ? 1 : -1);
    for (p in paths) {
      if (p.isValid && p.isShown) {
        lopts.push(Menu.separator());
        lopts.push(Menu.tlink(p.id, p.id));
      }
    }
    final ropts = [ Menu.ioption("exit", "cross", close) ];

    final menu = new Menu(lopts, ropts, pack);
    final body = Q("div");

    switch (pack) {
      case "@": new PathsPg(body, cf, paths).show();
      case pk: switch (pkPath) {
        case None: IndexPg.mk(body, pk);
        case Some(p): switch (anchor) {
          case Some (a): CodePg.mk(body, pk, p, a);
          case None: ModulePg.mk(body, pk, p);
        }
      }
    }

    wg
      .removeAll()
      .add(menu.wg)
      .add(body)
    ;
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
      final wg = Q("div");
      new MsgPg(
        wg , _args(_("Logout-message"), [Cts.appName]), false
      ).show();
      Q("@body")
        .removeAll()
        .add(wg)
        .add(Cts.foot)
      ;
    });
  }

  function savePath () {
    var path = pack;
    switch (pkPath) {
      case Some(p):
        path += "@" + p;
      case None:
    }
    Cts.client.send([
      "source" => Js.ws("Main"),
      "rq" => Js.ws("savePath"),
      "path" => Js.ws(path),
    ], rp -> {
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
          final paths = rp["paths"].ra().map(Dpath.fromJs);
          new Main(wg, conf, paths).show();
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

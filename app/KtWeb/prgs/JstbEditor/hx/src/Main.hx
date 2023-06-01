// Copyright 20-Jan-2023 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.B64;
import dm.Opt;
import I18n._;
import I18n._args;
import data.Tpath;
import pgs.TlistEditor;
import pgs.Editor;

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
    var tpath = None;
    if (url.exists("0")) {
      try {
        tpath = Some(Tpath.fromJs(Js.from(B64.decode(url.get("0")))));
      } catch (e) {
      }
    }

    final body = Q("div");

    switch (tpath) {
      case Some(path): Editor.mk(body, path);
      case None: TlistEditor.mk(body);
    }

    wg
      .removeAll()
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
      ;

      haxe.Timer.delay(() -> {
        var fc = Q("#autofocus");
        if (fc.e != null) fc.e.focus();
      }, 120);
    });
  }

}

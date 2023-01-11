// Copyright 07-Nov-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.Menu;
import I18n._;
import I18n._args;
import pgs.Home;

/// Applicatoin entry.
class Main {
  final wg: Domo;
  var isPicture = true;

  function new (wg: Domo, lang: String) {
    this.wg = wg;
    if (lang == "es") I18n.es();
    else I18n.en();
  }

  // View ----------------------------------------------------------------------

  public function show (): Void {
    final menu = new Menu(
      [ Menu.toption("pictures", _("Pictures"), showPictures),
        Menu.separator(),
        Menu.toption("trash", _("Trash"), showTrash),
      ],
      [],
      isPicture ? "pictures" : "trash"
    );

    final waiter = Q("div");
    haxe.Timer.delay(() ->
      waiter
        .add(Q("table")
          .att("align", "center")
          .add(Q("tr")
            .add(Q("td")
              .klass("frame")
              .add(Ui.img("wait.gif"))))),
      2000
    );
    final body = Q("div").add(waiter);

    wg
      .removeAll()
      .add(menu.wg)
      .add(body)
    ;

    Home.mk(body, isPicture);
  }

  // Control -------------------------------------------------------------------

  function showPictures (): Void {
    isPicture = true;
    show();
  }

  function showTrash (): Void {
    isPicture = false;
    show();
  }

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
    });
  }

}

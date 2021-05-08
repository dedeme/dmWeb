// Copyright 26-Apr-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.Store;
import I18n._;
import I18n._args;

/// Applicatoin entry.
class Main {

  var allMenu = false;
  var wg: Domo;

  function new (wg: Domo) {
    this.wg = wg;

    view();
  }

  // VIEW

  function view () {
    final lang = switch (Store.get(Cts.langKey)) {
      case Some(l): l;
      case None: "es";
    }
    if (lang == "es") I18n.es();
    else I18n.en();

    wg
      .removeAll()
      .klass("margin")
      .add(Q("div")
        .klass("head")
        .text(_("Wallpapers")))
      .add(Q("div")
        .html("&nbsp;"))
      .add(Q("table")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td")
            .add(Q("button")
            .att("id", "mainButton")
            .text(_("Wallpapers"))
            .on(CLICK, e -> Pictures.mk(wg, () -> view())))))
        .add(Q("tr")
          .add(Q("td")
            .add(Q("button")
              .text(_("Wallpapers with Music"))
              .on(CLICK, e -> Songs.mk(wg, () -> view())))))
        .add(Q("tr")
          .add(Q("td")
            .add(Q("hr"))))
        .adds(moreRows()))
      .add(Cts.foot)
    ;

    final mainButton = Q("#mainButton").e;
    if (mainButton != null) mainButton.focus();
  }

  function moreRows (): Array<Domo> {
    return allMenu
      ? [
          Q("tr")
            .add(Q("td")
              .add(Q("button")
                .text(_("Pictures Management"))
                .on(CLICK, e -> PictsManagement.mk(wg, () -> view())))),
          Q("tr")
            .add(Q("td")
              .add(Q("button")
                .text(_("Songs Management"))
                .on(CLICK, e -> SongsManagement.mk(wg, () -> view())))),
          Q("tr")
            .add(Q("td")
              .add(Q("hr"))),
          Q("tr")
            .add(Q("td")
              .add(Q("button")
                .text(_("Change Language"))
                .on(CLICK, e -> changeLang(wg)))),
          Q("tr")
            .add(Q("td")
              .add(Q("button")
                .text(_("Change Password"))
                .on(CLICK, e -> ChangePass.mk(wg, Cts.app, () -> view())))),
          Q("tr")
            .add(Q("td")
              .add(Q("button")
                .text(_("Close"))
                .on(CLICK, e -> close())))
        ]
      : [Q("tr")
            .add(Q("td")
              .add(Q("button")
                .text(_("More") + "...")
                .on(CLICK, e -> showAllMenu())))
        ]
    ;
  }

  // CONTROL

  function showAllMenu () {
    allMenu = true;
    view();
  }

  function close () {
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
        .add(Q("div")
          .klass("margin")
          .add(wg)
          .add(Cts.foot))
      ;
    });
  }

  function changeLang (wg: Domo): Void {
    final lang = switch (Store.get(Cts.langKey)) {
      case Some(l): l;
      case None: "es";
    }
    Store.put(Cts.langKey, lang == "es" ? "en" : "es");
    view();
  }

  function pending () {
    Ui.alert("Without implementation");
  }

  // STATIC

  static function mk (wg: Domo, fn: () -> Void) {
    Cts.client.connect(ok -> {
      if (ok) {
        new Main(wg);
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
      ;
      Q("#mainButton").e.focus();
    });
  }
}

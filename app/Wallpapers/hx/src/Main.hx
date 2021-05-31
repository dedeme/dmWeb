// Copyright 26-Apr-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import js.html.KeyboardEvent;
import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.Store;
import pages.Pictures;
import pages.Songs;
import pages.DanceSelector;
import pages.PictsManagement;
import pages.SongsManagement;
import pages.DanceManagement;
import pages.StandBy;
import pages.Times;
import pages.ChangePass;
import pages.Authentication;
import pages.MsgPage;
import I18n._;
import I18n._args;

/// Applicatoin entry.
class Main {

  final wallpapersBt = Q("button").text(_("Wallpapers"));
  final songsBt = Q("button").text(_("Wallpapers with Music"));
  final shortDanceBt = Q("button").text(_("Short Dance"));
  final longDanceBt = Q("button").text(_("Long Dance"));
  final standByBt = Q("button").text(_("Stand By"));

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

    wallpapersBt
      .on(KEYDOWN, e -> keyInButton(e, null, songsBt))
      .on(CLICK, e -> Pictures.mk(wg, reload))
    ;
    songsBt
      .on(KEYDOWN, e -> keyInButton(e, wallpapersBt, shortDanceBt))
      .on(CLICK, e -> Songs.mk(wg, reload))
    ;
    shortDanceBt
      .on(KEYDOWN, e -> keyInButton(e, songsBt, longDanceBt))
      .on(CLICK, e -> DanceSelector.mk(wg, true, reload))
    ;
    longDanceBt
      .on(KEYDOWN, e -> keyInButton(e, shortDanceBt, standByBt))
      .on(CLICK, e -> DanceSelector.mk(wg, false, reload))
    ;
    standByBt
      .on(KEYDOWN, e -> keyInButton(e, longDanceBt, null))
      .on(CLICK, e -> new StandBy(wg, reload))
    ;

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
            .add(wallpapersBt)))
        .add(Q("tr")
          .add(Q("td")
            .add(songsBt)))
        .add(Q("tr")
          .add(Q("td")
            .add(shortDanceBt)))
        .add(Q("tr")
          .add(Q("td")
            .add(longDanceBt)))
        .add(Q("tr")
          .add(Q("td")
            .add(standByBt)))
        .add(Q("tr")
          .add(Q("td")
            .add(Q("hr"))))
        .adds(moreRows()))
      .add(Cts.foot)
    ;

    haxe.Timer.delay(() -> {
      final mainButton = wallpapersBt.e;
      if (mainButton != null) mainButton.focus();
    }, 100);
  }

  function moreRows (): Array<Domo> {
    return allMenu
      ? [
          Q("tr")
            .add(Q("td")
              .add(Q("button")
                .text(_("Pictures Management"))
                .on(CLICK, e -> PictsManagement.mk(wg, reload)))),
          Q("tr")
            .add(Q("td")
              .add(Q("button")
                .text(_("Songs Management"))
                .on(CLICK, e -> SongsManagement.mk(wg, reload)))),
          Q("tr")
            .add(Q("td")
              .add(Q("button")
                .text(_("Dance Management"))
                .on(CLICK, e -> DanceManagement.mk(wg, reload)))),
          Q("tr")
            .add(Q("td")
              .add(Q("button")
                .text(_("Times Management"))
                .on(CLICK, e -> Times.mk(wg, reload)))),
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
                .on(CLICK, e -> ChangePass.mk(wg, Cts.app, reload)))),
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

  function reload (): Void {
    js.Browser.location.assign("");
  }

  function keyInButton(ev: KeyboardEvent, up: Domo, down: Domo): Void {
    if (ev.keyCode == KeyboardEvent.DOM_VK_UP && up != null) {
      up.e.focus();
      ev.preventDefault();
      return;
    }

    if (ev.keyCode == KeyboardEvent.DOM_VK_DOWN && down != null) {
      down.e.focus();
      ev.preventDefault();
      return;
    }
  }

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
    });
  }
}

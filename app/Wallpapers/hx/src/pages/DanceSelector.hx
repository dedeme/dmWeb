// Copyright 17-May-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pages;

import js.html.KeyboardEvent;
import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.Client;
import dm.Menu;
import data.DanceSong;
import I18n._;

// Dance selector page
class DanceSelector {

  final wg: Domo;
  final isShort: Bool;
  final groups: Array<String>;
  final group: String;
  final danceSongs: Array<DanceSong>;
  final fnBack: () -> Void;

  function new (
    wg: Domo, isShort: Bool,
    groups: Array<String>, group: String, danceSongs: Array<DanceSong>,
    fnBack: () -> Void
  ) {
    this.wg = wg;
    this.isShort = isShort;
    this.groups = groups;
    this.group = group;
    this.danceSongs = danceSongs;
    this.fnBack = fnBack;

    view();
  }

  // VIEW

  function view () {
    final lopts = [];
    var first = true;
    groups.sort((e1, e2) ->
      dm.Str.compare(e1.toUpperCase(), e2.toUpperCase())
    );
    for (g in groups) {
      if (first) first = false;
      else lopts.push(Menu.separator());

      lopts.push(Menu.toption(g, g, () -> changeGroup(g)));
    }
    final ropts = [
      Menu.toption("_back_", _("Back"), fnBack)
    ];

    final menu = new Menu(lopts, ropts, group);

    wg
      .removeAll()
      .add(Q("div")
        .klass("head")
        .text(isShort ? _("Short Dance Songs") : _("Long Dance Songs")))
      .add(Q("hr"))
      .add(menu.wg)
     .add(Q("table")
        .att("align", "center")
        .klass("frame3")
        .adds(entries()))
      .add(Cts.foot)
    ;

    haxe.Timer.delay(() -> {
      final mainButton = Q("#bt:0").e;
      if (mainButton != null) mainButton.focus();
    }, 100);

  }

  function entries (): Array<Domo> {
    danceSongs.sort((e1, e2) ->
      dm.Str.compare(e1.id.toUpperCase(), e2.id.toUpperCase())
    );
    final rows: Array<Domo> = [];
    for (i in 0...danceSongs.length) {
      final s = danceSongs[i];
      rows.push(
        Q("tr")
          .add(Q("td")
            .att("title", _("Quality"))
            .style("width:5px")
            .klass("frame")
            .text(s.level == 0 ? _("???")
                : s.level == 1 ? _("Ok")
                  : _("Good")
              ))
          .add(Q("td")
            .att("title", _("Speed"))
            .style("width:5px")
            .klass("frame")
            .text(s.level == 0 ? "???" : s.speed == 0 ? _("Slow") : _("Fast")))
          .add(Q("td")
            .add(Q("button")
              .att("id", "bt:" + i)
              .style("width:100%;white-space:nowrap;text-align:left")
              .text(s.id))
              .on(KEYDOWN, e -> keyInButton(e, i))
              .on(CLICK, e -> play(s.id)))
          .add(Q("td")
            .klass("frame")
            .text(Cts.formatInt(isShort ? s.shortPlays : s.longPlays, 2)))
      );
    }
    return rows;
  }

  // CONTROL

  function changeGroup(g: String) {
    Cts.client.ssend([
      "source" => Js.ws("DanceSelector"),
      "rq" => Js.ws("changeGroup"),
      "group" => Js.ws(g)
    ], rp -> {
      mk(wg, isShort, fnBack);
    });
  }

  function keyInButton(ev: KeyboardEvent, isong: Int): Void {
    if (ev.keyCode == KeyboardEvent.DOM_VK_UP && isong > 0) {
      Q("#bt:" + (isong - 1)).e.focus();
      ev.preventDefault();
      return;
    }
    if (
      ev.keyCode == KeyboardEvent.DOM_VK_DOWN && isong < danceSongs.length - 1
    ) {
      Q("#bt:" + (isong + 1)).e.focus();
      ev.preventDefault();
      return;
    }

    if (ev.keyCode == KeyboardEvent.DOM_VK_LEFT) {
      var ix = groups.indexOf(group);
      if (ix == -1) ix = 0;
      if (ix == 0) {
        fnBack();
      } else {
        --ix;
        changeGroup(groups[ix]);
      }
      ev.preventDefault();
      return;
    }

    if (ev.keyCode == KeyboardEvent.DOM_VK_RIGHT) {
      var ix = groups.indexOf(group);
      if (ix == -1) ix = 0;

      if (ix == groups.length - 1) {
        fnBack();
      } else {
        ++ix;
        changeGroup(groups[ix]);
      }
      ev.preventDefault();
      return;
    }
  }

  function play (song: String) {
    Dance.mk(wg, isShort, group, song, fnBack);
  }

  // STATIC

  public static function mk(wg: Domo, isShort: Bool, fnBack: () -> Void): Void {
    Cts.client.ssend([
      "source" => Js.ws("DanceSelector"),
      "rq" => Js.ws("idata")
    ], rp -> {
      final groups = rp["groups"].ra().map(e -> e.rs());
      final group = rp["group"].rs();
      final danceSongs = rp["songs"].ra().map(e -> DanceSong.fromJs(e));

      new DanceSelector(wg, isShort, groups, group, danceSongs, fnBack);
    });

  }
}

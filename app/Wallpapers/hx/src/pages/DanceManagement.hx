// Copyright 17-May-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pages;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.Client;
import dm.Menu;
import data.DanceSong;
import I18n._;

// Dance management page
class DanceManagement {

  final wg: Domo;
  final groups: Array<String>;
  final group: String;
  final danceSongs: Array<DanceSong>;
  final fnBack: () -> Void;

  function new (
    wg: Domo,
    groups: Array<String>, group: String, danceSongs: Array<DanceSong>,
    fnBack: () -> Void
  ) {
    this.wg = wg;
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

    final menu = new Menu(lopts, ropts, group, true);

    wg
      .removeAll()
      .add(Q("div")
        .klass("head")
        .text(_("Dance Songs Management")))
      .add(Q("hr"))
      .add(menu.wg)
      .add(Q("table")
        .att("align", "center")
        .klass("frame3")
        .adds(entries()))
      .add(Cts.foot)
    ;
  }

  function entries (): Array<Domo> {
    danceSongs.sort((e1, e2) ->
      dm.Str.compare(e1.id.toUpperCase(), e2.id.toUpperCase())
    );
    return danceSongs.map(s ->
      Q("tr")
        .add(Q("td")
          .att("title", _("Quality"))
          .style("width:5px")
          .klass("frame")
          .add(Q("table")
            .klass("main")
            .add(Q("tr")
              .add(Q("td")
                .add(Q("input")
                  .att("type", "radio")
                  .att("name", "level|" + s)
                  .checked(0 == s.level))
                  .on(CLICK, e -> setLevel(s.id, 0)))
              .add(Q("td")
                .html("?&nbsp;"))
              .add(Q("td")
                .add(Q("input")
                  .att("type", "radio")
                  .att("name", "level|" + s)
                  .checked(1 == s.level))
                  .on(CLICK, e -> setLevel(s.id, 1)))
              .add(Q("td")
                .html("&#8722;&nbsp;"))
              .add(Q("td")
                .add(Q("input")
                  .att("type", "radio")
                  .att("name", "level|" + s)
                  .checked(2 == s.level))
                  .on(CLICK, e -> setLevel(s.id, 2)))
              .add(Q("td")
                .html("+&nbsp;"))
                  )))
        .add(Q("td")
          .att("title", _("Speed"))
          .style("width:5px")
          .klass("frame")
          .add(Q("table")
            .klass("main")
            .add(Q("tr")
              .add(Q("td")
                .add(Q("input")
                  .att("type", "radio")
                  .att("name", "speed|" + s)
                  .checked(0 == s.speed))
                  .on(CLICK, e -> setSpeed(s.id, 0)))
              .add(Q("td")
                .html("&#171;&nbsp;"))
              .add(Q("td")
                .add(Q("input")
                  .att("type", "radio")
                  .att("name", "speed|" + s)
                  .checked(1 == s.speed))
                  .on(CLICK, e -> setSpeed(s.id, 1)))
              .add(Q("td")
                .html("&#187;&nbsp;"))
                  )))
        .add(Q("td")
          .klass("frame")
          .text(s.id))
    );
  }

  // CONTROL

  function changeGroup(g: String) {
    Cts.client.ssend([
      "source" => Js.ws("DanceManagement"),
      "rq" => Js.ws("changeGroup"),
      "group" => Js.ws(g)
    ], rp -> {
      mk(wg, fnBack);
    });
  }

  function setLevel(song: String, level: Int) {
    Cts.client.ssend([
      "source" => Js.ws("DanceManagement"),
      "rq" => Js.ws("setLevel"),
      "group" => Js.ws(group),
      "song" => Js.ws(song),
      "level" => Js.wi(level)
    ], rp -> {
    });
  }

  function setSpeed(song: String, speed: Int) {
    Cts.client.ssend([
      "source" => Js.ws("DanceManagement"),
      "rq" => Js.ws("setSpeed"),
      "group" => Js.ws(group),
      "song" => Js.ws(song),
      "speed" => Js.wi(speed)
    ], rp -> {
    });
  }

  // STATIC

  public static function mk(wg: Domo, fnBack: () -> Void): Void {
    Cts.client.ssend([
      "source" => Js.ws("DanceManagement"),
      "rq" => Js.ws("idata")
    ], rp -> {
      final groups = rp["groups"].ra().map(e -> e.rs());
      final group = rp["group"].rs();
      final danceSongs = rp["songs"].ra().map(e -> DanceSong.fromJs(e));

      new DanceManagement(wg, groups, group, danceSongs, fnBack);
    });

  }
}

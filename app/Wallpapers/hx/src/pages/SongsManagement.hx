// Copyright 07-May-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pages;

import js.html.InputElement;
import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.It;
import dm.Str;
import dm.Menu;
import data.Song;
import I18n._;

/// Pictures management.
class SongsManagement {
  final wg: Domo;
  final fnBack: Void -> Void;
  final groups: Array<String>;
  final group: String;
  final songs: Array<Song>;
  final songGroup: String;
  final song: String;

  function new (
    wg: Domo,
    groups: Array<String>, group: String,
    songs: Array<Song>, songGroup: String, song: String,
    fnBack: Void -> Void
  ) {
    this.wg = wg;
    groups.sort((e1, e2) ->
      Str.compare(e1.toUpperCase(), e2.toUpperCase())
    );
    this.groups = groups;
    this.group = group;
    songs.sort((e1, e2) ->
      Str.compare(e1.id.toUpperCase(), e2.id.toUpperCase())
    );
    this.songs = songs;
    this.songGroup = songGroup;
    this.song = song;
    this.fnBack = fnBack;

    view();
  }

  // VIEW

  function view (): Void {
    final lopts = [];
    var first = true;
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
        .text(_("Songs Management")))
      .add(Q("hr"))
      .add(menu.wg)
      .add(Q("table")
        .att("align", "center")
        .klass("frame3")
        .adds(rows()))
      .add(Cts.foot)
    ;
  }

  function rows (): Array<Domo> {
    final r: Array<Domo> = [];
    for (s in songs) {
      final tds: Array<Domo> = [];

      tds.push(Q("td")
        .style("width:5px")
        .add(Q("input")
          .att("type", "radio")
          .att("name", "sel")
          .checked(song == s.id && group == songGroup))
          .on(CLICK, e -> setSel(s.id))
      );

      final range = Q("input")
        .att("type", "range")
        .att("min", 0)
        .att("max", 100)
        .att("value", s.lapse)
      ;
      range.on(CHANGE, e -> setLapse(
        s.id, cast(range.e, InputElement).valueAsNumber
      ));

      tds.push(Q("td")
        .klass("frame")
        .style("width:5px;")
        .add(range)
      );


      for (i in 1...4) {
        tds.push(Q("td")
          .style("width:5px")
          .add(Q("input")
            .att("type", "radio")
            .att("name", s.id)
            .checked(i == s.level))
            .on(CLICK, e -> setLevel(s.id, i))
        );
        tds.push(Q("td")
          .style("width:5px")
          .text(Std.string(i))
        );
        tds.push(Q("td")
          .style("width:5px")
          .html("&nbsp;")
        );
      }
      tds.push(Q("td")
        .klass("frame")
        .text(s.id)
      );
      r.push(Q("tr").adds(tds));
    }
    return r;
  }

  // CONTROL

  function changeGroup(g: String) {
    Cts.client.ssend([
      "source" => Js.ws("SongsManagement"),
      "rq" => Js.ws("changeGroup"),
      "group" => Js.ws(g)
    ], rp -> {
      mk(wg, fnBack);
    });
  }

  function setLevel (song: String, level: Int): Void {
    Cts.client.ssend([
      "source" => Js.ws("SongsManagement"),
      "rq" => Js.ws("setLevel"),
      "group" => Js.ws(group),
      "song" => Js.ws(song),
      "level" => Js.wi(level)
    ], rp -> {
    });
  }

  function setSel (song: String): Void {
    Cts.client.ssend([
      "source" => Js.ws("SongsManagement"),
      "rq" => Js.ws("setSel"),
      "group" => Js.ws(group),
      "song" => Js.ws(song)
    ], rp -> {
      if (!rp["ok"].rb()) {
        mk(wg, fnBack);
      }
    });
  }

  function setLapse (song: String, lapse: Float) {
    Cts.client.ssend([
      "source" => Js.ws("SongsManagement"),
      "rq" => Js.ws("setLapse"),
      "group" => Js.ws(group),
      "song" => Js.ws(song),
      "lapse" => Js.wf(lapse)
    ], rp -> {
    });
  }

  // STATIC

  public static function mk (wg: Domo, fnBack: Void -> Void): Void {
    Cts.client.ssend([
      "source" => Js.ws("SongsManagement"),
      "rq" => Js.ws("idata")
    ], rp -> {
      final groups = rp["groups"].ra().map(e -> e.rs());
      final group = rp["group"].rs();
      final songs = rp["songs"].ra().map(e -> Song.fromJs(e));
      final songGroup = rp["songGroup"].rs();
      final song = rp["song"].rs();
      new SongsManagement(wg, groups, group, songs, songGroup, song, fnBack);
    });
  }
}

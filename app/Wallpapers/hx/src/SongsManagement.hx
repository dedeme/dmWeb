// Copyright 07-May-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.It;
import dm.Str;
import data.Song;
import I18n._;

/// Pictures management.
class SongsManagement {
  final wg: Domo;
  final fnBack: Void -> Void;
  final songs: Array<Song>;

  function new (
    wg: Domo, songs: Array<Song>, fnBack: Void -> Void
  ) {
    this.wg = wg;
    songs.sort((e1, e2) ->
      Str.compare(e1.id.toUpperCase(), e2.id.toUpperCase())
    );
    this.songs = songs;
    this.fnBack = fnBack;

    view();
  }

  // VIEW

  function view (): Void {
    wg
      .removeAll()
      .add(Q("div")
        .klass("head")
        .text(_("Songs Management")))
      .add(Q("table")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td")
            .add(Ui.link(fnBack)
              .klass("link")
              .text("[ " + _("Back") + " ]")))))
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

  function setLevel (id: String, lv: Int): Void {
    Cts.client.ssend([
      "source" => Js.ws("SongsManagement"),
      "rq" => Js.ws("setLevel"),
      "id" => Js.ws(id),
      "level" => Js.wi(lv)
    ], rp -> {
      mk(wg, fnBack);
    });
  }

  // STATIC

  public static function mk (wg: Domo, fnBack: Void -> Void): Void {
    Cts.client.ssend([
      "source" => Js.ws("SongsManagement"),
      "rq" => Js.ws("idata")
    ], rp -> {
      final songs = rp["songs"].ra().map(e -> Song.fromJs(e));

      new SongsManagement(wg, songs, fnBack);
    });
  }
}

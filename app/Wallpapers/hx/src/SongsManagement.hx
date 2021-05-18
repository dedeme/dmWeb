// Copyright 07-May-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import js.html.InputElement;
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
    final sel = Store.getSelId();
    final r: Array<Domo> = [];
    for (s in songs) {
      final tds: Array<Domo> = [];

      tds.push(Q("td")
        .style("width:5px")
        .add(Q("input")
          .att("type", "radio")
          .att("name", "sel")
          .checked(sel == s.id))
          .on(CLICK, e -> setSel(s.id))
      );

      final range = Q("input")
        .att("type", "range")
        .att("min", 0)
        .att("max", 100)
        .att("value", s.lapse)
        .checked(sel == s.id)
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

  function setLevel (id: String, lv: Int): Void {
    Store.setLevel(id, lv);
    mk(wg, fnBack);
  }

  function setSel (id: String): Void {
    Store.setSel(id);
    view();
  }

  function setLapse (id: String, value: Float) {
    Store.setLapse(id, value);
    mk(wg, fnBack);
  }

  // STATIC

  public static function mk (wg: Domo, fnBack: Void -> Void): Void {
    Cts.client.ssend([
      "source" => Js.ws("SongsManagement"),
      "rq" => Js.ws("idata")
    ], rp -> {
      final songIds = rp["songs"].ra().map(e -> e.rs());
      final songs = Store.sync(songIds);

      new SongsManagement(wg, songs, fnBack);
    });
  }
}

// Copyright 07-May-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import js.html.KeyboardEvent;
import haxe.Timer;
import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import data.Pict;
import data.Song;
import I18n._;

/// Pictures + songs show.
class Songs {
  final w = Std.string(Cts.screenWidth);
  final h = Std.string(Cts.screenHeight);
  final div = Q("div");
  final img = Q("img");
  final audio = new js.html.Audio();
  final wg: Domo;
  final fnBack: Void -> Void;
  final clocks: Clocks;

  var timer = new Timer(Cts.songsTime);

  public function new (
    wg: Domo, group: String, pict: String, song: Song, fnBack: Void -> Void
  ) {
    this.wg = wg;
    this.fnBack = fnBack;

    img
      .att("src", "img/fondosEscritorio/" + group + "/" + pict)
      .style(
        "width:" + w +"px; height:" + h + "px;" +
        "z-index:1;" +
        "transition: opacity 5s linear;"
      )
    ;
    final timeDiv = Q("div")
      .style(
        "z-index:2;" +
        "position:relative;" +
        "top:-250px;" +
        "left:0px;"
      )
    ;
    this.clocks = new Clocks(timeDiv);

    audio.src = "songs/" + song.id;
    audio.autoplay = true;
    audio.controls = false;
    audio.volume = 0.5;
    final tm = new Timer(50);
    tm.run = () -> {
      if (!Math.isNaN(audio.duration)) {
        audio.currentTime = (audio.duration * song.lapse) / 100.0;
        tm.stop();
      }
    }

    div
      .style(
        "width:" + w +"px; height:" + h + "px;" +
        "background-image: " +
          "url(img/fondosEscritorio/" + group + "/" + pict + ");" +
        "background-size:" + w + "px " + h + "px;"
      )
      .add(img)
      .add(timeDiv)
      .on(CLICK, goBack)
    ;

    Q("@body")
      .on(KEYDOWN, e -> keyDown(e));

    view();

    div.e.requestFullscreen();
    timer.run = () -> {
      Cts.client.ssend([
        "source" => Js.ws("Songs"),
        "rq" => Js.ws("idata"),
      ], rp -> {
        final newGroup = rp["group"].rs();
        final newPict = rp["pict"].rs();
        if (newGroup != group || newPict != pict) {
          group = newGroup;
          pict = newPict;
          showPict(group, pict);
        }

        if (audio.ended) {
          final songs = rp["songs"].ra().map(e -> e.rs());
          final song = Store.getNext(songs);

          audio.src = "songs/" + song.id;
          final tm = new Timer(50);
          tm.run = () -> {
            if (!Math.isNaN(audio.duration)) {
              audio.currentTime = (audio.duration * song.lapse) / 100.0;
              tm.stop();
            }
          }

        } else {
          Store.setCurrentLapse(audio.currentTime * 100.0 / audio.duration);
        }
      });
    }
  }

  // VIEW

  function view (): Void {
    wg
      .removeAll()
      .add(new Domo(audio))
      .add(div)
    ;
  }

  function showPict (g: String, p: String) {
    final url = "img/fondosEscritorio/" + g + "/" + p;
    div
      .setStyle(
        "background-image",
        "url(" + url +")"
      );
    Timer.delay(() -> {
      img
        .setStyle("opacity", "0")
      ;
      Timer.delay(() -> {
        img
          .att("src", url)
          .setStyle("opacity", "1")
        ;
      }, 8000);
    }, 2000);
  }

  // CONTROL

  function goBack (): Void {
    timer.stop();
    js.Browser.document.exitFullscreen();
    fnBack();
  }

  function keyDown (ev: KeyboardEvent): Void {
    if (ev.keyCode == KeyboardEvent.DOM_VK_UP) {
      goBack();
      ev.preventDefault();
      return;
    }

    if (ev.keyCode == KeyboardEvent.DOM_VK_LEFT) {
      clocks.clockChangeOpacity();
      ev.preventDefault();
      return;
    }
    if (ev.keyCode == KeyboardEvent.DOM_VK_RIGHT) {
      clocks.chronChangeOpacity();
      ev.preventDefault();
      return;
    }
  }

  // STATIC

  public static function mk (wg: Domo, fnBack: Void -> Void): Void {
    Cts.client.ssend([
      "source" => Js.ws("Songs"),
      "rq" => Js.ws("idata")
    ], rp -> {
      final group = rp["group"].rs();
      final pict = rp["pict"].rs();
      final songs = rp["songs"].ra().map(e -> e.rs());
      final song: Song = Store.getSel(songs);
      new Songs(wg, group, pict, song, fnBack);
    });
  }
}

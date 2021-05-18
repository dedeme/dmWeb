// Copyright 18-May-2021 ºDeme
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

/// Dance play.
class Dance {
  final w = Std.string(Cts.screenWidth);
  final h = Std.string(Cts.screenHeight);
  final div = Q("div");
  final img = Q("img");
  final audio = new js.html.Audio();
  final wg: Domo;
  final fnBack: Void -> Void;
  final clocks: Clocks;

  var timer = new Timer(Cts.picturesTime);

  public function new (
    wg: Domo,
    isShort: Bool, group: String, pict: String,
    songGroup: String, song: String,
    fnBack: Void -> Void
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

    var vol = 0.5;
    audio.src = "dance/" + songGroup + "/" + song;
    audio.autoplay = true;
    audio.controls = false;
    audio.volume = vol;
    final tm = new Timer(50);
    tm.run = () -> {
      if (!Math.isNaN(audio.duration)) {
        final duration = isShort ? Cts.shortDanceTime : Cts.longDanceTime;
        final duration2 = duration + Cts.fadeOutDanceTime;
        audio.currentTime = dm.Rnd.f(0, audio.duration - (duration2 / 1000), 0);
        Timer.delay(() -> {
          clocks.chronOutOfTime();
          final fadeSec = Cts.fadeOutDanceTime / 1000;
          final delta = vol / fadeSec;
          final tm2 = new Timer(1000);
          tm2.run = () -> {
            if (vol <= 0) {
              tm2.stop();
              return;
            }
            vol = vol - delta;
            audio.volume = vol;
          }
        }, Std.int(duration));
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
        "source" => Js.ws("Pictures"),
        "rq" => Js.ws("idata"),
      ], rp -> {
        final newGroup = rp["group"].rs();
        final newPict = rp["pict"].rs();
        if (newGroup != group || newPict != pict) {
          group = newGroup;
          pict = newPict;
          showPict(group, pict);
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

  public static function mk (
    wg: Domo,
    isShort: Bool, songGroup: String, song: String,
    fnBack: Void -> Void
  ): Void {
    Cts.client.ssend([
      "source" => Js.ws("Pictures"),
      "rq" => Js.ws("idata")
    ], rp -> {
      final group = rp["group"].rs();
      final pict = rp["pict"].rs();
      new Dance(wg, isShort, group, pict, songGroup, song, fnBack);
    });
  }
}

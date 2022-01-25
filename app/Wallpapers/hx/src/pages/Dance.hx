// Copyright 18-May-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pages;

import js.html.KeyboardEvent;
import haxe.Timer;
import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import data.Pict;
import data.Song;
import widgets.Clocks;
import widgets.Info;
import widgets.PictTime;
import I18n._;

/// Dance play.
class Dance {
  final div: Domo;
  final img: Domo;
  final audio = new js.html.Audio();
  final wg: Domo;
  final fnBack: Void -> Void;
  final clocks: Clocks;
  final infoPicture: Info;
  final infoDance: Info;
  final pictTime: PictTime;

  var timer = new Timer(Media.picturesTime);

  function new (
    wg: Domo,
    duration: Float, group: String, pict: Pict,
    songGroup: String, song: String,
    fnBack: Void -> Void
  ) {
    this.wg = wg;
    this.fnBack = fnBack;

    final visuals = Media.visuals();

    img = visuals.img
      .att("src", "img/fondosEscritorio/" + group + "/" + pict.id)
    ;

    this.clocks = new Clocks(visuals.time);

    audio.src = "dance/" + songGroup + "/" + song;
    audio.autoplay = true;
    audio.controls = true;
    audio.volume = Media.volume;
    final tm = new Timer(50);
    tm.run = () -> {
      if (!Math.isNaN(audio.duration)) {
        final duration2 = duration + Media.fadeOutDanceTime;
        audio.currentTime = dm.Rnd.f(0, audio.duration - (duration2 / 1000), 0);
        Timer.delay(() -> {
          clocks.chronOutOfTime();
          Media.fadeOut(true, audio, Media.fadeOutDanceTime);
        }, Std.int(duration));
        tm.stop();
      }
    }

    infoPicture = new Info(-485, Info.pictureWg(group, pict));
    infoDance = new Info(
      -480, Info.danceWg(songGroup, song, Std.int(duration / 60000), audio)
    );
    pictTime = new PictTime(-480);

    div = visuals.div
      .setStyle(
        "background-image",
        "url('img/fondosEscritorio/" + group + "/" + pict.id + "')"
      )
      .add(infoPicture.wg)
      .add(infoDance.wg)
      .add(pictTime.wg)
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
        final newPict = Pict.fromJs(rp["pict"]);
        if (newGroup != group || newPict.id != pict.id) {
          group = newGroup;
          pict = newPict;
          Media.changePict(div, img, infoPicture, group, pict);
        }
      });
    }
  }

  // VIEW

  function view (): Void {
    wg
      .removeAll()
      .add(div)
    ;
  }

  // CONTROL

  function goBack (): Void {
    fnBack();
  }

  function keyDown (ev: KeyboardEvent): Void {
    if (ev.keyCode == KeyboardEvent.DOM_VK_UP) {
      goBack();
      ev.preventDefault();
      return;
    }

    if (ev.keyCode == KeyboardEvent.DOM_VK_DOWN) {
      infoPicture.changeOpacity();
      infoDance.changeOpacity();
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

    if (
      ev.keyCode >= KeyboardEvent.DOM_VK_1 &&
      ev.keyCode <= KeyboardEvent.DOM_VK_6
    ) {
      pictTime.show(ev.keyCode - KeyboardEvent.DOM_VK_0);
      ev.preventDefault();
      return;
    }

    if (
      ev.keyCode >= KeyboardEvent.DOM_VK_NUMPAD1 &&
      ev.keyCode <= KeyboardEvent.DOM_VK_NUMPAD6
    ) {
      pictTime.show(ev.keyCode - KeyboardEvent.DOM_VK_NUMPAD0);
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
      "source" => Js.ws("Dance"),
      "rq" => Js.ws("idata"),
      "isShort" => Js.wb(isShort),
      "songGroup" => Js.ws(songGroup),
      "song" => Js.ws(song)
    ], rp -> {
      final group = rp["group"].rs();
      final pict = Pict.fromJs(rp["pict"]);
      final duration = rp["duration"].rf(); // milliseconds
      new Dance(wg, duration, group, pict, songGroup, song, fnBack);
    });
  }
}

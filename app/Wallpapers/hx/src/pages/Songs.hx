// Copyright 07-May-2021 ºDeme
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

/// Pictures + songs show.
class Songs {
  final div: Domo;
  final img: Domo;
  final audio = new js.html.Audio();
  final wg: Domo;
  final fnBack: Void -> Void;
  final clocks: Clocks;
  final infoPicture: Info;
  final infoSong: Info;
  final pictTime: PictTime;

  var pictTimer = new Timer(Media.picturesTime);
  var songTimer = new Timer(Media.songsTime);
  var goBackCalls = 0;

  function new (
    wg: Domo,
    group: String, pict: Pict,
    songGroup: String, song: Song,
    fnBack: Void -> Void
  ) {
    this.wg = wg;
    this.fnBack = fnBack;

    final visuals = Media.visuals();

    img = visuals.img
      .att("src", "img/fondosEscritorio/" + group + "/" + pict.id)
    ;

    this.clocks = new Clocks(visuals.time);

    audio.src = "songs/" + songGroup + "/" + song.id;
    audio.autoplay = true;
    audio.controls = true;
    audio.volume = Media.volume;
    final tm = new Timer(50);
    tm.run = () -> {
      if (!Math.isNaN(audio.duration)) {
        audio.currentTime = (audio.duration * song.lapse) / 100.0;
        tm.stop();
      }
    }

    infoPicture = new Info(-485, Info.pictureWg(group, pict));
    infoSong = new Info(-480, Info.songWg(songGroup, song, audio));
    pictTime = new PictTime(-480);

    div = visuals.div
      .setStyle(
        "background-image",
        "url('img/fondosEscritorio/" + group + "/" + pict.id + "')"
      )
      .add(infoPicture.wg)
      .add(infoSong.wg)
      .add(pictTime.wg)
      .on(CLICK, goBack)
    ;

    Q("@body")
      .on(KEYDOWN, e -> keyDown(e));

    view();

    div.e.requestFullscreen();
    pictTimer.run = () -> {
      Cts.client.ssend([
        "source" => Js.ws("Songs"),
        "rq" => Js.ws("pictData")
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

    songTimer.run = () -> {
      if (audio.ended) {
        Cts.client.ssend([
          "source" => Js.ws("Songs"),
          "rq" => Js.ws("newSong"),
        ], rp -> {
          songGroup = rp["group"].rs();
          song = Song.fromJs(rp["song"]);

          audio.src = "songs/" + songGroup + "/" + song.id;
          final tm = new Timer(50);
          tm.run = () -> {
            if (!Math.isNaN(audio.duration)) {
              audio.currentTime = (audio.duration * song.lapse) / 100.0;
              infoSong.changeContent(Info.songWg(songGroup, song, audio));
              tm.stop();
            }
          }
        });
      } else {
        Cts.client.ssend([
          "source" => Js.ws("Songs"),
          "rq" => Js.ws("setLapse"),
          "group" => Js.ws(songGroup),
          "song" => Js.ws(song.id),
          "lapse" => Js.wf(audio.currentTime * 100 / audio.duration)
        ], rp -> {
        });
      }
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
    pictTimer.stop();
    songTimer.stop();

    if (goBackCalls > 0) {
      fnBack();
      return;
    }

    ++goBackCalls;
    Media.fadeOut(false, audio, Media.fadeOutSongEnd);
    Timer.delay(() -> {
      fnBack();
    }, Std.int(Media.fadeOutSongEnd));
  }

  function keyDown (ev: KeyboardEvent): Void {
    if (ev.keyCode == KeyboardEvent.DOM_VK_UP) {
      goBack();
      ev.preventDefault();
      return;
    }

    if (ev.keyCode == KeyboardEvent.DOM_VK_DOWN) {
      infoPicture.changeOpacity();
      infoSong.changeOpacity();
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

  public static function mk (wg: Domo, fnBack: Void -> Void): Void {
    Cts.client.ssend([
      "source" => Js.ws("Songs"),
      "rq" => Js.ws("idata")
    ], rp -> {
      final group = rp["group"].rs();
      final pict = Pict.fromJs(rp["pict"]);
      final groupSong = rp["groupSong"].rs();
      final song: Song = Song.fromJs(rp["song"]);
      new Songs(wg, group, pict, groupSong, song, fnBack);
    });
  }
}

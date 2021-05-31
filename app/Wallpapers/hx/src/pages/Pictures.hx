// Copyright 02-May-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pages;

import js.html.KeyboardEvent;
import haxe.Timer;
import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import data.Pict;
import widgets.Clocks;
import widgets.Info;
import widgets.PictTime;
import I18n._;

/// Pictures show.
class Pictures {
  final div: Domo;
  final img: Domo;
  final wg: Domo;
  final clocks: Clocks;
  final info: Info;
  final pictTime: PictTime;
  final fnBack: Void -> Void;

  var timer = new Timer(Media.picturesTime);

  function new (
    wg: Domo, group: String, pict: Pict, fnBack: Void -> Void
  ) {
    this.wg = wg;
    this.fnBack = fnBack;

    final visuals = Media.visuals();

    img = visuals.img
      .att("src", "img/fondosEscritorio/" + group + "/" + pict.id)
    ;

    info = new Info(-350, Info.pictureWg(group, pict));
    pictTime = new PictTime(-350);

    this.clocks = new Clocks(visuals.time);

    div = visuals.div
      .setStyle(
        "background-image",
        "url('img/fondosEscritorio/" + group + "/" + pict.id + "')"
      )
      .add(info.wg)
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
        "rq" => Js.ws("idata")
      ], rp -> {
        final newGroup = rp["group"].rs();
        final newPict = Pict.fromJs(rp["pict"]);
        if (newGroup != group || newPict.id != pict.id) {
          group = newGroup;
          pict = newPict;
          Media.changePict(div, img, info, group, pict);
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
    timer.stop();
    fnBack();
  }

  function keyDown (ev: KeyboardEvent): Void {
    if (ev.keyCode == KeyboardEvent.DOM_VK_UP) {
      goBack();
      ev.preventDefault();
      return;
    }

    if (ev.keyCode == KeyboardEvent.DOM_VK_DOWN) {
      info.changeOpacity();
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
      "source" => Js.ws("Pictures"),
      "rq" => Js.ws("idata")
    ], rp -> {
      final group = rp["group"].rs();
      final pict = Pict.fromJs(rp["pict"]);

      new Pictures(wg, group, pict, fnBack);
    });
  }
}

// Copyright 02-May-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import js.html.KeyboardEvent;
import haxe.Timer;
import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import data.Pict;
import I18n._;

/// Pictures show.
class Pictures {
  final w = Std.string(Cts.screenWidth);
  final h = Std.string(Cts.screenHeight);
  final div = Q("div");
  final img = Q("img");
  final wg: Domo;
  final fnBack: Void -> Void;
  final clocks: Clocks;

  var timer = new Timer(Cts.picturesTime);

  public function new (
    wg: Domo, group: String, pict: String, fnBack: Void -> Void
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
        "rq" => Js.ws("idata")
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
      .add(div)
    ;
  }

  function showPict (g: String, p: String): Void {
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
    fnBack();
    js.Browser.document.exitFullscreen();
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
      "source" => Js.ws("Pictures"),
      "rq" => Js.ws("idata")
    ], rp -> {
      final group = rp["group"].rs();
      final pict = rp["pict"].rs();

      new Pictures(wg, group, pict, fnBack);
    });
  }
}

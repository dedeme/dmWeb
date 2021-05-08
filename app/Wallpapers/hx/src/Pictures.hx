// Copyright 02-May-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

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

  var timer = new Timer(Cts.picturesTime);
  var pict: String;

  public function new (wg: Domo, pict: String, fnBack: Void -> Void) {
    this.wg = wg;
    this.pict = pict;
    this.fnBack = fnBack;

    img
      .att("src", "img/fondosEscritorio/" + pict)
      .style(
        "width:" + w +"px; height:" + h + "px;" +
        "transition: opacity 5s;"
      )
      .on(CLICK, goBack)
    ;

    div
      .style(
        "width:" + w +"px; height:" + h + "px;" +
        "background-image: " + "url(img/fondosEscritorio/" + pict + ");" +
        "background-size:" + w + "px " + h + "px;"
      )
      .add(img)
    ;

    view();

    div.e.requestFullscreen();
    timer.run = () -> {
      Cts.client.ssend([
        "source" => Js.ws("Pictures"),
        "rq" => Js.ws("idata")
      ], rp -> {
        final newPict = rp["pict"].rs();
        if (newPict != pict) {
          pict = newPict;
          showPict(pict);
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

  function showPict (p: String) {
    div
      .setStyle(
        "background-image",
        "url(img/fondosEscritorio/" + p +")"
      );
    img
      .setStyle("opacity", "0")
    ;
    Timer.delay(() -> {
      img
        .att("src", "img/fondosEscritorio/" + p)
        .setStyle("opacity", "1")
      ;
    }, 8000);
  }

  // CONTROL

  function goBack (): Void {
    timer.stop();
    js.Browser.document.exitFullscreen();
    fnBack();
  }

  // STATIC

  public static function mk (wg: Domo, fnBack: Void -> Void): Void {
    Cts.client.ssend([
      "source" => Js.ws("Pictures"),
      "rq" => Js.ws("idata")
    ], rp -> {
      final pict = rp["pict"].rs();

      new Pictures(wg, pict, fnBack);
    });
  }
}

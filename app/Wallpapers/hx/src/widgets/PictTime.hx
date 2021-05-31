// Copyright 20-May-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package widgets;

import js.html.Audio;
import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import data.Pict;
import data.Song;
import I18n._;

/// Change pictures time widget.
class PictTime {
  public final wg = Q("div");
  final content = Q("div");
  final topPadding: Int;
  final leftPadding: Int;

  public function new (topPadding: Int) {
    this.topPadding = topPadding - Std.int(Media.screenHeight / 2);
    this.leftPadding = Std.int(Media.screenWidth / 2) - 20;

    view();
  }

  // VIEW

  function view (): Void {
    wg
      .removeAll()
      .style(
          "z-index:4;" +
          "position:relative;" +
          "top: " + topPadding + "px;" +
          "left: " + leftPadding  + "px;" +
          "opacity:0;" +
          "transition: opacity 1s linear;"
        )
      .add(content)
    ;
  }

  public function show (value: Int): Void {
    Cts.client.ssend([
      "source" => Js.ws("PictTime"),
      "rq" => Js.ws("changePictTime"),
      "value" => Js.wi(value)
    ], rp -> {
      content
        .removeAll()
        .add(Q("table")
          .add(Q("tr")
            .add(Q("td")
              .klass("frame")
              .style("font-size: 40px")
              .html("&nbsp;" + Std.string(value) + "&nbsp"))))
      ;

      wg.setStyle("opacity", "1");
      haxe.Timer.delay(() -> {
        wg.setStyle("opacity", "0");
      }, 1500);
    });
  }
}


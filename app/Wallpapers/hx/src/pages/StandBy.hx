// Copyright 22-May-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pages;

import js.html.KeyboardEvent;
import haxe.Timer;
import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import data.Pict;
import I18n._;

/// Stand by page.
class StandBy {
  final div: Domo;
  final wg: Domo;
  final fnBack: Void -> Void;

  public function new (wg: Domo, fnBack: Void -> Void
  ) {
    this.wg = wg;
    this.fnBack = fnBack;

    final visuals = Media.visuals();

    div = visuals.div
      .setStyle("background-color", "#000000")
      .on(CLICK, goBack)
    ;
    visuals.img.setStyle("opacity", "0");

    Q("@body")
      .on(KEYDOWN, e -> keyDown(e));

    view();

    div.e.requestFullscreen();
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
  }
}

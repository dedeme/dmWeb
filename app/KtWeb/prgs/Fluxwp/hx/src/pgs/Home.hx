// Copyright 05-Jan-2023 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.ModalBox;
import I18n._;

/// Home page.
class Home {
  final miniWidth = 200;
  final maxWidth = 800;
  final cols = 5;
  final wg: Domo;
  final isPicture: Bool;
  final picts: Array<String>;
  final popup: ModalBox;
  final popupWg = Q("div");
  final pPath: String;

  function new (wg: Domo, isPicture: Bool, picts: Array<String>) {
    this.wg = wg;
    this.isPicture = isPicture;
    this.picts = picts;
    popup = new ModalBox(popupWg, false);
    pPath = isPicture ? "img/picts" : "img/trash";
  }

  // View ----------------------------------------------------------------------

  public function show (): Void {
    final picts2 = new Array<Array<String>>();
    var i = 0;
    while (true) {
      final row = [];
      for (j in 0...cols) {
        if (i < picts.length) row.push(picts[i++]);
        else row.push("");
      }
      picts2.push(row);
      if (i >= picts.length) break;
    }
    wg
      .removeAll()
      .add(Q("table")
        .klass("main")
        .adds(picts2.map(row -> Q("tr").adds(row.map(p -> mkTd(p))))))
      .add(popup.wg)
    ;
  }

  function mkTd(pict: String) {
    if (pict == "") return Q("td");

    final imgPath = pPath + "/" + pict + "?t=" + Date.now().getTime();
    function onClick () {
      final img = Q("img")
        .att("src", imgPath)
        .att("title", pict)
        .att("border", 1)
        .att("style", "cursor:pointer")
        .on(CLICK, () -> popup.show(false))
      ;
      img.on(LOAD, ev -> img.att("width", maxWidth));

      popupWg
        .removeAll()
        .add(img)
      ;
      popup.show(true);
    }

    final img = Q("img")
      .att("src", imgPath)
      .att("title", pict)
      .att("border", 1)
      .att("style", "cursor:pointer")
      .on(CLICK, onClick)
    ;
    img.on(LOAD, ev -> img.att("width", miniWidth));

    return Q("td")
      .add(Q("table")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td")
            .att("colspan", 3)
            .add(img))))
    ;
  }

  // Control -------------------------------------------------------------------


  // Static --------------------------------------------------------------------

  public static function mk (wg: Domo, isPicture: Bool): Void {
    Global.client.send([
      "prg" => Js.ws("Fluxwp"),
      "source" => Js.ws("Home"),
      "rq" => Js.ws("idata"),
      "isPicture" => Js.wb(isPicture)
    ], rp -> {
      final picts = rp["picts"].ra().map(e -> e.rs());
      new Home(wg, isPicture, picts).show();
    });
  }
}

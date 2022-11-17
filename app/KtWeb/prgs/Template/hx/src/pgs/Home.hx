// Copyright 01-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import I18n._;
import data.Conf;

/// Home page.
class Home {
  final wg: Domo;
  final area: Domo;
  final color: String;
  final text: String;

  function new (wg: Domo, color: String, text: String) {
    this.wg = wg;
    this.color = color;
    this.text = text;
    area = Q("textarea")
      .att("cols", 50)
      .att("rows", 6)
      .style("background-color:" + color)
      .value(text)
    ;
  }

  // View ----------------------------------------------------------------------

  public function show (): Void {
    final btCancel = Q("button")
      .text(_("Delete"))
      .on(CLICK, delete)
    ;

    final btModify = Q("button")
      .text(_("Modify"))
      .on(CLICK, modify)
    ;

    wg
      .removeAll()
      .add(Q("table")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td")
            .add(area)))
        .add(Q("tr")
          .add(Q("td")
            .style("text-align: right")
            .add(Q("span")
              .add(btCancel))
            .add(Q("span")
              .html("&nbsp;&nbsp;"))
            .add(Q("span")
              .add(btModify))
              )))
    ;
  }

  // Control -------------------------------------------------------------------

  function delete() {
    Global.client.ssend([
      "prg" => Js.ws("Template"),
      "source" => Js.ws("Home"),
      "rq" => Js.ws("delete"),
    ], rp -> {
      mk(wg);
    });
  }

  function modify() {
    Global.client.ssend([
      "prg" => Js.ws("Template"),
      "source" => Js.ws("Home"),
      "rq" => Js.ws("modify"),
      "text" => Js.ws(area.getValue())
    ], rp -> {
      mk(wg);
    });
  }


  // Static --------------------------------------------------------------------

  public static function mk (wg: Domo): Void {
    Global.client.send([
      "prg" => Js.ws("Template"),
      "source" => Js.ws("Home"),
      "rq" => Js.ws("idata")
    ], rp -> {
      final conf = Conf.fromJs(rp["conf"]);
      final text = rp["text"].rs();
      new Home(wg, conf.color, text).show();
    });
  }
}

// Copyright 05-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.settings.acc;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.It;
import dm.Dec;
import dm.Menu;
import data.Cts;
import data.Annotation;
import I18n._;
import pgs.settings.acc.wgs.Annotations;

/// All investors (managers) annotations page.
class All {
  var wg: Domo;
  var investors: Int;
  var years: Array<String>;
  var year: String;
  var anns: Array<Annotation>;
  var cash: Float;

  final body = Q("div");

  /// Constructor
  ///   wg   : Container.
  ///   years: Years with annotations.
  ///   anns : Diary annotations.
  ///   cash : Final cash value.
  public function new (
    wg: Domo, years: Array<String>, anns: Array<Annotation>, cash: Float
  ) {
    this.wg = wg;
    this.years = years;
    this.year = years[0];
    this.anns = anns;
    this.cash = cash;

    view();
  }

  // View ----------------------------------------------------------------------

  function view () {
    final lopts = [];
    It.from(years).eachIx((year, i) -> {
      if (i > 0) {
        lopts.push(Menu.separator());
      }
      lopts.push(Menu.toption(year, year, () -> changeYear(year)));
    });
    final menu = new Menu(lopts, [], year);

    final annsWg = Q("div");
    new Annotations(annsWg, anns, None);

    wg
      .removeAll()
      .add(menu.wg)
      .add(Q("div")
        .add(Q("div")
          .klass("head")
          .html(_("Annotations")))
        .add(Q("table")
          .att("align", "center")
          .klass("frame3")
          .add(Q("tr")
            .add(Q("td")
              .add(Q("table").att("align", "right")
                .add(Q("tr")
                  .add(Q("td")
                    .klass("rlabel")
                    .add(Q("span")
                      .html(_("Cash:"))))
                  .add(Q("td")
                    .klass("number")
                    .text(Dec.toIso(cash, 2)))
                  .add(Q("td"))))))
          .add(Q("tr")
            .add(Q("td").klass("frame")
              .add(annsWg)))))
    ;
  }

  // Control -------------------------------------------------------------------

  function changeYear (y: String) {
    Cts.client.ssend([
      "module" => Js.ws("settings"),
      "source" => Js.ws("acc/all"),
      "rq" => Js.ws("anns"),
      "year" => Js.ws(y)
    ], rp -> {
      anns = rp["anns"].ra().map(e -> Annotation.fromJs(e));
      cash = rp["cash"].rf();
      year = y;
      view();
    });
  }
}

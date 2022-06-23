// Copyright 12-Nov-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.settings.acc;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.It;
import dm.Dec;
import dm.Dt;
import dm.Opt;
import dm.Menu;
import dm.DatePicker;
import data.Annotation;
import I18n._;
import pgs.settings.acc.wgs.Annotations;

/// All investors (managers) annotations page.
class All {
  var wg: Domo;
  var investors: Int;
  var years: Array<String>;
  var year: String;
  var lastDate: String;
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
    lastDate = Dt.to(Date.now());
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

    final lastDateDiv = Q("div");

    var anns2 = anns.copy();
    if (year == years[0]) {
      anns2 = It.from(anns).filter(a -> a.date <= lastDate).to();
    }
    final annsWg = Q("div");
    new Annotations(annsWg, anns2, None);

    wg
      .removeAll()
      .add(menu.wg)
      .add(Q("div")
        .add(Q("div")
          .klass("head")
          .html(_("Annotations")))
        .add(lastDateDiv)
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

    if (year == years[0]) {
      final dp = new DatePicker();
      dp.lang = I18n.lang;
      dp.date = Opt.eget(Dt.from(lastDate));
      dp.action = s -> {};
      final input = Q("input")
        .att("type", "text")
        .style("text-align:center;width:166px")
      ;
      final button = Q("button")
        .text(_("Change"))
        .on(CLICK, e -> changeLastDate(input))
      ;
      lastDateDiv
        .style("text-align: center")
        .add(Q("table")
          .att("align", "center")
          .add(Q("tr")
            .add(Q("td")
              .style("width:120px;text-align:right")
              .text(_("Up to") + ": "))
            .add(Q("td")
              .add(dp.mkText(input)))
            .add(Q("td")
              .style("width:120px;text-align:left")
              .add(button))))
      ;
    }
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
      lastDate = Dt.to(Date.now());
      view();
    });
  }

  function changeLastDate (input: Domo) {
    var d = cast(input.getValue(), String);
    if (d == "") {
      d = Dt.toIso(Date.now());
    }
    lastDate = Dt.to(
      Opt.eget(I18n.lang == "es" ? Dt.fromIso(d): Dt.fromEn(d))
    );

    Cts.client.ssend([
      "module" => Js.ws("settings"),
      "source" => Js.ws("acc/all"),
      "rq" => Js.ws("cashUpTo"),
      "date" => Js.ws(lastDate)
    ], rp -> {
      cash = rp["cash"].rf();
      view();
    });
  }

}

// Copyright 14-Nov-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.models.ranges;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.It;
import dm.B64;
import dm.Dec;
import dm.Menu;
import data.model.Model;
import data.Cts;
import data.model.Rank;
import wgs.Table; // import Col
import I18n._;

/// Rankings of model with only one parameter.
class Rankings {
  var wg: Domo;
  var cols: Array<Col>;
  var rankings: Array<Rank>;
  var link: (Dynamic, Int) -> String;
  var selSubmenu: String;

  /// Constructor
  ///   wg      : Container.
  ///   cols    : Tables header.
  ///   rankings: Tables data.
  ///   Link    : Link constructor from table element and column index.
  public function new (
    wg: Domo,
    cols: Array<Col>,
    rankings: Array<Rank>,
    link: (Dynamic, Int) -> String
  ) {
    this.wg = wg;
    this.cols = cols;
    this.rankings = rankings;
    this.link = link;
    selSubmenu = rankings.length > 0
      ? dateFormat(rankings[0].date)
      : ""
    ;

    view();
  }

  // View ----------------------------------------------------------------------

  function view () {
    final ranks = rankings;
    final len = ranks.length;

    if (len == 0) {
      this.wg
        .removeAll()
        .add(Q("table")
          .att("align", "center")
          .add(Q("tr")
            .add(Q("td")
              .klass("frame")
              .html(_("Without data")))));
      return;
    }

    final wg = Q("div");

    final dt = dateFormat(ranks[0].date);
    final lopts = [Menu.toption(dt, dt, () -> show(dt))];
    var models = ranks[0].models;
    var prev = len > 1 ? ranks[1].models : [];
    for (i in 1...len) {
      lopts.unshift(Menu.separator());
      final dt = dateFormat(ranks[i].date);
      lopts.unshift(Menu.toption(dt, dt, () -> this.show(dt)));

      if (dt == selSubmenu) {
        models = ranks[i].models;
        prev = len > i + 1 ? ranks[i + 1].models : [];
      }
    }
    final ropts = [];
    final submenu = new Menu(lopts, ropts, selSubmenu);

    final table: Array<Array<Dynamic>> = [];
    It.from(models).eachIx((md, i) -> {
      final e = md.evaluation;
      final he = md.hevaluation;
      final name = "[" + md.qlevel + "] " + md.id;
      final r:Array<Dynamic> = [
        md, 0, icon(prev, md, i), name, e.assets, e.profitsAvg,
        e.value * 1000, e.buys, e.sales, md.param,
        he.value * 1000, he.sales
      ];
      table.push(r);
    });

    new Table(wg, cols, table, -1, link);

    this.wg
      .removeAll()
      .add(Q("table")
        .klass("main")
        .add(Q("tr")
          .add(Q("td")
            .add(submenu.wg)
            .add(wg)
            )))
    ;
  }

  // Control -------------------------------------------------------------------

  function show (option: String) {
    selSubmenu = option;
    view();
  }

  // Static --------------------------------------------------------------------

  public static function mk (wg: Domo, qlevel: Int, lcPath0: String): Void {
    Cts.client.send([
      "module" => Js.ws("models"),
      "source" => Js.ws("rankings"),
      "rq" => Js.ws("idata"),
      "qlevel" => Js.wi(qlevel)
    ], rp -> {
      final rankings = rp["rankings"].ra().map(e -> Rank.fromJs(e));

      final cols = mkHeaderBase();

      new Rankings(wg, cols, rankings, (md, i) ->
        (i == 2) ? chart(lcPath0, false, md) : chart(lcPath0, true, md)
      );
    });
  }

  /// Returns DD/MM from YYYYMMDD
  static function dateFormat (d: String): String {
    return d.substring(6) + "/" + d.substring(4, 6);
  }

  // Returns the name 'up-down' of an icon.
  //    prev: Previous ranking
  //    md  : Model to evaluate.
  //    i   : Index of 'md' in the current ranking.
  static function icon (prev: Array<Model>, md: Model, i: Int): String {
    final pi = It.from(prev).indexf(pmd -> pmd.eq(md));

    if (pi == -1) return "rk-new";
    final df = pi - i;
    if (df > 5) return "rk-up2";
    if (df > 0) return "rk-up";
    if (df == 0) return "rk-eq";
    if (df < -5) return "rk-down2";
    return "rk-down";
  }

  static function mkHeaderBase (): Array<Col> {
    return [
      new Col(_("Nº"), Col.COUNTER, 0, false, false),
      new Col("", Col.ICON, -1, true, false),
      new Col(_("Id"), Col.STRING, -1, true, false),
      new Col(_("Assets"), Col.NUMBER, 2, false, false),
      new Col(_("Pf. Avg"), Col.NUMBER, 4, false, false),
      new Col(_("Eval."), Col.NUMBER, 2, false, false),
      new Col(_("Buys"), Col.NUMBER, 0, false, false),
      new Col(_("Sales"), Col.NUMBER, 0, false, false),
      new Col(_("Jump"), Col.PARAM, 6, false, false),
      new Col(_("H. Eval."), Col.NUMBER, 2, false, false),
      new Col(_("H. Sales"), Col.NUMBER, 0, false, false)
    ];
  }

  // 'isSummary' is 'true' if chart to show is from assets summary.
  static function chart (
    lcPath0: String, isSummary: Bool, md: Model
  ): String {
    return '?models&${lcPath0}&charts' +
           '&${isSummary}&${B64.encode(md.toJs().to())}'
    ;
  }
}

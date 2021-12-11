// Copyright 09-Nov-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.ranking;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.It;
import dm.B64;
import dm.Dec;
import dm.Str;
import dm.Menu;
import data.Cts;
import data.model.Model;
import data.rank.RangeEntry;
import data.rank.TableEntry;
import data.rank.AvgTableEntry;
import wgs.Table; // import Col
import I18n._;

enum RankingType { RANGES; AVG; ALL; }

/// Investors ranking.
class Ranking {
  var wg: Domo;
  var cols: Array<Col>;
  var rangesTable: Array<RangeEntry>;
  var avgTable: Array<AvgTableEntry>;
  var table: Array<TableEntry>;
  var type: RankingType;
  var selDate: String;
  var rangesSorted: Bool;
  function new (
    wg: Domo, cols: Array<Col>,
    rangesTable: Array<RangeEntry>,
    avgTable: Array<AvgTableEntry>,
    table: Array<TableEntry>
  ) {
    this.wg = wg;
    this.cols = cols;
    this.rangesTable = rangesTable;
    this.avgTable = avgTable;
    this.table = table;
    type = RANGES;
    selDate = avgTable.length > 0
      ? dateFormat(avgTable[0].date)
      : ""
    ;
    rangesSorted = true;

    this.view();
  }

  // View ----------------------------------------------------------------------

  function view () {
    final len = switch (type) {
      case RANGES: rangesTable.length;
      case AVG: avgTable.length;
      case ALL: table.length;
    }
    if (len == 0) {
      wg
        .removeAll()
        .add(Q("table")
          .att("align", "center")
          .add(Q("tr")
            .add(Q("td")
              .klass("frame")
              .html(_("Without data")))));
      return;
    }

    final submenu = new Menu(
      [
        Menu.toption("ranges", _("Ranges"), () -> showBody(RANGES)),
        Menu.separator(),
        Menu.toption("avg", _("Average"), () -> showBody(AVG)),
        Menu.separator(),
        Menu.toption("all", _("All"), () -> showBody(ALL))
      ],
      [],
      switch(type) {
        case RANGES: "ranges";
        case AVG: "avg";
        case ALL: "all";
      }
    );

    switch(type) {
      case RANGES: rangesView(submenu);
      case AVG: avgView(submenu);
      case ALL: allView(submenu);
    }
  }

  function rangesView(submenu: Menu) {
    if (rangesSorted) {
      rangesTable.sort((e1, e2) -> e1.value < e2.value ? 1 : -1);
    } else {
      rangesTable.sort((e1, e2) -> e1.param > e2.param ? 1 : -1);
    }
    final trs = It.from(rangesTable).map(v ->
      Q("tr")
        .add(Q("td")
          .klass("fnumber")
          .text(Dec.toIso(v.param * 100, 1)))
        .add(Q("td")
          .klass("fnumber")
          .text(Dec.toIso(v.value * 1000, 2)))
        .add(Q("td")
          .add(Q("table")
            .add(Q("tr")
              .add(Q("td")
                .style("width:" + Std.int(v.value * 1000) + "px")
                .add(Q("hr")))
              .add(Q("td")))))
        .add(Q("td")
          .klass("fnumber")
          .text(Dec.toIso(v.sales, 0)))
    );
    this.wg
      .removeAll()
      .add(Q("table")
        .klass("main")
        .add(Q("tr")
          .add(Q("td")
            .add(submenu.wg)
            .add(Q("table")
              .att("align", "center")
              .add(Q("tr")
                .add(Q("td")
                  .att("colspan", "4")
                  .style("text-align:center")
                  .add(Ui.link(() -> sortRanges(!rangesSorted))
                    .klass("link")
                    .text(rangesSorted
                        ? _("Sort by parameter")
                        : _("Sort by value")
                      ))))
                .add(Q("td")
                  .att("colspan", "4")
                  .add(Q("hr")))
              .adds(trs)))))
    ;
  }

  function avgView (submenu: Menu) {
    function mkTr (rk: Array<Array<Model>>, prev: Array<Array<Model>>, i: Int) {
      final rs = rk[i];
      final param = rs[0].param;
      final eval = AvgTableEntry.evalRow(rs);
      final sales = AvgTableEntry.salesRow(rs);
      final pi = It.from(prev).indexf(e -> e[0].id == rs[0].id);
      return Q("tr")
        .add(Q("td")
          .add(Ui.img(
              pi == -1 ? "rk-new"
                : i < pi ? "rk-up"
                  : i == pi ? "rk-eq"
                    : "rk-down"
            )))
        .add(Q("td")
          .klass("fparam")
          .text(Dec.toIso(param * 100, 4)))
        .add(Q("td")
          .klass("fnumber")
          .text(Dec.toIso(eval * 1000, 2)))
        .add(Q("td")
          .klass("fnumber")
          .text(Dec.toIso(sales, 0)))
        .adds(rs.map(md ->
            Q("td")
              .klass("menu")
              .add(Q("a")
                .klass("link")
                .att("href", link(md, 1))
                .text(_("Qlevel") + "-" + md.qlevel))
              .add(Q("span")
                .text(" [" +
                    Dec.toIso(md.hevaluation.value * 1000, 2) + " | " +
                    Dec.toIso(md.hevaluation.sales, 0) + "]"
                  ))

          ))
      ;
    }

    final tb = avgTable;
    final len = tb.length;

    final dt = dateFormat(tb[0].date);
    final lopts = [Menu.toption(dt, dt, () -> show(dt))];
    var rk = tb[0].rowRank;
    var prev = len > 1 ? tb[1].rowRank : [];
    for (i in 1...len) {
      lopts.unshift(Menu.separator());
      final dt = dateFormat(tb[i].date);
      lopts.unshift(Menu.toption(dt, dt, () -> show(dt)));

      if (dt == selDate) {
        rk = tb[i].rowRank;
        prev = len > i + 1 ? tb[i + 1].rowRank : [];
      }
    }
    final ropts = [];
    final datesSubmenu = new Menu(lopts, ropts, selDate);


    this.wg
      .removeAll()
      .add(Q("table")
        .klass("main")
        .add(Q("tr")
          .add(Q("td")
            .add(submenu.wg)
            .add(datesSubmenu.wg)
            .add(Q("table")
              .klass("white")
              .att("align", "center")
              .add(Q("tr")
                .add(Q("td")
                  .klass("header"))
                .add(Q("td")
                  .klass("header")
                  .text(_("Param")))
                .add(Q("td")
                  .klass("header")
                  .text(_("Eval.")))
                .add(Q("td")
                  .klass("header")
                  .text(_("Sales")))
                .add(Q("td")
                  .klass("header")
                  .att("colspan", rk[0].length)
                  .text(_("Models")))
                )
              .adds(It.range(rk.length).map(i -> mkTr(rk, prev, i)))
              ))))
    ;
  }

  function allView (submenu: Menu) {
    final tb = table;
    final len = tb.length;


    final wg = Q("div");

    final dt = dateFormat(tb[0].date);
    final lopts = [Menu.toption(dt, dt, () -> show(dt))];
    var rk = tb[0].modelRank;
    var prev = len > 1 ? tb[1].modelRank : [];
    for (i in 1...len) {
      lopts.unshift(Menu.separator());
      final dt = dateFormat(tb[i].date);
      lopts.unshift(Menu.toption(dt, dt, () -> show(dt)));

      if (dt == selDate) {
        rk = tb[i].modelRank;
        prev = len > i + 1 ? tb[i + 1].modelRank : [];
      }
    }
    final ropts = [];
    final datesSubmenu = new Menu(lopts, ropts, selDate);

    final tableWg: Array<Array<Dynamic>> = [];
    It.from(rk).eachIx((md, i) -> {
      final e = md.evaluation;
      final he = md.hevaluation;
      final name = "[" + md.qlevel + "] " + md.id;
      final r: Array<Dynamic> = [
        md, 0, icon(prev, md, i), name, e.assets, e.profitsAvg,
        e.value * 1000, e.buys, e.sales,
        Fns.nformat(md.param, Cts.paramDecs),
        he.value * 1000, he.sales
      ];
      tableWg.push(r);
    });

    new Table(wg, cols, tableWg, -1, (e, i) -> link(e, i));

    this.wg
      .removeAll()
      .add(Q("table")
        .klass("main")
        .add(Q("tr")
          .add(Q("td")
            .add(submenu.wg)
            .add(datesSubmenu.wg)
            .add(wg))))
    ;
  }

  // Control -------------------------------------------------------------------

  function show (option: String): Void {
    selDate = option;
    view();
  }

  function showBody (type: RankingType): Void {
    this.type = type;
    view();
  }

  function sortRanges (value: Bool): Void {
    this.rangesSorted = value;
    view();
  }

  function link (md: Model, i: Int): String {
    return '?models&${md.qlevel}&charts&${i == 1}&' +
           '${B64.encode(md.toJs().to())}'
    ;
  }

  // Static --------------------------------------------------------------------

  /// Constructor
  ///   wg: Container.
  public static function mk (wg: Domo) {
    wg
      .removeAll()
      .add(Q("table")
        .klass("main")
        .add(Q("tr")
          .add(Q("td")
            .style("text-align:center")
            .add(Ui.Q("img")
              .att("src", "img/wait2.gif")
              .klass("frame")))))
    ;
    Cts.client.send([
      "module" => Js.ws("ranking"),
      "rq" => Js.ws("idata"),
    ], rp -> {
      final rangesTable = rp["rangesTable"].ra().map(e -> RangeEntry.fromJs(e));
      final avgTable = rp["avgTable"].ra().map(e -> AvgTableEntry.fromJs(e));
      final table = rp["table"].ra().map(e -> TableEntry.fromJs(e));

      final cols = [
        new Col(_("Nº"), Col.COUNTER, 0, false, false),
        new Col("", Col.ICON, -1, true, false),
        new Col(_("Id"), Col.STRING, -1, true, false),
        new Col(_("Assets"), Col.NUMBER, 2, false, false),
        new Col(_("Pf. Avg"), Col.NUMBER, 4, false, false),
        new Col(_("Eval."), Col.NUMBER, 2, false, false),
        new Col(_("Buys"), Col.NUMBER, 0, false, false),
        new Col(_("Sells"), Col.NUMBER, 0, false, false),
        new Col(_("Param."), Col.P_STRING, -1, false, false),
        new Col(_("H. Eval."), Col.NUMBER, 2, false, false),
        new Col(_("H. Sales"), Col.NUMBER, 0, false, false)
      ];

      new Ranking(wg, cols, rangesTable, avgTable, table);
    });
  }

  // Returns a date in format DD/MM from a date in format YYYYMMDD.
  static function dateFormat (d: String): String {
    return d.substring(6) + "/" + d.substring(4, 6);
  }

  // Return icon name.
  function icon (prev: Array<Model>, md: Model, i: Int): String {
    final pi = It.from(prev).indexf(pmd -> pmd.eq(md));

    if (pi == -1) return "rk-new";
    final df = pi - i;
    if (df > 5) return "rk-up2";
    if (df > 0) return "rk-up";
    if (df == 0) return "rk-eq";
    if (df < -5) return "rk-down2";
    return "rk-down";
  }

}

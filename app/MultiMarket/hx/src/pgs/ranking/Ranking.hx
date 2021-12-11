// Copyright 15-Sep-2020 ºDeme
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
import data.flea.Irank;
import data.flea.Investor;
import data.flea.JumpResult;
import data.flea.JumpRanking;
import wgs.Table; // import Col
import I18n._;

/// Investors ranking.
class Ranking {
  var wg: Domo;
  var cols: Array<Col>;
  var jranking: Array<JumpRanking>;
  var ranking: Array<Irank>;
  var isJumps: Bool;
  var selSubmenu: String;
  function new (
    wg: Domo, cols: Array<Col>,
    jranking: Array<JumpRanking>, ranking: Array<Irank>
  ) {
    this.wg = wg;
    this.cols = cols;
    this.jranking = jranking;
    this.ranking = ranking;
    isJumps = true;
    selSubmenu = jranking.length > 0
      ? dateFormat(ranking[0].date)
      : ""
    ;

    this.view();
  }

  // View ----------------------------------------------------------------------

  function view () {
    final len = isJumps ? jranking.length : ranking.length;
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

    final jsubmenu = new Menu(
      [
        Menu.toption("jumps", _("Jumps"), () -> showJumps(true)),
        Menu.separator(),
        Menu.toption("all", _("All"), () -> showJumps(false))
      ],
      [],
      isJumps ? "jumps" : "all"
    );

    if (isJumps) {
      jumpsView(jsubmenu);
    } else {
      allView(jsubmenu);
    }
  }

  function jumpsView (jsubmenu: Menu) {
    function mkTr (rk: Array<JumpResult>, prev: Array<JumpResult>, i: Int) {
      final rs = rk[i];
      final pi = It.from(prev).indexf(e ->
        Dec.eq(e.param, rs.param, 0.0000001)
      );
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
          .text(Dec.toIso(rs.param * 100, 4)))
        .add(Q("td")
          .klass("fnumber")
          .text(Dec.toIso(rs.eval * 1000, 2)))
        .add(Q("td")
          .klass("fnumber")
          .text(Dec.toIso(rs.sales, 0)))
        .adds(rs.investors.map(inv ->
            Q("td")
              .klass("menu")
              .add(Q("a")
                .klass("link")
                .att("href", link(inv, 1))
                .text(inv.model.id))
              .add(Q("span")
                .text(" [" +
                    Dec.toIso(inv.eflea.historicEval * 1000, 2) + ", " +
                    Dec.toIso(inv.eflea.historicSales, 0) + "]"
                  ))

          ))
      ;
    }

    final rank = jranking;
    final len = rank.length;

    final dt = dateFormat(rank[0].date);
    final lopts = [Menu.toption(dt, dt, () -> show(dt))];
    var rk = rank[0].ranking;
    var prev = len > 1 ? rank[1].ranking : [];
    for (i in 1...len) {
      lopts.unshift(Menu.separator());
      final dt = dateFormat(rank[i].date);
      lopts.unshift(Menu.toption(dt, dt, () -> show(dt)));

      if (dt == selSubmenu) {
        rk = rank[i].ranking;
        prev = len > i + 1 ? rank[i + 1].ranking : [];
      }
    }
    final ropts = [];
    final submenu = new Menu(lopts, ropts, selSubmenu);


    this.wg
      .removeAll()
      .add(Q("table")
        .klass("main")
        .add(Q("tr")
          .add(Q("td")
            .add(jsubmenu.wg)
            .add(submenu.wg)
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
                  .att("colspan", rk[0].investors.length)
                  .text(_("Investors")))
                )
              .adds(It.range(rk.length).map(i -> mkTr(rk, prev, i)))
              ))))
    ;
  }

  function allView (jsubmenu: Menu) {
    final rank = ranking;
    final len = rank.length;


    final wg = Q("div");

    final dt = dateFormat(rank[0].date);
    final lopts = [Menu.toption(dt, dt, () -> show(dt))];
    var invs = rank[0].invs;
    var prev = len > 1 ? rank[1].invs : [];
    for (i in 1...len) {
      lopts.unshift(Menu.separator());
      final dt = dateFormat(rank[i].date);
      lopts.unshift(Menu.toption(dt, dt, () -> show(dt)));

      if (dt == selSubmenu) {
        invs = rank[i].invs;
        prev = len > i + 1 ? rank[i + 1].invs : [];
      }
    }
    final ropts = [];
    final submenu = new Menu(lopts, ropts, selSubmenu);

    final table: Array<Array<Dynamic>> = [];
    It.from(invs).eachIx((inv, i) -> {
      final e = inv.eflea;
      final model = Str.left(inv.name, inv.name.indexOf("-") + 1);
      final name = inv.model.id + "-" + e.flea.name;
      final r: Array<Dynamic> = [
        inv, 0, icon(prev, inv, i), name, e.assets, e.profitsAvg,
        e.profitsVa, e.eval * 1000, e.buys, e.sales,
        Cts.nformat(e.flea.param, Cts.paramDecs),
        e.historicEval * 1000, e.historicSales
      ];
      table.push(r);
    });

    new Table(wg, cols, table, -1, (e, i) -> link(e, i));

    this.wg
      .removeAll()
      .add(Q("table")
        .klass("main")
        .add(Q("tr")
          .add(Q("td")
            .add(jsubmenu.wg)
            .add(submenu.wg)
            .add(wg))))
    ;
  }

  // Control -------------------------------------------------------------------

  function show (option: String): Void {
    selSubmenu = option;
    view();
  }

  function showJumps (isJumps: Bool): Void {
    this.isJumps = isJumps;
    view();
  }

  function link (inv: Investor, i: Int): String {
    return '?fleas&${inv.model.id}&charts&${i == 1}&' +
           '${B64.encode(inv.eflea.toJs().to())}'
    ;
  }

  // Static --------------------------------------------------------------------

  /// Constructor
  ///   wg: Container.
  public static function mk (wg: Domo) {
    Cts.client.send([
      "module" => Js.ws("ranking"),
      "source" => Js.ws("ranking"),
    ], rp -> {
      final jranking = rp["jranking"].ra().map(e -> JumpRanking.fromJs(e));
      final ranking = rp["ranking"].ra().map(e -> Irank.fromJs(e));

      final cols = [
        new Col(_("Nº"), Col.COUNTER, 0, false, false),
        new Col("", Col.ICON, -1, true, false),
        new Col(_("Id"), Col.STRING, -1, true, false),
        new Col(_("Assets"), Col.NUMBER, 2, false, false),
        new Col(_("Pf. Avg"), Col.NUMBER, 4, false, false),
        new Col(_("Pf. Var"), Col.NUMBER, 4, false, false),
        new Col(_("Eval."), Col.NUMBER, 2, false, false),
        new Col(_("Buys"), Col.NUMBER, 0, false, false),
        new Col(_("Sells"), Col.NUMBER, 0, false, false),
        new Col(_("Param."), Col.P_STRING, -1, false, false),
        new Col(_("H. Eval."), Col.NUMBER, 2, false, false),
        new Col(_("H. Sales"), Col.NUMBER, 0, false, false)
      ];

      new Ranking(wg, cols, jranking, ranking);
    });
  }

  // Returns a date in format DD/MM from a date in format YYYYMMDD.
  static function dateFormat (d: String): String {
    return d.substring(6) + "/" + d.substring(4, 6);
  }

  // Return icon name.
  function icon (prev: Array<Investor>, inv: Investor, i: Int): String {
    final name = inv.model.id + "-" + inv.eflea.flea.name;
    var pi = -1;
    for (j in 0...prev.length) {
      final inv2 = prev[j];
      if (inv2.model.id + "-" + inv2.eflea.flea.name == name) {
        pi = j;
        break;
      }
    }

    if (pi == -1) return "rk-new";
    final df = pi - i;
    if (df > 5) return "rk-up2";
    if (df > 0) return "rk-up";
    if (df == 0) return "rk-eq";
    if (df < -5) return "rk-down2";
    return "rk-down";
  }

}

// Copyright 15-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.rangesPlus;

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
import wgs.Table; // import Col
import I18n._;

/// Investors ranges +.
class RangesPlus {
  var wg: Domo;
  var cols: Array<Col>;
  var ranking: Array<Irank>;
  var selSubmenu: String;
  function new (wg: Domo, cols: Array<Col>, ranking: Array<Irank>) {
    this.wg = wg;
    this.cols = cols;
    this.ranking = ranking;
    selSubmenu = ranking.length > 0
      ? dateFormat(ranking[0].date)
      : ""
    ;

    this.view();
  }

  // View ----------------------------------------------------------------------

  function view () {
    final rank = ranking;
    final len = rank.length;

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
      final parDecs = inv.model.parDecs;
      final e = inv.eflea;
      final model = Str.left(inv.name, inv.name.indexOf("-") + 1);
      final name = e.flea.cycle + "-" + e.flea.id + "-" + e.flea.date;
      final parValues = e.flea.params;
      final r: Array<Dynamic> = [
        inv, 0, icon(prev, inv, i), name, e.assets, e.profitsAvg,
        e.profitsVa, e.ev * 1000, e.buys, e.sells
      ];
      final len = r.length - 1;
      for (i in 0...cols.length-len) {
        r.push(i < parValues.length
          ? Cts.nformat(parValues[i], parDecs[i])
          : ""
        );
      }
      table.push(r);
    });

    new Table(wg, cols, table, 3, (e, i) -> link(e, i));

    this.wg
      .removeAll()
      .add(Q("table")
        .klass("main")
        .add(Q("tr")
          .add(Q("td")
            .add(submenu.wg)
            .add(wg))))
    ;
  }

  // Control -------------------------------------------------------------------

  function show (option: String) {
    selSubmenu = option;
    view();
  }

  function link (inv: Investor, i: Int) {
    return '?fleas&${inv.model.id}&charts&${i == 1}&' +
           '${B64.encode(inv.eflea.toJs().to())}'
    ;
  }

  // Static --------------------------------------------------------------------

  /// Constructor
  ///   wg: Container.
  public static function mk (wg: Domo) {
    Cts.client.send([
      "module" => Js.ws("rangesPlus"),
      "source" => Js.ws("rangesPlus"),
    ], rp -> {
      final ranking = rp["ranking"].ra().map(e -> Irank.fromJs(e));

      final nPar: Int = It.from(ranking).reduce(
        0,
        (r, irk) -> {
          final n = It.from(irk.invs).reduce(
            0,
            (r, inv) -> {
              final n = inv.eflea.flea.params.length;
              return n > r ? n : r;
            }
          );
          return n > r ? n : r;
        }
      );

      final cols = [
        new Col(_("Nº"), Col.COUNTER, 0, false, false),
        new Col("", Col.ICON, -1, true, false),
        new Col(_("Id"), Col.STRING, -1, true, false),
        new Col(_("Assets"), Col.NUMBER, 2, false, false),
        new Col(_("Pf. Avg"), Col.NUMBER, 4, false, false),
        new Col(
          _("Pf. Var"), Col.NUMBER, 4,
          false, false
        ),
        new Col(
          _("Eval."), Col.NUMBER, 2,
          false, false
        ),
        new Col(
          _("Buys"), Col.NUMBER, 0,
          false, false
        ),
        new Col(
          _("Sells"), Col.NUMBER, 0,
          false, false
        ),
        new Col(_("Param."), Col.P_STRING, -1, false, false)
      ];

      new RangesPlus(wg, cols, ranking);
    });
  }

  // Returns a date in format DD/MM from a date in format YYYYMMDD.
  static function dateFormat (d: String): String {
    return d.substring(6) + "/" + d.substring(4, 6);
  }

  // Return icon name.
  function icon (prev: Array<Investor>, inv: Investor, i: Int): String {
    final name = Dec.toIso(inv.eflea.flea.params[0] * 100, 4);
    var pi = -1;
    for (j in 0...prev.length) {
      if (Dec.toIso(prev[j].eflea.flea.params[0] * 100, 4) == name) {
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
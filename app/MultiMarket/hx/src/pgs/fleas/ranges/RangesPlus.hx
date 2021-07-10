// Copyright 08-Jul-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.fleas.ranges;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.It;
import dm.B64;
import dm.Dec;
import dm.Menu;
import data.flea.Fmodel;
import data.Cts;
import data.flea.Frank;
import data.flea.Eflea;
import wgs.Table; // import Col
import I18n._;

/// Ranking of ranges of models with only one parameter.
class RangesPlus {
  var wg: Domo;
  var cols: Array<Col>;
  var rankings: Array<Frank>;
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
    rankings: Array<Frank>,
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
    var efleas = ranks[0].efleas;
    var prev = len > 1 ? ranks[1].efleas : [];
    for (i in 1...len) {
      lopts.unshift(Menu.separator());
      final dt = dateFormat(ranks[i].date);
      lopts.unshift(Menu.toption(dt, dt, () -> this.show(dt)));

      if (dt == selSubmenu) {
        efleas = ranks[i].efleas;
        prev = len > i + 1 ? ranks[i + 1].efleas : [];
      }
    }
    final ropts = [];
    final submenu = new Menu(lopts, ropts, selSubmenu);

    final table: Array<Array<Dynamic>> = [];
    It.from(efleas).eachIx((e, i) -> {
      final name = e.flea.cycle + "-" + e.flea.id;
      final r:Array<Dynamic> = [
        e, 0, icon(prev, e, i), name, e.assets, e.profitsAvg,
        e.profitsVa, e.ev * 1000, e.buys, e.sells
      ];
      for (g in e.flea.params) r.push(g);
      table.push(r);
    });

    new Table(wg, cols, table, 3, link);

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

  public static function mk (wg: Domo, md: Fmodel, lcPath0: String): Void {
    if (md.parNames.length != 1) {
      wg
        .removeAll()
        .add(Q("table")
          .att("align", "center")
          .add(Q("tr")
            .add(Q("td")
              .klass("frame")
              .text(
                  _("Option only available for models with only one parameter")
                ))))
      ;
      return;
    }
    Cts.client.send([
      "module" => Js.ws("fleas"),
      "source" => Js.ws("ranges+"),
      "rq" => Js.ws("idata"),
      "modelId" => Js.ws(md.id)
    ], rp -> {
      final rankings = rp["rankings"].ra().map(e -> Frank.fromJs(e));
      final parName = rp["parName"].rs();

      final cols = mkHeaderBase();
      cols.insert(1, new Col(
        "", Col.ICON, -1, true, false
      ));

      cols.push(new Col(parName, Col.PARAM, 6, false, false));

      new RangesPlus(wg, cols, rankings, (e, i) ->
        (i == 2) ? chart(lcPath0, false, e) : chart(lcPath0, true, e)
      );
    });
  }

  /// Returns DD/MM from YYYYMMDD
  static function dateFormat (d: String): String {
    return d.substring(6) + "/" + d.substring(4, 6);
  }

  // Returns the name 'up-down' of an icon.
  //    prev: Previous ranking
  //    e   : Eflea to evaluate.
  //    i   : Index of 'e' in the current ranking.
  static function icon (prev: Array<Eflea>, e: Eflea, i: Int): String {
    final name = Dec.toIso(e.flea.params[0] * 100, 4);
    var pi = -1;
    for (j in 0...prev.length) {
      if (Dec.toIso(prev[j].flea.params[0] * 100, 4) == name) {
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

  static function mkHeaderBase (): Array<Col> {
    return [
      new Col(_("Nº"), Col.COUNTER, 0, false, false),
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
      )
    ];
  }

  // 'isSummary' is 'true' if chart to show is from assets summary.
  static function chart (
    lcPath0: String, isSummary: Bool, eflea: Eflea
  ): String {
    return '?fleas&${lcPath0}&charts' +
           '&${isSummary}&${B64.encode(eflea.toJs().to())}'
    ;
  }
}

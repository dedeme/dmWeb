// Copyright 14-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.fleas.ranking;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Ui;
import dm.It;
import dm.Menu;
import wgs.Table; // import Col
import data.flea.Frank;
import data.flea.Eflea;
import I18n._;

/// Model ranking page.
class Ranking {
  var wg: Domo;
  var cols: Array<Col>;
  var ranking: Array<Frank>;
  var link: (Dynamic, Int) -> String;
  var selSubmenu: String;

  /// Constructor
  ///   wg     : Container.
  ///   cols   : Tables header.
  ///   ranking: Tables data.
  ///   Link   : Link constructor from table element and column index.
  public function new (wg, cols, ranking, link) {
    this.wg = wg;
    this.cols = cols;
    this.ranking = ranking;
    this.link = link;
    selSubmenu = ranking.length > 0
      ? dateFormat(ranking[0].date)
      : ""
    ;

    view();
  }

  // View ----------------------------------------------------------------------

  function view () {
    final rank = ranking;
    final len = rank.length;

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

    final dt = dateFormat(rank[0].date);
    final lopts = [Menu.toption(dt, dt, () -> show(dt))];
    var efleas = rank[0].efleas;
    var prev = len > 1 ? rank[1].efleas : [];
    for (i in 1...len) {
      lopts.unshift(Menu.separator());
      final dt = dateFormat(rank[i].date);
      lopts.unshift(Menu.toption(dt, dt, () -> this.show(dt)));

      if (dt == selSubmenu) {
        efleas = rank[i].efleas;
        prev = len > i + 1 ? rank[i + 1].efleas : [];
      }
    }
    final ropts = [];
    final submenu = new Menu(lopts, ropts, selSubmenu);

    final table: Array<Array<Dynamic>> = [];
    It.from(efleas).eachIx((e, i) -> {
      final r:Array<Dynamic> = [
        e, 0, icon(prev, e, i), e.flea.name, e.assets, e.profitsAvg,
        e.profitsVa, e.ev * 1000, e.buys, e.sells
      ];
      for (g in e.flea.params) r.push(g);
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

  /// Returns DD/MM from YYYYMMDD
  static function dateFormat (d: String): String {
    return d.substring(6) + "/" + d.substring(4, 6);
  }

  // Returns the name 'up-down' of an icon.
  //    prev: Previous ranking
  //    e   : Eflea to evaluate.
  //    i   : Index of 'e' in the current ranking.
  static function icon (prev: Array<Eflea>, e: Eflea, i: Int): String {
    final name = e.flea.name;
    var pi = -1;
    for (j in 0...prev.length) {
      if (prev[j].flea.name == name) {
        pi = j;
        break;
      }
    }

    if (pi == -1) return "rk-new";
    final df = pi - i;
    if (df > 2) return "rk-up2";
    if (df > 0) return "rk-up";
    if (df == 0) return "rk-eq";
    if (df < -2) return "rk-down2";
    return "rk-down";
  }
}

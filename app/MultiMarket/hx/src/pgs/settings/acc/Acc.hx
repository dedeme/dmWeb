// Copyright 03-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.settings.acc;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.Menu;
import data.Cts;
import data.Annotation;
import I18n._;

/// Main accounting annotations page.
class Acc {
  var wg: Domo;
  var investors: Int;
  var years: Array<String>;
  var anns: Array<Annotation>;
  var cash: Float;

  var menuSel = "all";

  final body = Q("div");

  function new (
    wg: Domo, investors: Int, years: Array<String>, anns: Array<Annotation>,
    cash: Float
  ) {
    this.wg = wg;
    this.investors = investors;
    this.years = years;
    this.anns = anns;
    this.cash = cash;

    all();
  }

  // View --------------------------------------------------------------------

  function view() {
    final lopts = [
      Menu.toption("all", _("All"), () -> Acc.mk(wg))
    ];
    for (i in 0...investors) {
      final lb = '${_("Inv")}-${i}';
      lopts.push(Menu.separator());
      lopts.push(Menu.toption(lb, lb, () -> investor(i)));
    }
    final menu = new Menu(lopts, [], menuSel);
    wg
      .removeAll()
      .add(menu.wg)
      .add(body)
    ;
  }

  // Control -------------------------------------------------------------------

  function all () {
    new All(body, years, anns, cash);
    menuSel = "all";
    view();
  }

  function investor (i: Int) {
    Editor.mk(body, years[0], i);
    menuSel = '${_("Inv")}-${i}';
    view();
  }

  // Static --------------------------------------------------------------------

  /// Constructor
  ///   wg: Container.
  public static function mk (wg: Domo) {
    Cts.client.ssend([
      "module" => Js.ws("settings"),
      "source" => Js.ws("acc"),
      "rq" => Js.ws("idata")
    ], rp -> {
      final investors = rp["investors"].ri();
      final years = rp["years"].ra().map(e -> e.rs());
      final anns = rp["anns"].ra().map(e -> Annotation.fromJs(e));
      final cash = rp["cash"].rf();

      new Acc(wg, investors, years, anns, cash);
    });
  }

}

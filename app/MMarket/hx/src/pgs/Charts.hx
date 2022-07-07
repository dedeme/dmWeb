// Copyright 21-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs;

import dm.Domo;
import dm.Ui.Q;
import dm.Menu;
import I18n._;

/// Entry of chart pages.
class Charts {
  final wg: Domo;
  final model: String;
  var chart: String;
  public function new (wg: Domo, model: String) {
    this.wg = wg;
    this.model = model;
    this.chart = "cos";
  }

  // View ----------------------------------------------------------------------

  public function show () {
    final lopts = [
      Menu.toption("cos", _("Companies"), () -> go("cos")),
      Menu.separator(),
      Menu.toption("historic", _("Historic"), () -> go("historic")),
      Menu.separator(),
      Menu.toption("operations", _("Operations"), () -> go("operations")),
      Menu.separator2(),
      Menu.toption("noLost", _("No Lost"), () -> go("noLost")),
    ];
    final menu = new Menu(lopts, [], chart);

    final body = Q("div");
    switch (chart) {
      case "cos": CosPg.mk(body, model);
      case "historic": HistoricPg.mk(body, model);
      case "noLost": NoLostPg.mk(body, model);
      default: OperationsPg.mk(body, model);
    }

    wg
      .removeAll()
      .add(menu.wg)
      .add(body)
    ;
  }

  // Control -------------------------------------------------------------------

  function go (chart: String) {
    this.chart = chart;
    show();
  }
}

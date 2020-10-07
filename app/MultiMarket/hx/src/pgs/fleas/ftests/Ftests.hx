// Copyright 10-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.fleas.ftests;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Menu;
import data.flea.Fmodel;
import I18n._;

/// Fleas test main page.
class Ftests {
  var wg: Domo;
  var model: Fmodel;
  var selSubmenu: String;
  public function new (wg: Domo, model: Fmodel) {
    this.wg = wg;
    this.model = model;
    selSubmenu = "references";

    view();
  }

  function view () {
    final wg = Q("div");

    final lopts = [
      Menu.toption("references", _("References"),
        () -> show("references")
      ),
      Menu.separator(),
      Menu.toption("orders", _("Orders"), () -> show("orders")),
      Menu.separator(),
      Menu.toption("selection", _("Selection"), () -> show("selection"))
    ];
    final ropts = [];
    final submenu = new Menu(lopts, ropts, selSubmenu);

    switch (selSubmenu) {
    case "references":
      References.mk(wg, model);
    case "orders":
      new Orders(wg, model);
    case "selection":
      Selection.mk(wg, model);
    default:
      References.mk(wg, model);
    }

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

}

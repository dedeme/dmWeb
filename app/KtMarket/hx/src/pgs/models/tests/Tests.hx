// Copyright 16-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.models.tests;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Menu;
import data.Model;
import I18n._;

/// Models test main page.
class Tests {
  var wg: Domo;
  var model: Model;
  var selSubmenu: String;
  public function new (wg: Domo, model: Model) {
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
    ];
    final ropts = [];
    final submenu = new Menu(lopts, ropts, selSubmenu);

    switch (selSubmenu) {
    case "references":
      References.mk(wg, model);
    case "orders":
      new Orders(wg, model);
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

// Copyright 15-Nov-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.models.tests;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Menu;
import data.model.Model;
import I18n._;

/// Model tests main page.
class Tests {
  var wg: Domo;
  var qlevel: Int;
  var selSubmenu: String;
  public function new (wg: Domo, qlevel: Int) {
    this.wg = wg;
    this.qlevel = qlevel;
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
      Menu.toption("orders", _("Orders"), () -> show("orders"))
    ];
    final ropts = [];
    final submenu = new Menu(lopts, ropts, selSubmenu);

    switch (selSubmenu) {
    case "references":
      References.mk(wg, qlevel);
    case "orders":
      new Orders(wg, qlevel);
    default:
      References.mk(wg, qlevel);
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

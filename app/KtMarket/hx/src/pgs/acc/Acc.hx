// Copyright 19-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.acc;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Menu;
import wgs.Dmenu;
import I18n._;

/// Accounting information, main page.
class Acc {
  var wg: Domo;
  var dmenu: Dmenu;
  var lcPath: Array<String>;
  var mSel: String;

  /// Constructor
  ///   wg    : Container.
  ///   dmenu : Double menu.
  ///   lcPath: Location path.
  public function new (wg: Domo, dmenu: Dmenu, lcPath: Array<String>) {
    if (lcPath.length == 0) lcPath.push("profits");

    this.wg = wg;
    this.dmenu = dmenu;
    this.lcPath = lcPath;

    mSel = lcPath[0];

    view();
  }

  // View ----------------------------------------------------------------------

  function view () {
    final wg = Q("div");

    final lopts = [
      dmenu.hiddingButton(),
      Menu.separator2(),
      Menu.tlink("companies", _("Companies"), "acc"),
      Menu.separator(),
      Menu.tlink("balance", _("Balance"), "acc"),
      Menu.separator(),
      Menu.tlink("trading", _("Trading"), "acc"),
      Menu.separator(),
      Menu.tlink("profits", _("Profits"), "acc"),
      Menu.separator2(),
      Menu.tlink("speedometers", _("Speedometers"), "acc")
    ];
    final ropts = [];
    dmenu.setDownMenu(new Menu(lopts, ropts, mSel));

    switch (mSel) {
      case "companies": Companies.mk(wg);
      case "balance": Balance.mk(wg);
      case "trading": Trading.mk(wg);
      case "profits": Profits.mk(wg);
      case "speedometers": Speedometers.mk(wg);
      default: Profits.mk(wg);
    }

    this.wg
      .removeAll()
      .add(wg)
    ;
  }
}

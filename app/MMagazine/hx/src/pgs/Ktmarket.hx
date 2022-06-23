// Copyright 09-May-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs;

import dm.Domo;
import dm.Menu;
import wgs.Dmenu;
import I18n._;

class Ktmarket {
  /// Constructor
  ///   wg    : Container.
  ///   dmenu : Double menu.
  ///   lcPath: Location path.
  public static function mk (
    wg:Domo, dmenu: Dmenu, lcPath: Array<String>
  ) {
    if (lcPath.length == 0) lcPath = ["profits"];

    final lopts = [
      dmenu.hiddingButton(),
      Menu.separator2(),
      Menu.tlink("profits", _("Profits"), "ktmarket"),
      Menu.separator(),
      Menu.tlink("percents", _("Percentages"), "ktmarket"),
    ];
    final ropts = [];
    final menu = new Menu(lopts, ropts, lcPath[0]);
    dmenu.setDownMenu(menu);

    switch (lcPath[0]) {
    case "profits": pgs.ktmarket.ProfitsPg.mk(wg);
    case "percents": pgs.ktmarket.PercentsPg.mk(wg);
    default: pgs.ktmarket.ProfitsPg.mk(wg);
    }
  }
}


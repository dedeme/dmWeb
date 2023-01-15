// Copyright 09-May-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs;

import dm.Domo;
import dm.Menu;
import wgs.Dmenu;
import I18n._;

class Mmarket {
  /// Constructor
  ///   wg    : Container.
  ///   dmenu : Double menu.
  ///   lcPath: Location path.
  public static function mk (
    wg:Domo, dmenu: Dmenu, lcPath: Array<String>
  ) {
    if (lcPath.length == 0) lcPath = ["models"];

    final lopts = [
      dmenu.hiddingButton(),
      Menu.separator2(),
      Menu.tlink("models", _("Models"), "mmarket"),
      Menu.separator(),
      Menu.tlink("hotmaps", _("Hot Maps"), "mmarket"),
    ];
    final ropts = [];
    final menu = new Menu(lopts, ropts, lcPath[0]);
    dmenu.setDownMenu(menu);

    switch (lcPath[0]) {
    case "models": pgs.mmarket.ModelsPg.mk(wg);
    case "hotmaps": pgs.mmarket.HotMapsPg.mk(wg);
    default: pgs.mmarket.ModelsPg.mk(wg);
    }
  }
}


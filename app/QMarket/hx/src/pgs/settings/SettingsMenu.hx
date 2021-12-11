// Copyright 16-Oct-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.settings;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Menu;
import wgs.Dmenu;
import I18n._;
import pgs.settings.settings.Settings;
import pgs.settings.calendarPg.CalendarPg;
import pgs.settings.nicks.Nicks;
import pgs.settings.servers.Servers;
import pgs.settings.acc.Acc;
import pgs.settings.investorsPg.InvestorsPg;

/// Settings menu.
class SettingsMenu {

  // static --------------------------------------------------------------------

  /// Constructor
  ///   wg    : Container.
  ///   dmenu : Double menu.
  ///   lcPath: Location path.
  ///   lang  : Language.
  public static function mk (
    wg: Domo, dmenu: Dmenu, lcPath: Array<String>, lang: String
  ): Void {
    if (lcPath.length == 0) lcPath.push("home");
    final target =
      switch (lcPath[0]) {
        case "settings" | "calendar" | "nicks" | "servers" | "annotations" |
             "investors":
          lcPath[0];
        default:
          "nicks";
      }

    final lopts = [
      dmenu.hiddingButton(),
      Menu.separator2(),
      Menu.tlink("nicks", _("Nicks"), "settings"),
      Menu.separator(),
      Menu.tlink("servers", _("Servers"), "settings"),
      Menu.separator2(),
      Menu.tlink("annotations", _("Annotations"), "settings"),
      Menu.separator2(),
      Menu.tlink("investors", _("Investors"), "settings"),
    ];

    final ropts = [
      Menu.tlink("calendar", _("Calendar"), "settings"),
      Menu.separator(),
      Menu.tlink("settings", _("Settings"), "settings"),
    ];

    dmenu.setDownMenu(new Menu(lopts, ropts, target));

    switch (target) {
      case "nicks":
        Nicks.mk(wg);
      case "servers":
        Servers.mk(wg);
      case "annotations":
        Acc.mk(wg);
      case "investors":
        InvestorsPg.mk(wg, lcPath.length == 2 ? lcPath[1] : "0");
      case "calendar":
        CalendarPg.mk(wg);
      case "settings":
        new Settings(wg, lang);
      default:
        Nicks.mk(wg);
    }
  }
}

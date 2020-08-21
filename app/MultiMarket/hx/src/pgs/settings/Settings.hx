// Copyright 16-Aug-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.settings;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Menu;
import wgs.Dmenu;
import I18n._;
import pgs.settings.settings.Settings as Set;
import pgs.settings.calendar.Calendar;
import pgs.settings.nicks.Nicks;

/// Main settings page.
class Settings {

  // View ----------------------------------------------------------------------

  /// Constructor
  ///   wg    : Container.
  ///   dmenu : Double menu.
  ///   lcPath: Location path.
  ///   lang  : Language.
  public static function mk (
    wg: Domo, dmenu: Dmenu, lcPath: Array<String>, lang: String
  ) {
    if (lcPath.length == 0) lcPath.push("home");
    final target =
      switch (lcPath[0]) {
        case "settings" | "calendar" | "nicks" | "servers" | "annotations" |
             "models":
          lcPath[0];
        default:
          "settings";
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
      Menu.tlink("models", _("Models"), "settings"),
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
      case "calendar":
        Calendar.mk(wg);
      case "settings":
        Set.mk(wg, lang);
      default:
        Set.mk(wg, lang);
    }
  }
}

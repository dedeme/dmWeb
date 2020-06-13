// Copyright 23-Apr-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Domo from "../../dmjs/Domo.js"; //eslint-disable-line
import {Menu, MenuEntry} from "../../dmjs/Menu.js"; //eslint-disable-line
import {_} from "../../I18n.js";
import Dmenu from "../../wgs/Dmenu.js"; //eslint-disable-line
import Set from "./settings/Settings.js";
import Calendar from "./calendar/Calendar.js";
import Nicks from "./nicks/Nicks.js";
import Servers from "./servers/Servers.js";
import Acc from "./acc/Acc.js";
import Models from "./models/Models.js";

/**
    Annotations & Settings page
**/
export default class Settings {
  /**
      @param {!Domo} wg
      @param {!Dmenu} dmenu Double menu
      @param {!Array<string>} lcPath
      @param {string} lang
  **/
  constructor (wg, dmenu, lcPath, lang) {
    this._wg = wg;
    this._lcPath = lcPath;
    this._dmenu = dmenu;
    this._lang = lang;

    if (lcPath.length === 0) lcPath.push("home");
    this._lcPath = lcPath;

    switch (lcPath[0]) {
    case "settings":
    case "calendar":
    case "nicks":
    case "servers":
    case "annotations":
    case "models":
      this._target = lcPath[0];
      break;
    default:
      this._target = "nicks";
    }

    this.view();
  }

  // View ----------------------------------------------------------------------

  /**
      @private
  **/
  view () {
    const tg = this._target;
    const wg = this._wg;
    const dmenu = this._dmenu;

    const lopts = [
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

    const ropts = [
      Menu.tlink("calendar", _("Calendar"), "settings"),
      Menu.separator(),
      Menu.tlink("settings", _("Settings"), "settings"),
    ];

    dmenu.downMenu = new Menu(lopts, ropts, tg);

    switch (tg) {
    case "settings":
      new Set(wg, this._lang); // eslint-disable-line
      break;
    case "calendar":
      Calendar.mk(wg);
      break;
    case "nicks":
      Nicks.mk(wg);
      break;
    case "servers":
      Servers.mk(wg);
      break;
    case "annotations":
      Acc.mk(wg);
      break;
    case "models":
      Models.mk(wg, 0);
      break;
    default:
      new Set(wg, this._lang); // eslint-disable-line
      break;
    }
  }
}

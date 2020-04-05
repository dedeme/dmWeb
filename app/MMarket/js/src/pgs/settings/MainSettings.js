// Copyright 06-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/**
    System pages entry.
**/

import Domo from "../../dmjs/Domo.js"; //eslint-disable-line
import Client from "../../dmjs/Client.js"; //eslint-disable-line
import Ui from "../../dmjs/Ui.js";
import Maybe from "../../dmjs/Maybe.js";
import {Menu} from "../../dmjs/Menu.js";
import {_} from "../../I18n.js";
import HidingMenu from "../../pgs/main/HidingMenu.js"; //eslint-disable-line
import Path from "../../data/Path.js"; //eslint-disable-line
import Settings from "./Settings.js";
import Calendar from "./Calendar.js";

const $ = e => Ui.$(e);

/**
    System pages entry.
**/
export default class MainSettings {
  /**
      @private
      @param {!Client} client
      @param {string} page
      @param {!Domo} wgChild
      @param {!HidingMenu} upMenu
      @param {!Maybe<function():void>=} afterShow
  **/
  constructor (client, page, wgChild, upMenu, afterShow = Maybe.nothing) {
    this._client = client;
    this._page = page;
    this._wgChild = wgChild;
    this._upMenu = upMenu;
    this._afterShow = afterShow;

    this._wg = $("div");
    this.view();
  }

  /**
      @return {!Domo}
  **/
  get wg () {
    return this._wg;
  }

  /**
      @return {!Maybe<function():void>}
  **/
  get afterShow () {
    return this._afterShow;
  }

  // View ----------------------------------------------------------------------

  /**
      @private
  **/
  view () {
    const lopts = [
      this._upMenu.button,
      Menu.separator2(),
      Menu.tlink("Home", _("Home"), "Settings")
    ];

    const ropts = [
      Menu.tlink("Calendar", _("Calendar"), "Settings"),
      Menu.separator(),
      Menu.tlink("Settings", _("Settings"), "Settings")
    ];
    const menu = new Menu(lopts, ropts, this._page);

    this._wg.removeAll()
      .add(menu.wg)
      .add(this._wgChild)
    ;
  }

  // Static --------------------------------------------------------------------

  /**
      @param {!Client} client
      @param {!Path} path
      @param {string} lang
      @param {!HidingMenu} menu
      @return {!Promise<!Maybe<!MainSettings>>}
  **/
  static async mk (client, path, lang, menu) {
    const pg = path.nextPage("Settings");
    if (pg.isNothing()) return Maybe.nothing;
    const page = pg.fromJust();

    switch (page) {
    case "Calendar": {
      const pg = await Calendar.mk(client);
      return Maybe.just(new MainSettings(client, page, pg.wg, menu));
    }
    case "Settings": {
      const pg = new Settings(client, lang);
      return Maybe.just(new MainSettings(client, page, pg.wg, menu));
    }
    }
    return Maybe.nothing;
  }
}



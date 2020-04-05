// Copyright 08-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/**
    System pages entry.
**/

import Domo from "../../dmjs/Domo.js"; //eslint-disable-line
import Client from "../../dmjs/Client.js"; //eslint-disable-line
import Ui from "../../dmjs/Ui.js";
import Maybe from "../../dmjs/Maybe.js";
import {Menu} from "../../dmjs/Menu.js";
import Defs from "../../Defs.js";
import Msg from "../../Msg.js";
import {I18n, _, _args} from "../../I18n.js";
import HidingMenu from "./HidingMenu.js";
import Path from "../../data/Path.js"; //eslint-disable-line
import MainSettings from "../../pgs/settings/MainSettings.js";
import MainHome from "../../pgs/home/MainHome.js";
import MainFleas from "../../pgs/fleas/MainFleas.js";
import MainRanking from "../../pgs/ranking/MainRanking.js";

const $ = e => Ui.$(e);

/**
    @param {!Client} client
**/
function bye (client) {
  if (!confirm(_("Application exit?"))) {
    return;
  }
  client.send({
    "module": "Main",
    "rq": "bye",
    "sessionId": client.sessionId()
  });

  const pg = new Msg(Defs.app, _args(_("Logout-message"), Defs.app));
  $("@body").removeAll().add(pg.wg);
}

/**
    System pages entry.
**/
export default class MainMain {
  /**
      @private
      @param {!Client} client
      @param {!Domo} wgChild
      @param {!HidingMenu} menu
      @param {!Maybe<function():void>=} afterShow
  **/
  constructor (client, wgChild, menu, afterShow = Maybe.nothing) {
    this._client = client;
    this._wgChild = wgChild;
    this._afterShow = afterShow;
    this._menu = menu;

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
    this._wg.removeAll()
      .add(this._menu.wg)
      .add(this._wgChild)
    ;
  }

  // Static --------------------------------------------------------------------

  /**
      @param {!Client} client
      @param {!Path} path
      @return {Promise<!MainMain>}
  **/
  static async mk (client, path) {
    const rp = await client.send({
      "module": "Main",
      "rq": "lang"
    });

    const lang = rp["lang"];
    if (lang === "es") I18n.es(); else I18n.en();

    const pageM = path.nextPage("Home");
    const page = pageM.isNothing() ? "Home" : pageM.fromJust();
    const newPath = path.add(page);

    const lopts = [
      Menu.tlink("Home", _("Home")),
      Menu.separator2(),
      Menu.tlink("Fleas", _("Fleas")),
      Menu.separator(),
      Menu.tlink("Ranking", _("Ranking"))
    ];

    const ropts = [
      Menu.tlink("Settings", _("Annotations & Settings")),
      Menu.separator(),
      Menu.close(() => { bye(client) })
    ];
    const menu = new HidingMenu();
    menu.menu = new Menu(lopts, ropts, page);

    switch (page) {
    case "Fleas": {
      menu.hide();
      const pg = await MainFleas.mk(client, newPath, menu);
      if (pg.isJust())
        return new MainMain(client, pg.fromJust().wg, menu);
      break;
    }
    case "Ranking": {
      menu.hide();
      const pg = MainRanking.mk(client, newPath, menu);
      return new MainMain(client, pg.wg, menu);
    }
    case "Settings": {
      menu.hide();
      const pg = await MainSettings.mk(client, newPath, lang, menu);
      if (pg.isJust())
        return new MainMain(
          client, pg.fromJust().wg, menu, pg.fromJust().afterShow
        );
      break;
    }
    }
    menu.menu = new Menu(lopts, ropts, "Home");
    const pg = await MainHome.mk(client);
    menu.show();
    return new MainMain(client, pg.wg, menu);
  }

}



// Copyright 23-Apr-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Domo from "../dmjs/Domo.js"; //eslint-disable-line
import Ui from "../dmjs/Ui.js";
import Maybe from "../dmjs/Maybe.js";
import {Menu, MenuEntry} from "../dmjs/Menu.js"; //eslint-disable-line
import {_, _args} from "../I18n.js";
import Cts from "../data/Cts.js";
import Msg from "../MsgPage.js";

const $ = e => Ui.$(e);

/**
    Double menu.
**/
export default class Dmenu {
  /**
      @param {!Domo} wg
      @param {string} selected Option selected in up menu.
  **/
  constructor (wg, selected) {
    this._wg = wg;
    this._selected = selected;

    this._upDiv = $("div").style("padding:0px");
    this._upMenu = this.mkUpMenu();
    this._downDiv = $("div");
    this._hidden = false;
    this.view();
  }

  // View ----------------------------------------------------------------------
  /**
      @private
      @return void
  **/
  view () {
    this._wg
      .removeAll()
      .add(this._upDiv
        .removeAll()
        .add(this._upMenu.wg))
      .add(this._downDiv)
    ;
  }

  /**
      @private
      @return !Menu
  **/
  mkUpMenu () {
    const lopts = [
      Menu.tlink("home", _("Home")),
      Menu.separator2(),
      Menu.tlink("fleas", _("Fleas")),
      Menu.separator(),
      Menu.tlink("ranking", _("Ranking")),
      Menu.separator2(),
      Menu.tlink("acc", _("Accounting")),
      Menu.separator2(),
      Menu.tlink("daily", _("Daily Quotes"))
    ];

    const ropts = [
      Menu.tlink("settings", _("Annotations & Settings")),
      Menu.separator(),
      Menu.close(() => { this.bye() })
    ];

    return new Menu(lopts, ropts, this._selected);
  }

  /**
      Button to hidden-show up menu.
      @return {!MenuEntry}
  **/
  hiddingButton () {
    return new MenuEntry(
      Maybe.nothing,
      Ui.link(() => this.change())
        .add(Ui.img("menu").style("vertical-align:middle"))
    );
  }

  // Control -------------------------------------------------------------------

  /**
      @private
      @return void
  **/
  change () {
    this._hidden = !this._hidden;
    this._upDiv
      .removeAll()
    ;
    if (!this._hidden) {
      this._upDiv
        .removeAll()
        .add(this._upMenu.wg)
      ;
    }
  }

  bye () {
    if (!confirm(_("Application exit?"))) {
      return;
    }
    Cts.client.send({
      "module": "wgs",
      "source": "menu",
      "rq": "bye",
      "sessionId": Cts.client.sessionId()
    });

    const pg = new Msg(Cts.appName, _args(_("Logout-message"), Cts.appName));
    $("@body")
      .removeAll()
      .add(pg.wg)
    ;
  }

  /**
      Sets down menu.
      @param {!Menu} menu
  **/
  set downMenu (menu) {
    this._downDiv
      .removeAll()
      .add(menu.wg)
    ;
    this._hidden = true;
    this._upDiv
      .removeAll()
    ;
  }
}

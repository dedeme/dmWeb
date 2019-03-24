// Copyright 24-Mar-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Client from "../dmjs/Client.js";
import Ui from "../dmjs/Ui.js";
import Dom from "../core/Dom.js";
import Main from "../Main.js";
import SysMain from "./SysMain.js";

const $ = Ui.$;

/** Sys Main page. */
export default class Home {
  /**
   * @param {!SysMain} sysMain
   */
  constructor (sysMain) {
    /**
     * @private
     * @type {!SysMain}
     */
    this._sysMain = sysMain;
  }

  /** @return {!Client} */
  get client () {
    return this._sysMain.client;
  }

  /** @return {!Dom} */
  get dom () {
    return this._sysMain.dom;
  }

  show () {
    this.dom.show(SysMain.homePageId,
      $("div").html("here")
    );
  }
}


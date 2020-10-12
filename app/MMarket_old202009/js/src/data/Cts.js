// Copyright 21-Apr-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Domo from "../dmjs/Domo.js"; //eslint-disable-line
import Ui from "../dmjs/Ui.js";
import Client from "../dmjs/Client.js";
import Dec from "../dmjs/Dec.js";
import {_} from "../I18n.js";
import Msg from "../MsgPage.js";

const $ = e => Ui.$(e);

const app = "MMarket";
const version = "202004";
const appClient = new Client(true, app, () => {
  const exp = new Msg(app, _("Session is expired."), true);
  $("@body").removeAll().add(exp.wg);
});

/**
    Global application constants.
**/
export default class Cts {
  /**
      Application name.
      @return string
  **/
  static get appName () {
    return app;
  }

  /**
      Application version.
      @return string
  **/
  static get appVersion () {
    return version;
  }

  /**
      Communications manager.
      @return !Client
  **/
  static get client () {
    return appClient;
  }

  /**
      Ammount to make a buy.
      @return number
  **/
  static get bet () {
    return 10000;
  }

  /**
      Investor initial capital.
      @return number
  **/
  static get initialCapital () {
    return 100000;
  }

  /**
      Extern program name.
      @return string
  **/
  static get wget () {
    return "Wget";
  }

  /**
      Extern program name.
      @return string
  **/
  static get puppeteer () {
    return "Puppeteer";
  }

  /**
      Server state.
      @return number
  **/
  static get serverStopped () {
    return 0;
  }

  /**
      Server state.
      @return number
  **/
  static get serverActive () {
    return 1;
  }

  /**
      Server state.
      @return number
  **/
  static get serverSelected () {
    return 2;
  }

  /**
      Standar messsage for server fails.
      @return string
  **/
  static get okMsg () {
    return _("Operation successfully done.");
  }

  /**
      Standar messsage for server fails.
      @return string
  **/
  static get failMsg () {
    return _("Operation failed.\nSee log.");
  }

  /**
      Format for numeric flea parameters.
      @param {number} v Value
      @param {number} f Format. Expected:
                      0 - Integer
                      4, 6 - Percentage.
                      Other - Normal number with 'other' decimals positions.
      @return strting
  **/
  static nformat (v, f) {
    return f === 4 || f === 6
      ? new Dec(v * 100, f - 2).toIso() + "%"
      : f === 0
        ? new Dec(v|0, 0).toIso()
        : new Dec(v, f).toIso()
    ;
  }

}

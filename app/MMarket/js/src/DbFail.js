// Copyright 09-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Defs from "./Defs.js";
import Msg from "./Msg.js";
import {_} from "./I18n.js";
import Ui from "./dmjs/Ui.js";

const $ = e => Ui.$(e);

/**
    Page-message for data base out of date.
**/
export default class DbFail {
  /**
      @return {void}
  **/
  static show () {
    const pg = new Msg(Defs.app, _("Data Base is out of date."), true);
    $("@body").removeAll().add(pg.wg);
  }
}

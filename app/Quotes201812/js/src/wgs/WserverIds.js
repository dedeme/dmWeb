// Copyright 13-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// eslint-disable-next-line
import Main from "../Main.js";
import Ui from "../dmjs/Ui.js";
// eslint-disable-next-line
import Tp from "../dmjs/Tp.js";
import {_} from "../I18n.js";
// eslint-disable-next-line
import Domo from "../dmjs/Domo.js";
import Wrule from "./Wrule.js";
import WserverId from "./WserverId.js";

const $ = Ui.$;

/** Widget for reading a server identifier */
export default class WserverIds {
  /**
   * @param {!Main} main
   * @param {!Array<Tp<string, string>>} serverIdCodes They are pairs
   *         <serverId, nickCode>
   * @param {string} nickId
   */
  constructor (main, serverIdCodes, nickId) {
    this._main = main;

    this._wservers = serverIdCodes
      .map(idCode => new WserverId(main, idCode.e1, nickId, idCode.e2));
  }

  test () {
    for (const ws of this._wservers) {
      ws.test();
    }
  }

  /** @return {!Domo} */
  wg () {
    const self = this;
    return $("div").style("text-align:center;")
      .add(Wrule.mkBig(_("Server codes")))
      .add(Ui.link(() => {
        self.test();
      }).klass("link").text(_("Test all")))
      .add($("table").style("width:100%").att("cellspacing", 10)
        .add($("tr")
          .adds(self._wservers.map(ws =>
            $("td").klass("frame").style("text-align:center;width:33%;")
              .add(Wrule.mkSmall(ws.serverId))
              .add(ws.wg())
          ))))
    ;

  }
}

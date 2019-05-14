// Copyright 13-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// eslint-disable-next-line
import Main from "../Main.js";
import Ui from "../dmjs/Ui.js";
// eslint-disable-next-line
import Domo from "../dmjs/Domo.js";
import Dec from "../dmjs/Dec.js";
import {_} from "../I18n.js";

const $ = Ui.$;

/** Widget for reading a server identifier */
export default class WserverId {
  /**
   * @param {!Main} main
   * @param {string} serverId
   * @param {string} nickId
   * @param {string} nickCode Server code.
   */
  constructor (main, serverId, nickId, nickCode) {
    this._main = main;
    this._serverId = serverId;
    this._nickId = nickId;
    this._nickCode = nickCode;

    this._icon = $("td").style("width:5px;");
    this._msg = $("span");
    this._field = $("input").style("width:150px");
  }

  /** @return {string} */
  get nickId () {
    return this._nickId;
  }

  /** @return {string} */
  get serverId () {
    return this._serverId;
  }

  /** @return {string} */
  get nickCode () {
    return this._nickCode;
  }

  async test () {
    const code = this._field.value().trim();
    this._icon.removeAll().add($("img").att("src", "img/wait.gif"));
    this._msg.removeAll();
    if (code === "") {
      this._icon.removeAll().add(Ui.img("error"));
      return;
    }
    const now = Date.now();
    const data = {
      "source": "wserverId",
      "rq": "testCode",
      "serverId": this._serverId,
      "code": code
    };
    const rp = await this._main.client.send(data);
    const e = rp["error"];
    if (e === "") {
      const dif = new Dec((Date.now() - now) / 1000, 3);
      this._icon.removeAll().add(Ui.img("well"));
      this._msg.removeAll().text(`${dif.toEu()} secs.`);
    } else {
      this._icon.removeAll().add(Ui.img("error").att("title", e));
    }
  }

  change () {
    const code = this._field.value().trim();
    if (code === "") {
      return;
    }
    const data = {
      "source": "wserverId",
      "rq": "setCode",
      "serverId": this._serverId,
      "nickId": this._nickId,
      "code": code
    };
    this._main.client.send(data);
  }

  // ____
  // View ------------------------------------------------------------
  // TTTT

  /** @return {!Domo} */
  wg () {
    const self = this;

    self._field.value(self._nickCode).on("change", () => {
      self.change();
    });
    return $("table").att("align", "center")
      .add($("tr")
        .add($("td").style("width:5px;").add(Ui.link(() => {
          self.test();
        }).add(Ui.img("download")).att("title", _("Test"))))
        .add(self._icon.add(Ui.img("unknown")))
        .add($("td").add(self._msg)))
      .add($("tr")
        .add($("td").att("colspan", 3).add(self._field)))
    ;

  }
}

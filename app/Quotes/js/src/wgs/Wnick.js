// Copyright 12-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Main from "../Main.js";
import {_args, _} from "../I18n.js";
import Ui from "../dmjs/Ui.js";
// eslint-disable-next-line
import Nick from "../data/Nick.js";

const $ = Ui.$;

const img = (id, title) => Ui.img(id).att("title", title);
const limg = (id, title) => Ui.lightImg(id).att("title", title);
const emptyBt = (title) => $("div")
  .style("padding:5px;" +
         "border: 1px solid #002040;border-radius: 6px;" +
         "background: #d0ddde;")
  .att("title", title)
;
/** Wnick widget. */
export default class Wnick {
  /**
   * @param {!Main} main
   * @param {string} model
   * @param {!Nick} nick
   */
  constructor (main, model, nick) {
    this._main = main;
    this._nick = nick;
    this._isModel = model === nick.id;

    this._ibexDiv = $("div");
    this._selDiv = $("div");
    this._checkDiv = $("div");
  }

  /** @return {string} */
  get id () {
    return this._nick.id;
  }

  /**
   * @param {boolean} value
   * @param {string} msg
   * @return {void}
   */
  check (value, msg) {
    const self = this;
    if (value) {
      self._checkDiv.removeAll().add(Ui.img("well"));
    } else {
      self._checkDiv.removeAll().add(Ui.link(async () => {
        const data = {
          "source": "wnick",
          "rq": "issue",
          "id": self._nick.id,
          "menu": Main.issuesPageId
        };
        await this._main.client.send(data);
        this._main.run();
      }).add(Ui.img("error").att("title", msg)));
    }
  }

  /**
   * @param {string} error
   * @return {void}
   */
  download (error) {
    const self = this;
    if (error === "") {
      self._checkDiv.removeAll().add(Ui.img("well"));
    } else {
      self._checkDiv.removeAll().add(
        Ui.img("error")
          .att("title", _args(_("Fail downloading quotes:\n%0"), error))
      );
    }
  }

  /** @return {void} */
  wait () {
    this._checkDiv.removeAll().add($("img").att("src", "img/wait.gif"));
  }

  /** @private */
  async setModel (id) {
    const data = {
      "source": "wnick",
      "rq": "setModel",
      "id": id
    };
    await this._main.client.send(data);
    this._main.run();
  }

  /** @private */
  async del (id, nick) {
    if (!confirm(_args(_("Delete '%0'?"), nick))) {
      return;
    }
    const data = {
      "source": "wnick",
      "rq": "del",
      "id": id
    };
    await this._main.client.send(data);
    this._main.run();
  }

  /** @private */
  async toNicks (id, nick) {
    if (!confirm(_args(_("Send '%0' to Nicks?"), nick))) {
      return;
    }
    const data = {
      "source": "wnick",
      "rq": "setIsExtra",
      "id": id,
      "value": false
    };
    await this._main.client.send(data);
    this._main.run();
  }

  /** @private */
  async toExtra (id, nick) {
    if (!confirm(_args(_("Send '%0' to Extra?"), nick))) {
      return;
    }
    const data = {
      "source": "wnick",
      "rq": "setIsExtra",
      "id": id,
      "value": true
    };
    await this._main.client.send(data);
    this._main.run();
  }

  /** @private */
  async setIsIbex (id, value) {
    const self = this;
    const data = {
      "source": "wnick",
      "rq": "setIsIbex",
      "id": id,
      "value": value
    };
    await self._main.client.send(data);
    if (value) {
      self._ibexDiv.removeAll().add(Ui.link(() => {
        self.setIsIbex(id, false);
      }).add(img("flag2", "Ibex")));
    } else {
      self._ibexDiv.removeAll().add(Ui.link(() => {
        self.setIsIbex(id, true);
      }).add(emptyBt("Ibex")));
    }
  }

  /** @private */
  async setIsSel (id, value) {
    const self = this;
    const data = {
      "source": "wnick",
      "rq": "setIsSel",
      "id": id,
      "value": value
    };
    await self._main.client.send(data);
    if (value) {
      self._selDiv.removeAll().add(Ui.link(() => {
        self.setIsSel(id, false);
      }).add(img("flag1", _("Selection"))));
    } else {
      self._selDiv.removeAll().add(Ui.link(() => {
        self.setIsSel(id, true);
      }).add(emptyBt(_("Selection"))));
    }
  }

  /** @private */
  async edit (id) {
    const data = {
      "source": "wnick",
      "rq": "edit",
      "id": id,
      "menu": Main.editPageId
    };
    await this._main.client.send(data);
    this._main.run();
  }

  // ____
  // View ------------------------------------------------------------
  // TTTT

  wg () {
    const self = this;
    const nick = self._nick;

    const model = self._isModel
      ? img("star", _("Model"))
      : nick.isExtra
        ? Ui.link(() =>
          self.del(nick.id, nick.nick)).add(img("delete", _("Delete")))
        : Ui.link(() => {
          self.setModel(nick.id);
        }).add(limg("star2", _("Model")));

    const change = nick.isExtra
      ? Ui.link(() =>
        self.toNicks(nick.id, nick.nick)).add(img("plus", _("To Nicks")))
      : Ui.link(() =>
        self.toExtra(nick.id, nick.nick)).add(img("minus", _("To Extra")));

    const isIbex = nick.isIbex
      ? self._ibexDiv.removeAll().add(Ui.link(() => {
        self.setIsIbex(nick.id, false);
      }).add(img("flag2", "Ibex")))
      : nick.isExtra
        ? self._ibexDiv.removeAll()
        : self._ibexDiv.removeAll().add(Ui.link(() => {
          self.setIsIbex(nick.id, true);
        }).add(emptyBt("Ibex")));

    const isSel = nick.isSel
      ? self._selDiv.removeAll().add(Ui.link(() => {
        self.setIsSel(nick.id, false);
      }).add(img("flag1", _("Selection"))))
      : nick.isExtra
        ? self._ibexDiv.removeAll()
        : self._selDiv.removeAll().add(Ui.link(() => {
          self.setIsSel(nick.id, true);
        }).add(emptyBt(_("Selection"))));

    return $("table")
      .add($("tr")
        .add($("td").add(model))
        .add($("td").add(change))
        .add($("td").add(isIbex))
        .add($("td").add(isSel))
        .add($("td").add(Ui.link(() => {
          self.edit(nick.id);
        }).klass("link").text(nick.nick)))
        .add($("td").add(self._checkDiv.removeAll().add(Ui.img("unknown")))))
    ;
  }
}

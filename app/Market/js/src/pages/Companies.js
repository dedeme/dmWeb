// Copyright 04-Jan-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

//eslint-disable-next-line
import Main from "../Main.js";
import {_} from "../I18n.js";
import Ui from "../dmjs/Ui.js";
import It from "../dmjs/It.js";

const $ = Ui.$;

async function each (it, f) {
  if (it.has) {
    await f(it.value);
    each(it.next, f);
  }
}

function mkGr (nick, profits, qs) {
  console.log(qs);
  return $("td").html("|" + nick + "|" + String(profits) + "|");
}

/** Companies page. */
export default class Companies {
  /**
   * @param {!Main} main Main
   */
  constructor (main) {
    /**
     * @private
     * @type {!Main}
     */
    this._main = main;
  }

  /** @private */
  mkLink () {
    let value = true;
    let text = _("All Companies");
    if (this._main.model["allCos"]) {
      value = false;
      text = _("Portfolio");
    }
    const f = async () => {
      const rq = {
        "source": "companies",
        "rq": "allCos",
        "value": value
      };
      await this._main.client.send(rq);
      this._main.run();
    };
    return Ui.link(f).klass("link").html(text);
  }

  /**
   * @return {Promise}
   */
  async show () {
    const tb = $("table").klass("frame");
    this._main.dom.show(Main.companiesPageId,
      $("div").style("text-align:center")
        .add($("div").add(this.mkLink()))
        .add(tb)
    );

    const rq = {
      "source": "companies",
      "rq": "list"
    };
    const rp = await this._main.client.send(rq);
    let list = rp["list"];
    if (!this._main.model["allCos"]) {
      list = list.filter(e => e[1]);
    }
    list = list.map(e => e[0]);
    list.sort();

    let tds = [];
    each(It.from(list), async (nick) => {
      const rq = {
        "source": "companies",
        "rq": "historic",
        "nick": nick
      };
      const rp = await this._main.client.send(rq);
      const profits = rp["profits"];
      const quotes = rp["quotes"];
      tds.push(mkGr(nick, profits, quotes));
      if (tds.length === 5) {
        tb.add($("tr").adds(tds));
        tds = [];
      }
    });
    if (tds.length > 0) {
      It.range(5 - tds.length).each(() => { tds.push($("td")) });
      tb.add($("tr").adds(tds));
    }
  }
}

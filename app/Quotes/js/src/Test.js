// Copyright 29-Jan-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Main from "./Main.js";
import {_} from "./I18n.js";
import Ui from "./dmjs/Ui.js";
import DateDm from "./dmjs/DateDm.js";
import Dec from "./dmjs/Dec.js";

const $ = Ui.$;

class Quotes {
  constructor (open, close, max, min) {
    this.open = open;
    this.close = close;
    this.max = max;
    this.min = min;
  }

  static mk (a) {
    return new Quotes(a[0], a[1], a[2], a[3]);
  }

  toString () {
    function formatN (n) {
      const s = "        " + new Dec(n, 4).toString();
      return s.substring(s.length - 10);
    }
    return formatN(this.open) + " | " +
      formatN(this.close) + " | " +
      formatN(this.max) + " | " +
      formatN(this.min);
  }
}

class Server {
  constructor (id, quotes) {
    this.id = id;
    this.quotes = quotes;
  }

  static mk (a) {
    return new Server(a[0], Quotes.mk(a[1]));
  }

}

class Entry {
  constructor (lang, date, me, sv1, sv2, sv3) {
    this.lang = lang;
    this.date = date;
    this.me = me;
    this.sv1 = sv1;
    this.sv2 = sv2;
    this.sv3 = sv3;
  }

  static mk (lang, a) {
    return new Entry(
      lang,
      a[0], Quotes.mk(a[1]), Server.mk(a[2]), Server.mk(a[3]), Server.mk(a[4])
    );
  }

  toString () {
    function formatId (id) {
      return (id + "          ").substring(0, 10);
    }
    const date = DateDm.fromStr(this.date);
    return "----------\n" +
      (this.lang === "es" ? date.toString() : date.format("%M/%D/%Y")) +
      "\n|" +
      formatId("me") +
      " | " +
      this.me.toString() +
      "|\n|" +
      formatId(this.sv1.id) +
      " | " +
      this.sv1.quotes.toString() +
      "|\n|" +
      formatId(this.sv2.id) +
      " | " +
      this.sv2.quotes.toString() +
      "\n|" +
      formatId(this.sv3.id) +
      " | " +
      this.sv3.quotes.toString() +
      "\n";
  }
}

// -------------------------------------------------------------------

/** Test page. */
export default class Test {
  /**
   * @param {!Main} main Main
   */
  constructor (main) {
    /**
     * @private
     * @type {!Main}
     */
    this._main = main;

    this._issues = [];

    this._textArea = $("textArea").att("cols", 110).att("rows", 30)
      .att("readOnly", true);
  }

  showText () {
    let tx = "";
    this._issues.forEach(i =>
      tx += Entry.mk(this._main.model["lang"], i).toString()
    );
    this._textArea.value(tx);
  }

  /**
   * @return {Promise}
   */
  async show () {
    const data = {
      "source": "test",
      "rq": "idata",
      "type": "nicks"
    };
    const rp = await this._main.client.send(data);
    // one issue is [date, qsMe, sv1, sv2, sv3]
    this._issues = rp["issues"];

    this._main.dom.show(
      Main.testPageId,
      $("div").style("text-align: center;")
        .add($("div")
          .add($("h2").html(_("Test"))))
        .add($("hr"))
        .add($("div")
          .add(Ui.link(() => alert("Nicks")).klass("link")
            .html(_("Nicks")))
          .add($("span").html("&nbsp;&nbsp;||&nbsp;&nbsp;"))
          .add(Ui.link(() => alert("Extra")).klass("link")
            .html(_("Nicks + Extra"))))
        .add(this._textArea)
    );
    this.showText();
  }
}


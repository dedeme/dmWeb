// Copyright 13-Dic-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

//eslint-disable-next-line
import Main from "../Main.js";
import {_} from "../I18n.js";
import Ui from "../dmjs/Ui.js";
import Dec from "../dmjs/Dec.js";
import DateDm from "../dmjs/DateDm.js";

const $ = Ui.$;

const balanceSeparator = () => $("td").klass("separator");

/** Update page. */
export default class Balance {
  /**
   * @param {!Main} main Main
   */
  constructor (main) {
    /**
     * @private
     * @type {!Main}
     */
    this._main = main;

    this._currentProfits = 0;
    this._accountableProfits = 0;

    this._stocks = 0;
    this._cash = 0;
    this._capital = 0;
    this._sells = 0;
    this._fees = 0;
    this._incomes = 0;
    this._differences = 0;

    this._portfolio = [];

    this._connDiv = $("div");
  }

  formatN (n, dec) {
    const d = new Dec(n, dec);
    return this._main.model["lang"] === "es" ? d.toEu() : d.toEn();
  }

  download () {
    const n = this._portfolio.length;
    if (n === 0) {
      this._currentProfits = this._accountableProfits;
      this.reload();
      return;
    }
    this._connDiv.removeAll().add(Ui.$("img").att("src", "img/wait2.gif"));

    let c = 0;

    const f = async () => {
      this._connDiv.removeAll().add(
        $("div").style("font-size:44px").html(String(c))
      );
      if (c === n) {
        this._currentProfits = this._accountableProfits;
        this._portfolio.forEach(e => {
          if (this._currentProfits !== null) {
            if (e[3] !== null) {
              this._currentProfits += (e[3] - e[2]) * e[1];
            } else {
              this._currentProfits = null;
            }
          }
        });
        if (this._currentProfits !== null) {
          const last = {};
          this._portfolio.forEach(e => {
            if(e[3] !== null) {
              last[e[0]] = e[3];
            }
          });
          const rq = {
            "source": "balance",
            "rq": "historic",
            "date": DateDm.now().toBase(),
            "profits": Math.round(this._currentProfits * 100) / 100,
            "last": last
          };
          this._main.client.send(rq);
        }
        this.reload();
      } else {
        try {
          const e = this._portfolio[c];
          const rq = {
            "source": "balance",
            "rq": "download",
            "nick": e[0]
          };

          const rp = await this._main.client.sendAsync(rq);
          e[3] = rp["quote"];

          if (e[3] < 0) {
            e[3] = null;
          }

          ++c;
          f();
        } catch (e) {
          e[3] = null;
          ++c;
          f();
        }
      }
    };
    f();
  }

  body () {
    return $("div")
      .add($("div").style("text-align:center").add(this._connDiv))
      .add($("div").klass("head").html(_("Profits")))
      .add($("table").att("align", "center").klass("home")
        .add($("tr")
          .add($("td").klass("rlabel")
            .add($("span").html(_("Current profits:"))))
          .add($("td").klass("number")
            .add($("span").html(
              this._currentProfits === null ? "[?]"
                : this.formatN(this._currentProfits, 2)))))
        .add($("tr")
          .add($("td").klass("rlabel")
            .add($("span").html(_("Accounting profits:"))))
          .add($("td").klass("number")
            .add($("span").html(this.formatN(this._accountableProfits, 2))))))
      .add($("div").klass("head").html(_("Balance")))
      .add($("table").att("align", "center").klass("home")
        .add($("tr")
          .add($("td").att("colspan", 2).klass("head")
            .add($("span").html(_("Assets"))))
          .add(balanceSeparator())
          .add($("td").att("colspan", 2).klass("head")
            .add($("span").html(_("Equity")))))
        .add($("tr")
          .add($("td").klass("rlabel")
            .add($("span").html(_("Stocks") + ":")))
          .add($("td").klass("number")
            .add($("span").html(this.formatN(this._stocks, 2))))
          .add(balanceSeparator())
          .add($("td").klass("rlabel")
            .add($("span").html(_("Capital") + ":")))
          .add($("td").klass("number")
            .add($("span").html(this.formatN(this._capital, 2)))))
        .add($("tr")
          .add($("td").klass("rlabel")
            .add($("span").html(_("Cash") + ":")))
          .add($("td").klass("number")
            .add($("span").html(this.formatN(this._cash, 2))))
          .add(balanceSeparator())
          .add($("td").klass("rlabel")
            .add($("span").html(_("Sells") + ":")))
          .add($("td").klass("number")
            .add($("span").html(this.formatN(this._sells, 2)))))
        .add($("tr")
          .add($("td"))
          .add($("td"))
          .add(balanceSeparator())
          .add($("td").klass("rlabel")
            .add($("span").html(_("Fees") + ":")))
          .add($("td").klass("number")
            .add($("span").html(this.formatN(this._fees, 2)))))
        .add($("tr")
          .add($("td"))
          .add($("td"))
          .add(balanceSeparator())
          .add($("td").klass("rlabel")
            .add($("span").html(_("Profits") + ":")))
          .add($("td").klass("number")
            .add($("span").html(this.formatN(this._incomes, 2)))))
        .add($("tr")
          .add($("td"))
          .add($("td"))
          .add(balanceSeparator())
          .add($("td").klass("rlabel")
            .add($("span").html(_("Differences") + ":")))
          .add($("td").klass("number")
            .add($("span").html(this.formatN(this._differences, 2))))))
      .add($("div").klass("head").html(_("Stocks")))
      .add($("table").att("align", "center").klass("home")
        .add($("tr")
          .add($("td").klass("head")
            .add($("span").html(_("Co."))))
          .add($("td").klass("head")
            .add($("span").html("Nm.")))
          .add($("td").klass("head")
            .add($("span").html(_("Price"))))
          .add($("td").klass("head")
            .add($("span").html(_("Value"))))
          .add($("td").klass("head")
            .add($("span").html(_("Dif.")))))
        .adds(this._portfolio.map(e => $("tr")
          .add($("td").klass("nick")
            .add($("span").html(e[0])))
          .add($("td").klass("number2")
            .add($("span").html(this.formatN(e[1], 0))))
          .add($("td").klass("number2")
            .add($("span").html(this.formatN(e[2], 4))))
          .add($("td").klass("number")
            .add($("span").html(e[3] === null ? "[?]" : this.formatN(e[3], 2))))
          .add($("td").klass("number")
            .add($("span")
              .html(e[3] === null ? "[?]"
                : this.formatN((e[3] - e[2]) * e[1], 2)))))))
    ;

  }

  reload () {
    this._main.dom.show(Main.balancePageId, this.body());
    this._connDiv.removeAll()
      .add(Ui.link(() => { this.download() }).add(Ui.img("download")));
  }

  /**
   * @return {Promise}
   */
  async show () {
    const data = {
      "source": "balance",
      "rq": "idata"
    };
    const rp = await this._main.client.send(data);

    const ld = rp["ledger"];
    this._stocks = ld[0];
    this._cash = ld[1];
    this._capital = -ld[2];
    this._sells = -ld[3];
    this._fees = -ld[4];
    this._incomes = -ld[5];
    this._differences = -ld[6];

    this._accountableProfits = this._sells + this._fees +
      this._incomes + this._differences;
    this._currentProfits = this._accountableProfits;

    const pf = rp["pf"];
    pf.sort((row1, row2) => row1[0] > row2[0] ? 1 : -1);
    this._portfolio = pf.map(e => {
      if (e[3] <= 0) {
        e[3] = null;
        this._currentProfits = null;
      } else if (this._currentProfits !== null) {
        this._currentProfits += (e[3] - e[2]) * e[1];
      }
      return e;
    });

    this.reload();
  }
}


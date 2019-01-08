// Copyright 08-Jun-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

//eslint-disable-next-line
import Main from "../Main.js";
//eslint-disable-next-line
import Domo from "../dmjs/Domo.js";
import Ui from "../dmjs/Ui.js";
import DateDm from "../dmjs/DateDm.js";

const $ = Ui.$;

/** Downloader widget. */
export default class Downloader {
  /**
   * @param {!Main} main Main
   */
  constructor (main) {
    /**
     * @private
     * @type {!Main}
     */
    this._main = main;

    this._wg = $("div")
      .add(Ui.link(() => { this.download() })
        .add(Ui.img("download").style("vertical-align:middle")))
      .add($("span").html("&nbsp;&nbsp;&nbsp;&nbsp;"))
      .add(Ui.img("ledOk"));
  }

  /**
   * @return {!Domo}
   */
  get wg () {
    return this._wg;
  }

  /**
   * @private
   */
  async download () {
    const rq = {
      "source": "downloader",
      "rq": "portfolio",
    };

    const rp = await this._main.client.send(rq);
    const portfolio = rp["portfolio"];
    let profits = rp["profits"];

    const n = portfolio.length;
    if (n === 0) {
      return;
    }

    let c = 0;

    const f = async () => {
      this._wg.removeAll().add(
        $("span").style("font-size:12px").html(String(c) + " / " + String(n))
      );
      if (profits === null) {
        this._wg.removeAll()
          .add(Ui.link(() => { this.download() })
            .add(Ui.img("download").style("vertical-align:middle")))
          .add($("span").html("&nbsp;&nbsp;&nbsp;&nbsp;"))
          .add(Ui.img("ledWrong"));
      } else if (c === n) {
        portfolio.forEach(e => {
          profits += (e[3] - e[2]) * e[1];
        });
        const last = {};
        portfolio.forEach(e => {
          last[e[0]] = e[3];
        });
        const rq = {
          "source": "downloader",
          "rq": "historic",
          "date": DateDm.now().toBase(),
          "profits": Math.round(profits * 100) / 100,
          "last": last
        };
        await this._main.client.send(rq);
        this._wg.removeAll()
          .add(Ui.link(() => { this.download() })
            .add(Ui.img("download").style("vertical-align:middle")))
          .add($("span").html("&nbsp;&nbsp;&nbsp;&nbsp;"))
          .add(Ui.img("ledOk"));
        this._main.run();
      } else {
        try {
          const e = portfolio[c];
          const rq = {
            "source": "downloader",
            "rq": "download",
            "nick": e[0]
          };

          const rp = await this._main.client.send(rq);
          e[3] = rp["quote"];

          if (e[3] < 0) {
            profits = null;
          }

          ++c;
          f();
        } catch (e) {
          profits = null;
          ++c;
          f();
        }
      }
    };
    f();
  }

}

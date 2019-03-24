// Copyright 04-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Captcha from "../dmjs/Captcha.js";
import Store from "../dmjs/Store.js";
// eslint-disable-next-line
import Domo from "../dmjs/Domo.js";
import Ui from "../dmjs/Ui.js";
import Main from "../Main.js";
import {I18n, _} from "../I18n.js";

const $ = Ui.$;

/** Authentication pages. */
export default class Auth {
  /**
   * @param {!Main} main Main page
   */
  constructor (main) {
    /**
     * @private
     * @type {!Main}
     */
    this._main = main;
  }

  /**
   * @return {void}
   */
  show () {
    const main = this._main;
    const captchaStore = Main.captchaAuthStore;

    if (Store.take(Main.langStore) === "en") I18n.en();
    else I18n.es();

    /** @const {!Domo} */
    const user = Ui.field("pass");
    /** @const {!Domo} */
    const pass = Ui.pass("accept").att("id", "pass");
    /** @const {!Domo} */
    const persistent = $("input")
      .att("type", "checkbox")
      .style("vertical-align: middle");
    /** @const {!Domo} */
    const accept = $("input")
      .att("type", "button")
      .style("width:90px;")
      .att("id", "accept")
      .value(_("Accept"));

    /** @type {!Captcha} */
    const captcha = new Captcha(captchaStore, 3);

    /**
     * Change selected language
     * @return {void}
     */
    const changeLanguage = () => {
      Store.put(
        Main.langStore,
        Store.take(Main.langStore) === "en" ? "es" : "en"
      );
      main.run();
    };

    /**
     * Accept button pressed
     * @param {string} user User
     * @param {string} pass Password
     * @param {boolean} persistent If is 'true' connection lasts until its
     *    maximum.
     * @return {Promise}
     */
    const faccept = async (user, pass, persistent) => {
      if (user === "") {
        alert(_("User name is missing"));
        return;
      }
      if (pass === "") {
        alert(_("Password is missing"));
        return;
      }

      const ok = await main.client.authentication(user, pass, !persistent);

      if (ok) {
        captcha.resetCounter();
      } else {
        captcha.incCounter();
      }
      main.start();
    };

    /**
     * @private
     * @return {!Domo} Page body
     */
    function body () {
      const counter = captcha.counter();
      const counterLimit = captcha.counterLimit();

      accept.on("click", () => {
        if (counter > counterLimit && !captcha.match()) {
          alert(_("Grey squares checks are wrong"));
          main.run();
          return;
        }
        faccept(
          user.value().trim(),
          pass.value().trim(),
          persistent.checked()
        );
      });

      const rows = [
        $("tr")
          .add($("td")
            .style("padding: 10px 0px 0px 10px;text-align:right;")
            .html(_("User")))
          .add($("td").style("padding: 10px 10px 0px 10px;").add(user)),
        $("tr")
          .add($("td")
            .style("padding: 10px 0px 0px 10px;text-align:right;")
            .html(_("Password")))
          .add($("td").style("padding: 10px 10px 5px 10px;").add(pass)),
        $("tr")
          .add($("td")
            .att("colspan", 2)
            .style("border-top:1px solid #c9c9c9;" +
              "padding: 5px 10px 10px;text-align:right;")
            .add($("table")
              .style(
                "border-collapse : collapse;" +
                "border : 0px;" +
                "width : 100%;")
              .add($("tr")
                .add($("td").att("align", "center").att("colspan", 2)
                  .add(persistent)
                  .add($("span").html("&nbsp;" + _("Keep connected")))))
              .add($("tr")
                .add($("td")
                  .add(Ui.link(changeLanguage).att("class", "link")
                    .html(Store.take(
                      Main.langStore) === "en" ? "ES" : "EN")))
                .add($("td").att("align", "right").add(accept)))))
      ];

      if (counter > 0) {
        rows.push(
          $("tr")
            .add($("td")
              .att("colspan", 2)
              .style("border-top:1px solid #c9c9c9;" +
                "padding: 10px 10px 10px;text-align:right;")
              .add($("table")
                .att("align", "center")
                .style("background-color: rgb(250, 250, 250);" +
                  "border: 1px solid rgb(110,130,150);" +
                  "font-family: sans;font-size: 14px;" +
                  "padding: 4px;border-radius: 4px;")
                .add($("tr")
                  .add($("td").html(_("Wrong password"))))))
        );
      }

      if (counter > counterLimit) {
        rows.push(
          $("tr")
            .add($("td").att("colspan", 2).att("align", "center")
              .add(captcha.make()))
        );
        rows.push(
          $("tr")
            .add($("td")
              .att("colspan", 2)
              .style("padding: 5px 0px 5px 10px;text-align:center;")
              .html(_("Check gray squares")))
        );
      }

      return $("table")
        .att("align", "center")
        .style(
          "background-color: #f8f8f8;" +
          "border-collapse: collapse;" +
          "padding: 10px;" +
          "border: 1px solid rgb(110,130,150);")
        .add($("tr")
          .add($("td")
            .att("colspan", 2)
            .style(
              "background-color:#e8e8e8;" +
              "border-bottom:1px solid #c9c9c9;" +
              "padding: 10px;" +
              "color:#505050;"
            )
            .html("<big><big><b>" + _("Login") + "</big></big></b>")))
        .adds(rows);
    }

    main.dom.showRoot(
      $("div")
        .add($("div").klass("title").html(
          `&nbsp;<br>${Main.app}<br>&nbsp;`))
        .add($("div").add(body()))
    );
    user.e.focus();
  }
}


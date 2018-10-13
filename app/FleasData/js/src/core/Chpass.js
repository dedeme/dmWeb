// Copyright 04-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Main from "../Main.js";
import {_} from "../I18n.js";
// eslint-disable-next-line
import Ui from "../dmjs/Ui.js";
import Client from "../dmjs/Client.js";
import Captcha from "../dmjs/Captcha.js";

const $ = Ui.$;

/** Change password page. */
export default class Chpass {
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
   * Changes password,
   * @param {string} pass Current password.
   * @param {string} newPass New password.
   * @param {function (boolean):void} f Captcha management.
   * @return {Promise}
   */
  async changePass (pass, newPass, f) {
    const main = this._main;
    const rq = {
      "page": "chpass",
      "user": main.client.user(),
      "pass": Client.crypPass(pass),
      "newPass": Client.crypPass(newPass)
    };
    const rp = await main.client.send(rq);
    const ok = rp["ok"];
    f(ok);
    if (ok) {
      alert(_("Password successfully changed"));
      main.run();
    } else {
      main.go(Main.settingsPageId, "");
    }
  }

  // ____
  // View ------------------------------------------------------------
  // TTTT

  /**
   * @return {void}
   */
  show () {
    const self = this;
    const main = self._main;
    const captchaStore = Main.captchaChpassStore;

    const pass = Ui.pass("newPass");
    const newPass = Ui.pass("newPass2").att("id", "newPass");
    const newPass2 = Ui.pass("accept").att("id", "newPass2");
    const accept = $("input")
      .att("type", "button")
      .style("width:90px;")
      .att("id", "accept")
      .value(_("Accept"));
    const cancel = $("input")
      .att("type", "button")
      .style("width:90px;")
      .value(_("Cancel"));

    const captcha = new Captcha(captchaStore, 3);

    function body () {
      const counter = captcha.counter();
      const counterLimit = captcha.counterLimit();

      accept.on("click", () => {
        const passv = pass.value().trim();
        const newPassv = newPass.value().trim();
        const newPass2v = newPass2.value().trim();

        if (counter > counterLimit && !captcha.match()) {
          alert(_("Grey squares checks are wrong"));
          return;
        }
        if (passv === "") {
          alert(_("Current password is missing"));
          pass.value("");
          pass.e.focus();
          return;
        }
        if (newPassv === "") {
          alert(_("New password is missing"));
          newPass.value("");
          newPass.e.focus();
          return;
        }
        if (newPass2v === "") {
          alert(_("Confirm password is missing"));
          newPass2.value("");
          newPass2.e.focus();
          return;
        }
        if (newPassv !== newPass2v) {
          alert(_("New password and confirm password do not match"));
          newPass.value("");
          newPass2.value("");
          newPass.e.focus();
          return;
        }

        self.changePass(passv, newPassv, ok => {
          if (ok) {
            captcha.resetCounter();
          } else {
            captcha.incCounter();
          }
        });
      });

      cancel.on("click", () => {
        main.run();
      });

      const rows = [
        $("tr")
          .add($("td")
            .att("style", "padding: 10px 0px 0px 10px;text-align:right;")
            .html(_("Current password")))
          .add($("td").att("style", "padding: 10px 10px 0px 10px;")
            .add(pass)),
        $("tr")
          .add($("td")
            .att("style", "padding: 5px 0px 0px 10px;text-align:right;")
            .html(_("New password")))
          .add($("td").att("style", "padding: 5px 10px 0px 10px;")
            .add(newPass)),
        $("tr")
          .add($("td")
            .att("style", "padding: 5px 0px 10px 10px;text-align:right;")
            .html(_("Confirm password")))
          .add($("td").att("style", "padding: 5px 10px 10px 10px;")
            .add(newPass2)),
        $("tr")
          .add($("td")
            .att("colspan", 2)
            .att("style",
              "border-top:1px solid #c9c9c9;" +
              "padding: 10px 10px 10px;text-align:right;")
            .add($("span")
              .add(cancel)
              .add($("span").text("  "))
              .add(accept)))
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
                  .add($("td").html(_("Fail trying to change password"))))))
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
    pass.e.focus();
  }
}

// Copyright 24-Jul-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Main from "./Main.js";
import {_} from "./I18n.js";
import Domo from "./dmjs/Domo.js"; //eslint-disable-line
import Client from "./dmjs/Client.js";
import Ui from "./dmjs/Ui.js";
import Captcha from "./dmjs/Captcha.js";

const $ = Ui.$;

/** Change password page. */
export default class Chpass {

  /**
   * @param {!Main} main Main page
   */
  constructor (main) {

    this._main = main;

    // VIEW --------
    // TTTTTTTTTTTTT

    this._captcha = new Captcha(Main.captchaChpassStore, 3);

    this._pass = Ui.pass("newPass");

    this._newPass = Ui.pass("newPass2").att("id", "newPass");

    this._newPass2 = Ui.pass("accept").att("id", "newPass2");

    this._accept = $("input")
      .att("type", "button")
      .style("width:110px;")
      .att("id", "accept")
      .value(_("Accept"))
      .on("click", this.changePass.bind(this));

    this._cancel = $("input")
      .att("type", "button")
      .style("width:110px;")
      .value(_("Cancel"))
      .on("click", main.update.bind(main));
  }

  // VIEW ----------------------------------------
  // TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

  /**
   * @private
   * @return {!Domo}
   */
  get wg () {
    const rows = [
      $("tr")
        .add($("td")
          .att("style", "padding: 10px 0px 0px 10px;text-align:right;")
          .html(_("Current password")))
        .add($("td").att("style", "padding: 10px 10px 0px 10px;")
          .add(this._pass)),
      $("tr")
        .add($("td")
          .att("style", "padding: 5px 0px 0px 10px;text-align:right;")
          .html(_("New password")))
        .add($("td").att("style", "padding: 5px 10px 0px 10px;")
          .add(this._newPass)),
      $("tr")
        .add($("td")
          .att("style", "padding: 5px 0px 10px 10px;text-align:right;")
          .html(_("Confirm password")))
        .add($("td").att("style", "padding: 5px 10px 10px 10px;")
          .add(this._newPass2)),
      $("tr")
        .add($("td")
          .att("colspan", 2)
          .att("style",
            "border-top:1px solid #c9c9c9;" +
            "padding: 10px 10px 10px;text-align:right;")
          .add($("span")
            .add(this._cancel)
            .add($("span").text("  "))
            .add(this._accept)))
    ].concat(this._captcha.counter() > 0
      ? [
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
                .add($("td").html(_("Fail trying to change password"))))))]
      : []
    ).concat(this._captcha.counter() > this._captcha.counterLimit()
      ? [
        $("tr")
          .add($("td").att("colspan", 2).att("align", "center")
            .add(this._captcha.make())),

        $("tr")
          .add($("td")
            .att("colspan", 2)
            .style("padding: 5px 0px 5px 10px;text-align:center;")
            .html(_("Check gray squares")))]
      : []
    );

    const body = $("table")
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

    return $("div")
      .add($("div").klass("title").html(
        `&nbsp;<br>${Main.app}<br>&nbsp;`))
      .add($("div").add(body))
    ;
  }

  /**
   * @return {void}
   */
  show () {
    this._main.view.removeAll().add(this.wg);
    this._pass.e.focus();
  }

  // CONTROL -------------------------------------
  // TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

  /**
   * Changes password
   * @private
   * @return {Promise}
   */
  async changePass () {
    const passv = this._pass.value().trim();
    const newPassv = this._newPass.value().trim();
    const newPass2v = this._newPass2.value().trim();

    if (
      this._captcha.counter() > this._captcha.counterLimit() &&
      !this._captcha.match()
    ) {
      alert(_("Grey squares checks are wrong"));
      return;
    }
    if (passv === "") {
      alert(_("Current password is missing"));
      this._pass.value("");
      this._pass.e.focus();
      return;
    }
    if (newPassv === "") {
      alert(_("New password is missing"));
      this._newPass.value("");
      this._newPass.e.focus();
      return;
    }
    if (newPass2v === "") {
      alert(_("Confirm password is missing"));
      this._newPass2.value("");
      this._newPass2.e.focus();
      return;
    }
    if (newPassv !== newPass2v) {
      alert(_("New password and confirm password do not match"));
      this._newPass.value("");
      this._newPass2.value("");
      this._newPass.e.focus();
      return;
    }

    const rq = {
      "rq": "chpass",
      "user": Main.client.user(),
      "pass": Client.crypPass(passv),
      "newPass": Client.crypPass(newPassv)
    };
    const rp = await Main.client.send(rq);
    const ok = rp["ok"];
    if (ok) {
      this._captcha.resetCounter();
      alert(_("Password successfully changed"));
      this._main.update();
    } else {
      this._captcha.incCounter();
      new Chpass(this._main).show();
    }
  }

}

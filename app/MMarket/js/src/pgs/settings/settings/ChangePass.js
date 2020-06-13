// Copyright 28-Feb-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Captcha from "../../../dmjs/Captcha.js";
import Client from "../../../dmjs/Client.js"; //eslint-disable-line
import Domo from "../../../dmjs/Domo.js"; //eslint-disable-line
import Ui from "../../../dmjs/Ui.js";
import Cts from "../../../data/Cts.js";
import {_} from "../../../I18n.js";
import Msg from "../../../wgs/Msg.js";

const $ = e => Ui.$(e);

/**
    Change password page.
**/
export default class ChangePass {
  /**
      @param {!Domo} wg
      @param {function():void} outPage
  **/
  constructor (wg, outPage) {
    this._wg = wg;
    this._outPage = outPage;
    this._failed = false;

    this._captcha = new Captcha(`${Cts.appName}__captcha`);
    this._oldPass = Ui.pass("newPass");

    this.view();
  }

  // View ----------------------------------------------------------------------

  /**
      @private
      @return {void}
  **/
  view () {
    const oldPass = this._oldPass;
    oldPass.value("");
    const newPass = Ui.pass("newPass2").att("id", "newPass");
    const newPass2 = Ui.pass("acceptBt").att("id", "newPass2");
    const cancelBt = $("button")
      .on("click", () => { this.cancel() })
      .text(_("Cancel"));
    const acceptBt = $("button")
      .att("id", "acceptBt")
      .on("click", () => {
        this.send(
          oldPass.value().trim(),
          newPass.value().trim(),
          newPass2.value().trim()
        );
      })
      .text(_("Accept"));

    const rows = [
      $("tr")
        .add($("td")
          .style("padding: 10px 0px 0px 10px;text-align:right;")
          .html(_("Current password")))
        .add($("td").style("padding: 10px 10px 0px 10px;")
          .add(oldPass)),
      $("tr")
        .add($("td")
          .style("padding: 5px 0px 0px 10px;text-align:right;")
          .html(_("New password")))
        .add($("td")
          .style("padding: 5px 10px 0px 10px;")
          .add(newPass)),
      $("tr")
        .add($("td")
          .style("padding: 5px 0px 10px 10px;text-align:right;")
          .html(_("New password")))
        .add($("td")
          .style("padding: 5px 10px 10px 10px;")
          .add(newPass2)),
      $("tr")
        .add($("td")
          .att("colspan", 2)
          .style(`border-top:1px solid #c9c9c9;
                  padding: 10px 10px 10px;text-align:right;`)
          .add($("span")
            .add(cancelBt))
          .add($("span")
            .text("  "))
          .add($("span")
            .add(acceptBt)))
    ];

    if (this._failed) {
      rows.push(
        $("tr")
          .add($("td")
            .att("colspan", 2)
            .style(`border-top:1px solid #c9c9c9;
                    padding: 10px 10px 10px;text-align:right;`)
            .add($("table")
              .att("align", "center")
              .style(`background-color: rgb(250, 250, 250);
                      border: 1px solid rgb(110,130,150);
                      font-family: sans;font-size: 14px;
                      padding: 4px;border-radius: 4px;`)
              .add($("tr")
                .add($("td")
                  .html(_("Wrong password")))))))
      ;
    }

    if (this._captcha.isUpLimit()) {
      rows.push(
        $("tr")
          .add($("td")
            .att("colspan", 2)
            .att("align", "center")
            .add(this._captcha.wg))
      );
      rows.push(
        $("tr")
          .add($("td")
            .att("colspan", 2)
            .style("padding: 5px 0px 5px 10px;text-align:center;")
            .html(_("Check gray squares")))
      );
    }

    this._wg.removeAll()
      .add($("div")
        .style("padding-bottom:20px;")
        .klass("head")
        .text(Cts.appName))
      .add($("table")
        .att("align", "center")
        .style(`background-color: #f8f8f8;
                border-collapse: collapse;
                padding: 10px;
                border: 1px solid rgb(110,130,150);`)
        .add($("tr")
          .add($("td")
            .att("colspan", 2)
            .style(`background-color:#e8e8e8;
                    border-bottom:1px solid #c9c9c9;
                    padding: 10px;
                    color:#505050;`)
            .html(
              "<big><big><b>" + _("Password Change") + "</big></big></b>"
            )))
        .adds(rows));

    oldPass.e.focus();
  }

  // Control -------------------------------------------------------------------

  /**
      Set focus.
      @return void
  **/
  focus () {
    this._oldPass.e.focus();
  }

  /**
      @private
      @param {string} oldPass
      @param {string} newPass
      @param {string} newPass2
      return !Promise<void>
  **/
  async send (oldPass, newPass, newPass2) {
    const captcha = this._captcha;
    if (oldPass === "") {
      Msg.error(_("Current password is missing"));
      this.view();
      return;
    }
    if (newPass === "") {
      Msg.error(_("New password is missing"));
      this.view();
      return;
    }
    if (newPass2 === "") {
      Msg.error(_("Confirm password is missing"));
      this.view();
      return;
    }
    if (newPass !== newPass2) {
      Msg.error(_("New password and confirm password do not match"));
      this.view();
      return;
    }

    if (captcha.isUpLimit() && !captcha.check()) {
      Msg.error(_("Grey squares checks are wrong"));
      this._captcha = new Captcha(`${Cts.appName}__captcha`);
      this.view();
      return;
    }

    const rp = await Cts.client.send({
      "module": "settings",
      "source": "settings",
      "rq": "changePass",
      "user": Cts.client.user,
      "old": Client.crypPass(oldPass),
      "new": Client.crypPass(newPass),
    });
    const ok = rp["ok"];

    if (ok) {
      captcha.reset();
      Msg.ok(_("Password successfully changed"));
      this._outPage();
    } else {
      this._failed = true;
      captcha.increment();
      this._captcha = new Captcha(`${Cts.appName}__captcha`);
      this.view();
    }
  }

  /**
      @private
  **/
  cancel () {
    this._outPage();
  }
}


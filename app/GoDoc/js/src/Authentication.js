// Copyright 30-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Captcha from "./dmjs/Captcha.js";
import Client from "./dmjs/Client.js"; //eslint-disable-line
import Store from "./dmjs/Store.js";
import Domo from "./dmjs/Domo.js"; //eslint-disable-line
import Ui from "./dmjs/Ui.js";
import {I18n, _} from "./I18n.js";

const $ = e => Ui.$(e);

function getLang (app) {
  return Store.take(`${app}__lang`) || "es";
}

function setLang (app, lang) {
  Store.put(`${app}__lang`, lang);
}

/**
    Authentication page.
**/
export default class Authentication {
  /**
      @param {string} app
      @param {Client} client
  **/
  constructor (app, client) {
    this._app = app;
    this._client = client;
    this._lang = getLang(app);
    this._failed = false;

    this._captcha = new Captcha(`${app}__captcha`);
    this._pass = Ui.pass("accept").att("id", "pass");
    this._wg = $("div");

    this.view();
  }

  /**
      @return {!Domo}
  **/
  get wg () {
    return this._wg;
  }

  /**
      @return {!Domo}
  **/
  get pass () {
    return this._pass;
  }

  /**
      @return {string}
  **/
  get lang () {
    return this._lang;
  }

  // View ----------------------------------------------------------------------

  /**
      @private
      @return {void}
  **/
  view () {
    if (this._lang === "en") I18n.en();
    else I18n.es();

    const userIn = Ui.field("pass").att("id", "userIn").value("admin");
    const pass = this._pass.value("");
    const persistent = $("input")
      .att("type", "checkbox")
      .style("vertical-align: middle")
      .checked(true);
    const accept = $("button")
      .att("id", "accept")
      .on("click", () => {
        this.send(
          userIn.value().trim(),
          pass.value().trim(),
          !persistent.checked()
        );
      })
      .text(_("Accept"));

    const rows = [
      $("tr")
        .add($("td")
          .style("padding: 10px 0px 0px 10px;text-align:right;")
          .html(_("User")))
        .add($("td").style("padding: 10px 10px 0px 10px;").add(userIn)),
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
                .add(Ui.link(() => { this.changeLanguage() })
                  .att("class", "link")
                  .html(this._lang === "en" ? "ES" : "EN")))
              .add($("td").att("align", "right").add(accept)))))
    ];

    if (this._failed) {
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

    if (this._captcha.isUpLimit()) {
      rows.push(
        $("tr")
          .add($("td").att("colspan", 2).att("align", "center")
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

    this._wg
      .removeAll()
      .add($("div").klass("head").html(`&nbsp;<br>${this._app}<br>&nbsp;`))
      .add($("table")
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
        .adds(rows));

    pass.e.focus();
  }

  // Control -------------------------------------------------------------------

  /**
      @private
      @return {void}
  **/
  changeLanguage () {
    const lang = this._lang === "es" ? "en" : "es";
    setLang(this._app, lang);
    this._lang = lang;
    this.view();
  }

  /**
      @private
      @param {string} user
      @param {string} pass
      @param {boolean} expire
      @return {Promise<void>}
  **/
  async send (user, pass, expire) {
    const captcha = this._captcha;
    if (user === "") {
      alert(_("User name is missing"));
      return;
    }
    if (pass === "") {
      alert(_("Password is missing"));
      return;
    }

    if (captcha.isUpLimit() && !captcha.check()) {
      alert(_("Grey squares checks are wrong"));
      this._captcha = new Captcha(`${this._app}__captcha`);
      this.view();
      return;
    }

    const ok = await this._client.authentication(user, pass, expire);

    if (ok) {
      captcha.reset();
      location.reload();
    } else {
      this._failed = true;
      captcha.increment();
      this._captcha = new Captcha(`${this._app}__captcha`);
      this.view();
    }
  }
}


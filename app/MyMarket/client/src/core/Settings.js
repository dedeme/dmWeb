// Copyright 05-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

//eslint-disable-next-line
import Main from "../Main.js";
import Cts from "../Cts.js";
import {_, _args} from "../I18n.js";
import Ui from "../dmjs/Ui.js";

const $ = Ui.$;

/** Settings page. */
export default class Settings {
  /**
   * @param {!Main} main Main
   */
  constructor (main) {
    /**
     * @private
     * @type {!Main}
     */
    this._main = main;

    this._lang = "es";
    this._ueditor = $("div");
    this._udata = $("div");
    this._yeditor = $("div");
    this._ydata = $("div");
  }

  /**
   * @param {string} lang It can be "en" or "es"
   * @return {Promise}
   */
  async setLang (lang) {
    const main = this._main;
    const rq = {
      "page": "settings",
      "rq": "setLang",
      "lang": lang
    };
    await main.client.send(rq);
    this.show();
  }

  async addUser (user, userData) {
    if (user === "") {
      alert(_("User name is missing"));
      return;
    }
    if (userData.includes(user)) {
      alert(_("User name is duplicated"));
      return;
    }
    const data = {
      "page": "settings",
      "rq": "addUser",
      "user": user
    };
    await this._main.client.send(data);
    this.show();
  }

  async removeUser (user) {
    if (confirm(_args(_("Remove user '%0'"), user))) {
      const data = {
        "page": "settings",
        "rq": "removeUser",
        "user": user
      };
      await this._main.client.send(data);
      this.show();
    }
  }

  async modifyUser (old_user, user, userData) {
    if (user === "") {
      alert(_("User name is missing"));
      return;
    }
    if (old_user === user) {
      return;
    }
    if (userData.includes(user)) {
      alert(_("User name is duplicated"));
      return;
    }
    const data = {
      "page": "settings",
      "rq": "modifyUser",
      "old_user": old_user,
      "user": user
    };
    await this._main.client.send(data);
    this.show();
  }

  async selectUser (user) {
    const data = {
      "page": "settings",
      "rq": "selectUser",
      "user": user
    };
    await this._main.client.send(data);
    this._main.run(Cts.SETTINGS_PAGE_ID);
  }

  // ____
  // View ------------------------------------------------------------
  // TTTT

  newUser (userData) {
    const self = this;
    self._ueditor.removeAll().add($("div")
      .add($("div").html(_("New user:")))
      .add($("div")
        .add($("input").style("width:120px").att("id", "userInput")))
      .add($("div").style("text-align:right")
        .add($("span")
          .add(Ui.link(() => {
            self.show();
          }).klass("link").html("Cancel")))
        .add($("span").html("&nbsp;&nbsp;|&nbsp;&nbsp;"))
        .add($("span")
          .add(Ui.link(() => {
            self.addUser($("#userInput").value().trim(), userData);
          }).klass("link").html("Accept")))))
    ;
  }

  modUser (user, userData) {
    const self = this;
    self._ueditor.removeAll().add($("div")
      .add($("div").html(_args(_("Modify user '%0':"), user)))
      .add($("div")
        .add($("input").style("width:120px").att("id", "userInput")))
      .add($("div").style("text-align:right")
        .add($("span")
          .add(Ui.link(() => {
            self.show();
          }).klass("link").html("Cancel")))
        .add($("span").html("&nbsp;&nbsp;|&nbsp;&nbsp;"))
        .add($("span")
          .add(Ui.link(() => {
            self.modifyUser(user, $("#userInput").value().trim(), userData);
          }).klass("link").html("Accept")))))
    ;
  }

  setUsersData (userSelected, userData) {
    const self = this;
    userData.sort();
    self._udata.removeAll().add($("table")
      .adds(userData.map(id =>
        $("tr")
          .add($("td")
            .add(userSelected === id
              ? Ui.img("delete2")
              : Ui.link(() => {
                self.removeUser(id);
              }).add(Ui.img("delete"))))
          .add($("td")
            .add(userSelected === id
              ? Ui.img("edit2")
              : Ui.link(() => {
                self.modUser(id, userData);
              }).add(Ui.img("edit"))))
          .add($("td")
            .add(userSelected === id
              ? Ui.img("star")
              : Ui.link(() => {
                self.selectUser(id);
              }).add(Ui.img("star2"))))
          .add($("td")
            .add($("span").html(id)))
      )));
  }

  setYearsData (yearSelected, yearData) {
    this._ydata.removeAll().add($("table")
      .adds(yearData.map(id =>
        $("tr")
          .add($("td")
            .add(Ui.link(() => {
              alert("Delete");
            }).add(Ui.img("delete"))))
          .add($("td")
            .add(Ui.link(() => {
              alert("Modify");
            }).add(Ui.img("edit"))))
          .add($("td")
            .add(yearSelected === id
              ? Ui.img("star")
              : Ui.link(() => {
                alert("select");
              }).add(Ui.img("star2"))))
          .add($("td")
            .add($("span").html(id)))
      )));
  }

  body (userData, yearData) {
    const self = this;

    return $("div")
      .add($("div").klass("head").style("padding-bottom:0px")
        .html(_("Language")))
      .add($("table").att("align", "center")
        .add($("tr")
          .add($("td").klass("frame")
            .add(Ui.link(() => {
              self.setLang(this._lang === "es" ? "en" : "es");
            })
              .klass("link").html(_args(
                _("Change Language to %0"),
                this._lang === "es" ? "EN" : "ES"
              ))))))
      .add($("div").klass("head").html(_("Users")))
      .add($("table").att("align", "center").klass("frame")
        .add($("tr")
          .add($("td")
            .add(Ui.link(() => {
              this.newUser(userData);
            }).klass("link").html(_("new")))))
        .add($("tr")
          .add($("td")
            .add($("hr"))))
        .add($("tr")
          .add($("td")
            .add(this._ueditor)))
        .add($("tr")
          .add($("td")
            .add($("hr"))))
        .add($("tr")
          .add($("td")
            .add(this._udata))))
      .add($("div").klass("head")
        .html(_("Accounting-year")))
      .add($("table").att("align", "center").klass("frame")
        .add($("tr")
          .add($("td")
            .add(Ui.link(() => {
              alert("new");
            }).klass("link").html(_("new")))))
        .add($("tr")
          .add($("td")
            .add($("hr"))))
        .add($("tr")
          .add($("td")
            .add(this._yeditor)))
        .add($("tr")
          .add($("td")
            .add($("hr"))))
        .add($("tr")
          .add($("td")
            .add(this._ydata))))
    ;
  }

  /**
   * @return {Promise<?>}
   */
  async show () {
    const data = {
      "page": "settings",
      "rq": "init"
    };
    const rp = await this._main.client.send(data);
    const userData = rp["users"];
    const yearData = rp["years"];

    this._main.dom.show("settings", this.body(userData, yearData));

    this._ueditor.removeAll();
    this.setUsersData(this._main.user, userData);
    this._yeditor.removeAll();
    this.setYearsData(this._main.year, yearData);
  }
}


// Copyright 09-07-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

goog.provide("Settings");
goog.require("user_Chpass");

Settings = class {
  /**
   * @param {!Main} main
   * @param {string} lang
   * @return {!Settings}
   */
  constructor (main, lang) {
    this._main = main;
    this._lang = lang;
    this._div = $("div");
  }

  /** @return {!Domo} */
  div () {
    return this._div;
  }

  /** @return {!Client} */
  client () {
    return this._main.client();
  }

  changeLang () {
    const self = this;
    const main = this._main;
    const client = main.client();
    const lang = self._lang === "en" ? "es" : "en";
    const data = {"page": "settings", "rq": "setLang", "lang": lang};
    client.send(data, rp => {
      main.run();
    });
  }

  changePassPage () {
    new user_Chpass(this).mk();
  }

/* View --------------------------------------------------------- */

  /**
   * @return {!Domo}
   */
  mk() {
    const self = this;
    const main = self._main;
    return self._div.removeAll().style("text-align:center")
      .add($("h2").html(_("Settings")))
      .add($("table").att("align", "center").add($("tr").add($("td")
        .klass("frame")
        .add($("p")
          .add($("span").html(_("Change language to") + ": "))
          .add(Ui.link(ev => { self.changeLang(); })
            .klass("link")
            .html(self._lang === "en" ? "ES": "EN")))
        .add($("p")
          .add(Ui.link(ev => { self.changePassPage(); })
            .klass("link").html(_("Change password")))))))
    ;
  }
}

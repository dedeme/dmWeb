// Copyright 24-Sep-2017 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

goog.provide("view_Settings");

view_Settings = class {
  /**
   * @param {!Main} control
   */
  constructor (control) {
    /** @private */
    this._control = control;
  }

  /**
   * @return {void}
   */
  show () {
    const control = this._control;
    control.dom().show("settings", $("div").style("text-align:center")
      .add($("h2").html(_("Settings")))
      .add($("table").att("align", "center").add($("tr").add($("td")
        .klass("frame")
        .add($("p")
          .add($("span").html(_("Change language to") + ": "))
          .add(Ui.link(ev => { control.changeLang(); })
            .klass("link")
            .html(control.conf().language() == "en" ? "ES": "EN")))
        .add($("p")
          .add(Ui.link(ev => { control.changePassPage(); })
            .klass("link").html(_("Change password")))))))
    );
  }
}


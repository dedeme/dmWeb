// Copyright 12-Nov-2017 ºDeme
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
    const conf = control.conf();

    const langDiv = $("div")
      .add(Ui.link(ev => { control.changeLanguage() })
        .klass("link").html(_args(
          _("Change Language to %0"),
          conf.lang() === "es" ? "EN" : "ES"
        )));
    const passDiv = $("div")
      .add(Ui.link(ev => { control.changePassPage() })
        .klass("link").html(_("Change Password")));

    const opts = [
        langDiv,
        $("p").html("<p></p>"),
        passDiv
      ]

    control.dom().show("settings", $("div").style("text-align:center")
      .add($("h2").html(_("Settings")))
      .add($("table").att("align", "center").add($("tr").add($("td")
        .klass("frame")
        .addIt(It.from(opts)))))
    );
  }
}


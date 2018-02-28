// Copyright 8-Dec-2017 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

goog.provide("view_Run");

view_Run = class {
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

    control.dom().show("run", $("div").style("text-align:center")
      .add($("h2").html(_("Run")))
      .add($("table").att("align", "center").add($("tr").add($("td")
        .klass("frame")
        .add($("span").html("run")))))
    );
  }
}


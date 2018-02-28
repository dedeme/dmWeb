// Copyright 12-Nov-2017 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

goog.provide("user_Expired");

user_Expired = class {
  /**
   * @param {!Main} control
   */
  constructor (control) {
    /** @private */
    this._control = control;
  }

  /** @return {void} */
  show () {
    const td = "padding:0px 10px 0px 10px;";
    const link = "<a href=''>" + _("here") + "</a>";
    const w = $("div")
      .add($("p").att("style", "text-align:center")
        .html("<big><b>" + Main.app() + "</b></big>"))
      .add($("table").att("class", "main")
        .add($("tr")
          .add($("td")
            .add($("table")
              .att("class", "border")
              .att("width", "100%")
              .att("style",
                "background-color: #f8f8f8;" +
                "border-collapse: collapse;")
              .add($("tr")
                .add($("td")
                  .att("style", td)
                  .html("<p>" + _("Session is expired.") + "<p>" +
                    "<p><b>" +
                    _args(_("Click %0 to continue."),
                      link) + "</b></p>")))))));
    this._control.dom().showRoot(w);
  }
}

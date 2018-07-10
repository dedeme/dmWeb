// Copyright 24-Sep-2017 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Good bye page
goog.provide("Bye");

Bye = class {
  /**
   * @param {!Main} control
   */
  constructor ( control ) {
    /** @private */
    this._control = control;
  }

  /**
   * @return {void}
   */
  show () {
    this._control.dom().showRoot(
      $("div")
        .add($("div").klass("title")
          .html("&nbsp;<br>" + Main.app() + "<br>&nbsp;"))
        .add($("div")
          .add($("table")
            .att("class", "border")
            .att("width", "100%")
            .att("style",
              "background-color: #f8f8f8;" +
              "border-collapse: collapse;")
            .add($("tr")
              .add($("td")
                .att("style", "padding:0px 10px 0px 10px;")
                .html(_args(_("Logout-message"), Main.app()))))))
    );
  }
}

// Copyright 13-Oct-2017 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

goog.provide("view_Year");

view_Year = class {
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
    const year = conf.year();

    function years() {
      function td(y) {
        return $("td").add(
          y === year
            ? $("span").klass("frame").html("·" + y + "·")
            : Ui.link(ev => { control.changeYear(y); })
                .klass("link").html("·" + y + "·")
        );
      }
      return $("tr").add($("td").att("colspan", 2).style("text-align:center;")
        .add($("table").att("align", "center").add($("tr")
          .addIt(It.from(conf.years()).map(y => td(y)))
        )));
    }

    function close() {
      return $("tr").add($("td").att("colspan", 2)
        .add($("button").html(_("Close year"))
          .on("click", ev => {
              if (confirm(
                _("This operation only can be manually undone.\nContinue?")
              )) {
                control.closeYear();
              }
            })));
    }

    control.dom().show("year",  $("table")
      .style("width:100%;text-align:center")
      .add($("tr").add($("td").att("colspan", 2)
        .html("<b>" + _("Year") + "<b>")))
      .add($("tr")
        .add($("td").style("width:5px;white-space: nowrap;text-align:right")
          .html(_("Change")))
        .add($("td").add($("hr"))))
      .add(years())
      .add($("tr")
        .add($("td").style("width:5px;white-space: nowrap;text-align:right")
          .html(_("Close")))
        .add($("td").add($("hr"))))
      .add(close())
    );
  }
}

// Copyright 20-Jan-2017 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

goog.provide("view_FleasTable");

view_FleasTable = class {
  /**
   * @param {!Main} control
   * @param {!Array<!Flea>} fleas
   */
  constructor (control, fleas) {
    this._control = control;
    this._fleas = fleas;
    this._order = "incomes";
  }

  make () {
    const self = this;
    const conf = self._control.conf();

    function intFormat (n) {
      return conf.fDec(new Dec(n, 0))
    }
    function floatFormat (n) {
      return conf.fDec(new Dec(n, 2))
    }

    function th() {
      return $("td").klass("frame4").style("font-family:monospace;");
    }
    function thl() {
      return $("td").klass("frame4")
        .style("text-align:right;font-family:monospace;");
    }
    function td() {
      return $("td").klass("frame");
    }
    function tdl() {
      return $("td").klass("frame").style("text-align:right");
    }

    /**
     * @param {Flea} f
     * @param {number} span
     */
    function specialCols (f, span) {
      if (f === null) {
        return It.from([td().att("colspan", span)]);
      }
      return f.extra().bests()(intFormat, floatFormat, span);
    }

    let span = 1;
    It.from(self._fleas).each(f => {
      if (f != null && f.extra().fields() > span) {
        span = f.extra().fields();
      }
    })
    const thSpecialData = $("td").klass("frame4").att("colspan", span)
      .style("font-family:monospace;").html(_("Special data"));

    let i = 1;
    return $("table").att("align", "center")
      .style("border-collapse:collapse;border:0px;")
      .add($("tr")
        .add(thl().html(_("Ix")))
        .add(thl().html(_("Id")))
        .add(th().html(_("Type")))
        .add(thl().html(_("Cycle")))
        .add(thl().html(_("Bet")))
        .add(th().html(_("Ibex")))
        .add(thSpecialData)
        .add(thl().html(_("Incomes")))
        .add(thl().html(_("Buys")))
        .add(thl().html(_("Sells"))))
      .addIt(It.from(self._fleas).map(f =>
          $("tr")
            .add(tdl().html(i++))
            .add(tdl().html(intFormat(f.id())))
            .add(td().html(Flea.familyNames(f.family())))
            .add(tdl().html(intFormat(f.cycle())))
            .add(tdl().html(intFormat(5000 + f.bet() * 1000)))
            .add(td().html(
                f.ibex() === 0 ? _("Out")
                : f.ibex() === 1 ? _("In") : _("Mix")
              ))
            .addIt(specialCols(f, span))
            .add(tdl().html(floatFormat(f.stats().cash())))
            .add(tdl().html(intFormat(f.stats().buys())))
            .add(tdl().html(intFormat(f.stats().sells())))

        ))
    ;
  }

}

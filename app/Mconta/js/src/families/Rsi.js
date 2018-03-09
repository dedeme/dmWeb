// Copyright 16-Jan-2017 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

goog.provide("families_Rsi");
goog.require("Family");

families_Rsi = class {
  /**
   * @param {number} len
   * @param {number} upLevel
   * @param {number} downLevel
   */
  constructor (len, upLevel, downLevel) {
    /** @private */
    this._fieldsNumber = 3;
    /** @private */
    this._len = len;
    /** @private */
    this._upLevel = upLevel;
    /** @private */
    this._downLevel = downLevel;
  }

  /** @return {number} */
  id () {
    return Flea.rsi();
  }

  /** @return {number} */
  len () {
    return this._len;
  }

  /** @return {number} Percentage */
  upLevel () {
    return this._upLevel;
  }

  /** @return {number} Percentage */
  downLevel () {
    return this._downLevel;
  }

  mkFamily () {
    const self = this;

    function bests(intFormat, floatFormat, span) {
      function tdl() {
        return $("td").klass("frame").style("text-align:right");
      }
      return It.from([
        tdl().att("title", _("Length"))
          .html(intFormat(self.len() + 5)),
        tdl().att("title", _("Up Level"))
          .html(floatFormat(50 + (self.upLevel() + 1) * 0.01) + "%"),
        tdl().att("title", _("Down level"))
          .html(floatFormat(50 - (self.downLevel() + 1) * 0.01)  + "%")
      ]).addIt(It.range(span - self._fieldsNumber).map(i => tdl()));
    }

    function trace(intFormat, floatFormat, head, body) {
      return $("span").html(_("Rsi trace data"))
    }

    function traceError(quotes, t, r) {
      return r;
    }

    function traceBody(quotes, t) {
      return It.empty();
    }

    return new Family(
      self._fieldsNumber,
      bests,
      trace,
      traceError,
      traceBody
    );
  }
}

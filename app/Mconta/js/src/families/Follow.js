// Copyright 21-Jan-2017 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

goog.provide("families_Follow");
goog.require("Family");

families_Follow = class {
  /**
   * @param {number} interval
   * @param {number} len
   * @param {number} level
   */
  constructor (interval, len, level) {
    /** @private */
    this._fieldsNumber = 3;
    /** @private */
    this._interval = interval;
    /** @private */
    this._len = len;
    /** @private */
    this._level = level;
  }

  /** @return {number} */
  id () {
    return Flea.follow();
  }

  /** @return {number} */
  interval () {
    return this._interval;
  }

  /** @return {number} */
  len () {
    return this._len;
  }

  /** @return {number} percentage */
  level () {
    return this._level;
  }

  mkFamily () {
    const self = this;

    function thl() {
      return $("td").klass("frame4")
        .style("text-align:right;font-family:monospace;");
    }
    function tdl() {
      return $("td").klass("frame").style("text-align:right");
    }
    function td() {
      return  $("td").klass("frame");
    }
    function tdIf(value) {
      return $("td")
        .klass(value ? "frame" : "frame5")
        .style("text-align: left");
    }

    function bests(intFormat, floatFormat, span) {
      function tdl() {
        return $("td").klass("frame").style("text-align:right");
      }
      return It.from([
        tdl().att("title", _("Interval"))
          .html(intFormat(self.interval() + 5)),
        tdl().att("title", _("Length"))
          .html(intFormat(self.len() + 5)),
        tdl().att("title", _("Level"))
          .html(intFormat(self.level()) + "%")
      ]).addIt(It.range(span - self._fieldsNumber).map(i => tdl()));
    }

    function trace(intFormat, floatFormat, head, body) {
      return $("table")
        .add($("tr")
          .addIt(head)
          .addIt(It.from([
              thl().html(_("Interval")),
              thl().html(_("Length")),
              thl().html(_("Level"))
            ]))
        )
        .add($("tr")
          .addIt(body)
          .addIt(It.from([
              tdl().html(intFormat(self.interval() + 5)),
              tdl().html(intFormat(self.len() + 5)),
              tdl().html(intFormat(self.level()) + "%")
            ]))
        );
    }

    function traceError(quotes, t, r) {
      const textra = t.extra();
      const len = self.len() + 5;
      const quotesExp = Quote.getN(
        quotes, t.nick(), t.quote().date(), len
      );
      const closesExp = It.from(quotesExp).map(q => q.close()).to();
      const old = closesExp[0];
      const lastClose = closesExp[len - 1];
      const inc = new Dec((lastClose - old) / old, 4).value();

      return r ||
        !It.from(textra[0]).eq(It.from(closesExp)) ||
        Math.abs(textra[1] - inc) >= 0.0002
      ;
    }

    function traceBody(quotes, t) {
      const textra = t.extra();
      const len = self.len() + 5;
      const quotesExp = Quote.getN(
        quotes, t.nick(), t.quote().date(), len
      );
      const closesExp = It.from(quotesExp).map(q => q.close()).to();
      const old = closesExp[0];
      const lastClose = closesExp[len - 1];
      const inc = new Dec((lastClose - old) / old, 4).value();

      return It.from([
        $("tr")
          .add(tdIf(It.from(textra[0]).map(c => "" + c).eq(
            It.from(quotesExp).map(q => "" + q.close())
          )).html(_("Closes")))
          .add(td().html(It.join(
              It.from(textra[0]).map(c => "" + c), "; "
            )))
          .add(td().html(It.join(
              It.from(closesExp).map(c => "" + c), "; "
            ))),
        $("tr")
          .add(tdIf(Math.abs(textra[1] - inc) < 0.0002).html(_("Increment")))
          .add(td().html("" + textra[1]))
          .add(td().html("" + inc))
      ]);
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

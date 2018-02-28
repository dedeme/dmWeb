// Copyright 22-Jan-2017 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

goog.provide("families_FollowWma");
goog.require("Family");

families_FollowWma = class {
  /**
   * @param {number} interval
   * @param {number} lenShort
   * @param {number} lenLong
   * @param {number} level
   */
  constructor (interval, lenShort, lenLong, level) {
    /** @private */
    this._fieldsNumber = 4;
    /** @private */
    this._interval = interval;
    /** @private */
    this._lenShort = lenShort;
    /** @private */
    this._lenLong = lenLong;
    /** @private */
    this._level = level;
  }

  /** @return {number} */
  id () {
    return Flea.followWma();
  }

  /** @return {number} */
  interval () {
    return this._interval;
  }

  /** @return {number} */
  lenShort () {
    return this._lenShort;
  }

  /** @return {number} */
  lenLong () {
    return this._lenLong;
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
        tdl().att("title", _("Short Length"))
          .html(intFormat(self.lenShort() + 5)),
        tdl().att("title", _("Long Length"))
          .html(intFormat(self.lenLong() + 5)),
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
              thl().html(_("Short Length")),
              thl().html(_("Long Length")),
              thl().html(_("Level"))
            ]))
        )
        .add($("tr")
          .addIt(body)
          .addIt(It.from([
              tdl().html(intFormat(self.interval() + 5)),
              tdl().html(intFormat(self.lenShort() + 5)),
              tdl().html(intFormat(self.lenLong() + 5)),
              tdl().html(intFormat(self.level()) + "%")
            ]))
        );
    }

    function traceError(quotes, t, r) {
      const textra = t.extra();

      const avgS = textra[0] / (self.lenShort() + 5);
      const avgL = textra[1] / (self.lenLong() + 5);
      const inc = new Dec((avgS - avgL) / avgL, 4).value();

      return r || Math.abs(textra[2] - inc) >= 0.0002;
    }

    function traceBody(quotes, t) {
      const textra = t.extra();

      const avgS = textra[0] / (self.lenShort() + 5);
      const avgL = textra[1] / (self.lenLong() + 5);
      const inc = new Dec((avgS - avgL) / avgL, 4).value();

      return It.from([
        $("tr")
          .add(tdIf(Math.abs(textra[2] - inc) < 0.0002).html(_("Increment")))
          .add(td().html("" + textra[2]))
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

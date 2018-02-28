// Copyright 21-Jan-2017 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

goog.provide("families_FollowMa");
goog.require("Family");

families_FollowMa = class {
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
    return Flea.followMa();
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

      const lenS = self.lenShort() + 5;
      const quotesExpS = Quote.getN(
        quotes, t.nick(), t.quote().date(), lenS
      );
      const closesExpS = It.from(quotesExpS).map(q => q.close()).to();
      const sumS = It.from(quotesExpS).reduce(0, (s, e) => s + e.close());
      const avgS = new Dec(sumS / lenS, 4).value();

      const lenL = self.lenLong() + 5;
      const quotesExpL = Quote.getN(
        quotes, t.nick(), t.quote().date(), lenL
      );
      const closesExpL = It.from(quotesExpL).map(q => q.close()).to();
      const sumL = It.from(quotesExpL).reduce(0, (s, e) => s + e.close());
      const avgL = new Dec(sumL / lenL, 4).value();

      const inc = new Dec((avgS - avgL) / avgL, 4).value();

      return r ||
        !It.from(textra[0]).eq(It.from(closesExpS)) ||
        !It.from(textra[1]).eq(It.from(closesExpL)) ||
        Math.abs(textra[2] - inc) >= 0.0002
      ;
    }

    function traceBody(quotes, t) {
      const textra = t.extra();

      const lenS = self.lenShort() + 5;
      const quotesExpS = Quote.getN(
        quotes, t.nick(), t.quote().date(), lenS
      );
      const closesExpS = It.from(quotesExpS).map(q => q.close()).to();
      const sumS = It.from(quotesExpS).reduce(0, (s, e) => s + e.close());
      const avgS = new Dec(sumS / lenS, 4).value();

      const lenL = self.lenLong() + 5;
      const quotesExpL = Quote.getN(
        quotes, t.nick(), t.quote().date(), lenL
      );
      const closesExpL = It.from(quotesExpL).map(q => q.close()).to();
      const sumL = It.from(quotesExpL).reduce(0, (s, e) => s + e.close());
      const avgL = new Dec(sumL / lenL, 4).value();

      const inc = new Dec((avgS - avgL) / avgL, 4).value();

      return It.from([
        $("tr")
          .add(tdIf(It.from(textra[0]).map(c => "" + c).eq(
            It.from(quotesExpS).map(q => "" + q.close())
          )).html(_("Closes Short")))
          .add(td().html(It.join(
              It.from(textra[0]).map(c => "" + c), "; "
            )))
          .add(td().html(It.join(
              It.from(closesExpS).map(c => "" + c), "; "
            ))),
        $("tr")
          .add(tdIf(It.from(textra[1]).map(c => "" + c).eq(
            It.from(quotesExpL).map(q => "" + q.close())
          )).html(_("Closes Long")))
          .add(td().html(It.join(
              It.from(textra[1]).map(c => "" + c), "; "
            )))
          .add(td().html(It.join(
              It.from(closesExpL).map(c => "" + c), "; "
            ))),
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

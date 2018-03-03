// Copyright 22-Dic-2017 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

goog.provide("families_UpDown");
goog.require("Family");

families_UpDown = class {
  /**
   * @param {number} len
   * @param {number} buyStrip
   * @param {number} sellStrip
   */
  constructor (len, buyStrip, sellStrip) {
    /** @private */
    this._fieldsNumber = 3;
    /** @private */
    this._len = len;
    /** @private */
    this._buyStrip = buyStrip;
    /** @private */
    this._sellStrip = sellStrip;
  }

  /** @return {number} */
  id () {
    return Flea.upDown();
  }

  /** @return {number} */
  len () {
    return this._len;
  }

  /** @return {number} */
  buyStrip () {
    return this._buyStrip;
  }

  /** @return {number} */
  sellStrip () {
    return this._sellStrip;
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
      return It.from([
          tdl().att("title", _("Length"))
            .html(intFormat(self.len() + 5)),
          tdl().att("title", _("Buy Strip"))
            .html(floatFormat(self.buyStrip() * 0.1) + "%"),
          tdl().att("title", _("Sell Strip"))
            .html(floatFormat(self.sellStrip() * 0.1) + "%")
        ]).addIt(It.range(span - self._fieldsNumber).map(i => tdl()));
    }

    function trace(intFormat, floatFormat, head, body) {
      return $("table")
        .add($("tr")
          .addIt(head)
          .addIt(It.from([
              thl().html(_("Length")),
              thl().html(_("Buy Strip")),
              thl().html(_("Sell Strip"))
            ]))
        )
        .add($("tr")
          .addIt(body)
          .addIt(It.from([
              tdl().html(intFormat(self.len() + 5)),
              tdl().html(floatFormat(self.buyStrip() * 0.1) + "%"),
              tdl().html(floatFormat(self.sellStrip() * 0.1) + "%")
            ]))
        );
    }

    function traceError(quotes, t, r) {
      const textra = t.extra();
      const len =  self.len() + 5;
      let quotesExp = Quote.getN(quotes, t.nick(), t.quote(). date(), len);
      quotesExp = It.from(quotesExp).take(quotesExp.length - 1).to();
      const closesExp = It.from(quotesExp)
        .map(q => "" + q.close()).to();
      const old = closesExp[0];
      const lastClose = +closesExp[len - 2];

      return r ||
        !It.from(textra[0]).map(c => "" + c).eq(It.from(closesExp)) ||
        lastClose < old !== textra[1] ||
        lastClose > old !== textra[2]
      ;
    }

    /**
     * @param {!Object<string, !Array<!Quote>>} quotes
     * @param {!Trace} t
     * @return {!It<!Domo>}
     */
    function traceBody(quotes, t) {
      const textra = t.extra();
      const len = self.len() + 5;
      let quotesExp = Quote.getN(quotes, t.nick(), t.quote().date(), len);
      quotesExp = It.from(quotesExp).take(quotesExp.length - 1).to();
      const closesExp = It.from(quotesExp).map(q => q.close()).to();
      const old = closesExp[0];
      const lastClose = closesExp[len - 2];

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
          .add(tdIf(lastClose < old === textra[1]).html(_("Can buy")))
          .add(td().html(textra[1]))
          .add(td().html(
              "" + lastClose + " < " + old + " => " + (lastClose < old)
            )),
        $("tr")
          .add(tdIf(lastClose > old === textra[2]).html(_("Can sell")))
          .add(td().html(textra[2]))
          .add(td().html(
              "" + lastClose + " > " + old + " => " + (lastClose > old)
            ))
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

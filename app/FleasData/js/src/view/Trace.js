// Copyright 8-Dec-2017 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

goog.provide("view_Trace");

goog.require("Quote");

view_Trace = class {
  /**
   * @param {!Main} control
   * @param {!Flea} flea
   * @param {!Array<?>} traces
   * @param {!Object<string, !Array<!Quote>>} quotes
   */
  constructor (control, flea, traces, quotes) {
    /** @private */
    this._control = control;

    /**
     * @private
     * @type {!Flea}
     */
    this._flea = flea;

    /**
     * @private
     * @type {!Array<?>}
     */
    this._traces = traces;

    /**
     * @private
     * @type {!Object<string, !Array<!Quote>>} quotes
     */
    this._quotes = quotes;
  }

  /**
   * @return {void}
   */
  show () {
    const self = this;
    const control = self._control;
    const conf = control.conf();
    const traceDiv = $("div");

    function th() {
      return $("td").klass("frame4").style("font-family:monospace;");
    }

    function thl() {
      return $("td").klass("frame4")
        .style("text-align:right;font-family:monospace;");
    }

    function td() {
      return  $("td").klass("frame");
    }

    function tdl() {
      return $("td").klass("frame").style("text-align:right");
    }

    function intFormat (n) {
      return conf.fDec(new Dec(n, 0))
    }

    function floatFormat (n) {
      return conf.fDec(new Dec(n, 2))
    }

    function floatFormat4 (n) {
      return conf.fDec(new Dec(n, 4))
    }

    function appFee(money) {
      let broker = 9.75;
      if (money > 25000) {
        broker = money * 0.001;
      }

      let bolsa = 4.65 + money * 0.00012;
      if (money > 140000) {
        bolsa = 13.4;
      } else if (money > 70000) {
        bolsa = 9.2 + money * 0.00003;
      } else if (money > 35000) {
        bolsa = 6.4 + money * 0.00007;
      }

      return broker + bolsa;
    }

    function cashWithFees (isBuy, open, stockCash) {
      if (isBuy) {
        const money = stockCash - appFee(stockCash);
        let stocks = Math.floor(stockCash / open);
        while (true) {
          let cost = stocks * open;
          cost += appFee(cost);
          if (cost < stockCash) {
            return -cost;
          }
          --stocks;
        }
      } else {
        let income = stockCash * open;
        income -= appFee(income);
        return income;
      }
    }

    function buyNumber (open, cash) {
      const money = cash - appFee(cash);
      let stocks = Math.floor(cash / open);
      while (true) {
        let cost = stocks * open;
        cost += appFee(cost);
        if (cost < cash) {
          return stocks;
        }
        --stocks;
      }
    }

    /**
     * @param {!Array<!PortfolioEntry>} portfolio
     * @param {string} nick
     * @param {number} stocks (can be negative)
     * @return {!Array<!PortfolioEntry>}
     */
    function addPortfolio (portfolio, nick, stocks) {
      const r = [];
      let added = false;
      It.from(portfolio).each(e => {
        if (e.nick() === nick) {
          const sts = e.stocks() + stocks;
          if (Math.abs(sts) > 0.01) {
            r.push(new PortfolioEntry(nick, sts))
          }
          added = true;
        } else {
          r.push(new PortfolioEntry(e.nick(), e.stocks()));
        }
      });
      if (!added) {
        r.push(new PortfolioEntry(nick, stocks));
      }

      return r;
    }

    function dataFlea() {
      function head() {
        return [
          thl().html(_("Id")),
          th().html(_("Type")),
          thl().html(_("Cycle")),
          thl().html(_("Bet")),
          thl().html(_("Ibex")),
          thl().html(_("Incomes")),
          thl().html(_("Buys")),
          thl().html(_("Sells"))
        ];
      }
      function body() {
        const f = self._flea;
        return [
          tdl().html(intFormat(f.id())),
          td().html(Flea.familyNames(f.family())),
          tdl().html(f.cycle()),
          tdl().html(intFormat(5000 + f.bet() * 1000)),
          tdl().html(intFormat(f.ibex())),
          tdl().html(floatFormat(f.stats().cash())),
          tdl().html(intFormat(f.stats().buys())),
          tdl().html(intFormat(f.stats().sells()))
        ];
      }
      return self._flea.extra().trace()(
        intFormat, floatFormat, It.from(head()), It.from(body())
      );
    }

    function traces() {
      function span(i, trace) {
        let si = "000" + i
        si = si.substring(si.length - 3);
        return $("span")
          .add($("span").klass(isError(i, trace) ? "frame5" : "frame")
            .add(Ui.link(ev => { showTrace(si, trace); })
              .klass("link")
              .html(si)
            )
          ).add($("span").html(" "));
      }
      const div = $("div").style("line-height:200%;text-align:left;");
      const ts = self._traces;
      It.from(ts).eachIx((t, i) => { div.add(span(i, t)); });
      return div;
    }

    /**
     * @param {string} i
     * @param {!Trace} t
     */
    function isError(i, t) {
      const quotes = self._quotes;
      const flea = self._flea;
      const portfolioExp = addPortfolio(
        t.beforePortfolio(),
        t.nick(),
        t.stocks() === 0
          ? buyNumber(t.quote().open(), t.cash())
          : -t.stocks()
      );

      let r = false;
      r = r ||
        !t.quote().eq(Quote.get1(self._quotes, t.nick(), t.quote().date()));
      r = r ||
        Math.abs(
          (new Dec(t.afterCash(), 2).value() -
            new Dec(t.beforeCash(), 2).value())
          -
          cashWithFees(
            t.stocks() === 0,
            new Dec(t.quote().open(), 4).value(),
            t.stocks() === 0
              ? new Dec(t.cash(), 2).value()
              : new Dec(t.stocks(), 0).value()
          )
        ) > 0.01;
      r = r || portfolioToHtml(t.afterPortfolio()) !==
                portfolioToHtml(portfolioExp)

      return flea.extra().traceError()(quotes, t, r);
    }

    function portfolioToHtml (portfolio) {
      return "<table><tr><td>" +
        It.join(
          It.from(portfolio).map(e =>
            e.nick() + "</td><td>: </td><td>" + intFormat(e.stocks())
          ), "</td></tr><tr><td>"
        ) + "</td></tr></table>";
    }

    /**
     * @param {string} i
     * @param {!Trace} t
     */
    function showTrace(i, t) {
      const quotes = self._quotes;
      const flea = self._flea;
      const nickQuotes = quotes[t.nick()];

      function tdIf(value) {
        return $("td")
          .klass(value ? "frame" : "frame5")
          .style("text-align: left");
      }
      function td() {
        return $("td").klass("frame").style("text-align: left");
      }
      const dayQuote = Quote.get1(self._quotes, t.nick(), t.quote().date());
      const dayQuoteS = dayQuote === null ? "Unknown" : dayQuote.serialize();
      const dayQuoteStr = dayQuote === null ? "Unknown" : dayQuote.toStr(conf);

      const cashDif = new Dec(t.afterCash(), 2).value() -
        new Dec(t.beforeCash(), 2).value();
      const cashDifExp = cashWithFees(
          t.stocks() === 0,
          new Dec(t.quote().open(), 4).value(),
          t.stocks() === 0
            ? new Dec(t.cash(), 2).value()
            : new Dec(t.stocks(), 0).value()
        );

      const portfolioExp = addPortfolio(
        t.beforePortfolio(),
        t.nick(),
        t.stocks() === 0
          ? buyNumber(t.quote().open(), t.cash())
          : -t.stocks()
      );

      traceDiv.removeAll()
        .add($("p").add($("span").klass("frame").html(i)))
        .add($("p").html(
            t.quote().date() +
            (t.stocks() === 0
              ? " | Buy | " + t.nick() + " | " + floatFormat(t.cash())
              : " | Sell | " + t.nick() + " | " + intFormat(t.stocks())
            )
          ))
        .add($("table").klass("main")
          .add($("tr")
            .add($("td"))
            .add($("td").style("width:50%;").html(_("Actual")))
            .add($("td").style("width:50%;").html(_("Expected"))))
          .add($("tr")
            .add(tdIf(dayQuote.eq(t.quote())).html(_("Quote")))
            .add(td().html(t.quote().toStr(conf)))
            .add(td().html(dayQuoteStr)))
          .add($("tr")
            .add(tdIf(Math.abs(cashDif - cashDifExp) <= 0.01).html(_("Cash")))
            .add(td().html(
                _("Before: ") + floatFormat(t.beforeCash()) + "<br>" +
                _("After: ") + floatFormat(t.afterCash()) + "<br>" +
                _("Diference: ") + floatFormat(cashDif)))
            .add(td().html(floatFormat(cashDifExp))))
          .add($("tr")
            .add(td().html(_("Before Portfolio")))
            .add(td().html(portfolioToHtml(t.beforePortfolio())))
            .add(td().html("")))
          .add($("tr")
            .add(tdIf(
                portfolioToHtml(t.afterPortfolio()) ===
                  portfolioToHtml(portfolioExp)
              ).html(_("After Portfolio")))
            .add(td().html(portfolioToHtml(t.afterPortfolio())))
            .add(td().html(portfolioToHtml(portfolioExp))))
          .addIt(flea.extra().traceBody()(quotes, t))
        )
      ;
    }

    control.dom().show("trace", $("div").style("text-align:center")
      .add($("h2").html(_("Trace")))
      .add($("table").att("align", "center").add($("tr").add($("td")
        .klass("frame")
        .add(dataFlea()))))
      .add($("hr"))
      .add(traces())
      .add($("hr"))
      .add(traceDiv)
    );
  }

  /**
   * Shows an empty page.
   * @param {!Main} control
   */
  static emptyPage(control) {
    control.dom().show("trace", $("div").style("text-align:center")
      .add($("h2").html(_("Trace")))
      .add($("table").att("align", "center").add($("tr").add($("td")
        .klass("frame")
        .add($("span").html(_("There is no trace data"))))))
    );
  }
}


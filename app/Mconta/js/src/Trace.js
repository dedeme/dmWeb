// Copyright 10-Dic-2017 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

goog.provide("Trace");
goog.provide("PortfolioEntry");

goog.require("Quote");

Trace = class {
  /**
   * @param {!Quote} quote
   * @param {number} beforeCash
   * @param {!Array<!PortfolioEntry>} beforePortfolio
   * @param {string} nick
   * @param {number} cash
   * @param {number} stocks
   * @param {number} afterCash
   * @param {!Array<!PortfolioEntry>} afterPortfolio
   * @param {?} extra Jsonized extra data
   */
  constructor (
    quote, beforeCash, beforePortfolio,
    nick, cash, stocks, afterCash, afterPortfolio, extra
  ) {
    /** @private */
    this._quote = quote;
    /** @private */
    this._beforeCash = beforeCash;
    /** @private */
    this._beforePortfolio = beforePortfolio;
    /** @private */
    this._nick = nick;
    /** @private */
    this._cash = cash;
    /** @private */
    this._stocks = stocks;
    /** @private */
    this._afterCash = afterCash;
    /** @private */
    this._afterPortfolio = afterPortfolio;
    /** @private */
    this._extra = extra;
  }

  /** @return {!Quote} */
  quote () {
    return this._quote;
  }

  /** @return {number} */
  beforeCash () {
    return this._beforeCash;
  }

  /** @return {!Array<!PortfolioEntry>} */
  beforePortfolio () {
    return this._beforePortfolio;
  }

  /** @return {string} */
  nick () {
    return this._nick;
  }

  /** @return {number} */
  cash () {
    return this._cash;
  }

  /** @return {number} */
  stocks () {
    return this._stocks;
  }

  /** @return {number} */
  afterCash () {
    return this._afterCash;
  }

  /** @return {!Array<!PortfolioEntry>} */
  afterPortfolio () {
    return this._afterPortfolio;
  }

  /** @return {?} */
  extra () {
    return this._extra;
  }

  /** @return {!Array<?>} */
  serialize () {
    const qserial = this._quote.serialize();
    const date = qserial.shift();
    return [
      date,
      qserial,
      this._beforeCash,
      It.from(this._beforePortfolio).map(e => e.serialize()).to(),
      this._nick,
      this._cash,
      this._stocks,
      this._afterCash,
      It.from(this._afterPortfolio).map(e => e.serialize()).to(),
      this._afterPortfolio,
      this._extra
    ];
  }

  /**
   * @param {!Array<?>} serial
   * @return {!Trace}
   */
  static restore (serial) {
    const squote = serial[1];
    squote.unshift(serial[0]);
    const quote = Quote.restore(squote);
    return new Trace (
      quote,
      serial[2],
      It.from(serial[3]).map(s => PortfolioEntry.restore(s)).to(),
      serial[4],
      serial[5],
      serial[6],
      serial[7],
      It.from(serial[8]).map(s => PortfolioEntry.restore(s)).to(),
      serial[9]
    );
  }
}

PortfolioEntry = class {
  /**
   * @param {string} nick
   * @param {number} stocks
   */
  constructor (nick, stocks) {
    /** @private */
    this._nick = nick;
    /** @private */
    this._stocks = stocks;
  }

  /** @return {string} */
  nick () {
    return this._nick;
  }

  /** @return {number} */
  stocks () {
    return this._stocks;
  }

  /** @return {!Array<?>} */
  serialize () {
    return [
      this._nick,
      this._stocks
    ];
  }

  /**
   * @param {!Array<?>} serial
   * @return {!PortfolioEntry}
   */
  static restore (serial) {
    return new PortfolioEntry (
      serial[0],
      serial[1]
    );
  }
}


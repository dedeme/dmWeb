// Copyright 12-Nov-2017 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

goog.provide("Quote");

Quote = class {
  /**
   * @param {string} date
   * @param {number} open
   * @param {number} close
   * @param {number} max
   * @param {number} min
   * @param {number} vol
   */
  constructor (date, open, close, max, min, vol) {
    /** @private */
    this._date = date;
    /** @private */
    this._open = open;
    /** @private */
    this._close = close;
    /** @private */
    this._max = max;
    /** @private */
    this._min = min;
    /** @private */
    this._vol = vol;
  }

  /** @return {string} */
  date () {
    return this._date;
  }

  /** @return {number} */
  open () {
    return this._open;
  }

  /** @return {number} */
  close () {
    return this._close;
  }

  /** @return {number} */
  max () {
    return this._max;
  }

  /** @return {number} */
  min () {
    return this._min;
  }

  /** @return {number} */
  vol () {
    return this._vol;
  }

  /**
   * @param {Quote} q
   * @return {boolean}
   */
  eq (q) {
    return q
      ? this._date === q._date &&
        this._open === q._open &&
        this._close === q._close &&
        this._max === q._max &&
        this._min === q._min &&
        this._vol === q._vol
      : false
    ;
  }

  /**
   * @param {!Conf} conf
   * @return {string}
   */
  toStr (conf) {
    return this._date + " [" +
      conf.fDec(new Dec(this._open, 4)) + "; " +
      conf.fDec(new Dec(this._close, 4)) + "; " +
      conf.fDec(new Dec(this._max, 4)) + "; " +
      conf.fDec(new Dec(this._min, 4)) + " - " +
      conf.fDec(new Dec(this._vol, 0)) +
      "]"
    ;
  }

  /** @return {!Array<?>} */
  serialize () {
    return [
      this._date,
      this._open,
      this._close,
      this._max,
      this._min,
      this._vol
    ];
  }

  /**
   * @param {!Array<?>} serial
   * @return {!Quote}
   */
  static restore (serial) {
    return new Quote (
      serial[0],
      serial[1],
      serial[2],
      serial[3],
      serial[4],
      serial[5]
    );
  }

  /**
   * Returns quotes from old to new one.
   * @param {string} quotes
   * @return {!Array<!Quote>}
   */
  static fromString (quotes) {
    const lines = quotes.split("\n");
    lines.reverse();
    return It.from(lines).map(l => {
      const ps = l.split(":");
      return Quote.restore([ps[0], +ps[1], +ps[2], +ps[3], +ps[4], +ps[5]]);
    }).to();
  }

  /**
   * @param {!Object<string, !Array<!Quote>>} quotes
   * @param {string} nick
   * @param {string} date
   * @return {Quote}
   */
  static get1 (quotes, nick, date) {
    const nickQuotes = quotes[nick];
    if (nickQuotes) {
      const r = It.from(nickQuotes).findFirst(q => q.date() === date);
      return r ? r : null;
    }
    return null;
  }

  /**
   * @param {!Object<string, !Array<!Quote>>} quotes
   * @param {string} nick
   * @param {string} date
   * @param {number} n
   * @return {!Array<!Quote>}
   */
  static getN (quotes, nick, date, n) {
    const nickQuotes = quotes[nick];
    const r = [];
    if (nickQuotes) {
      const ix = It.from(nickQuotes).indexf(q => q.date() === date);
      if (ix != -1) {
        let start = ix - n;
        if (start < 0) {
          start = 0;
        }
        It.range(start, ix).each(i => {
          r.push(nickQuotes[i]);
        });
      }
    }
    return r;
  }
}

// Copyright 12-Nov-2017 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

goog.provide("Db");

goog.require("Quote");

Db = class {
  /** @param {!Object<string, ?>} d */
  constructor (d) {
    this._d = d;

    /**
     * @private
     * @type {string}
     */
    this._language = d["language"] || "es";

    /**
     * @private
     * @type {string}
     */
    this._source = d["source"] || Main.invertia();


    /**
     * @private
     * @type {string}
     */
    this._page = d["page"] || "update";

    /**
     * @private
     * @type {boolean}
     */
    this._quoteTranslator = d["quoteTranslator"] ||false;

    /**
     * @private
     * @type {!Object<string, string>}
     */
    this._invertiaId = d["invertiaId"] || {};

    /**
     * @private
     * @type {!Object<string, string>}
     */
    this._infomercadosId = d["infomercadosId"] || {};;


    /**
     * @private
     * @type {string}
     */
    this._model = d["model"] || "";

    /**
     * @private
     * @type {!Object<string, boolean>}
     */
    this._ibex = d["ibex"] || {};

    /**
     * @private
     * @type {!Object<string, string>}
     */
    this._status = {};

    /**
     * @private
     * @type {!Object<string, number>}
     */
    this._fixeds = {};

    /**
     * @private
     * @type {!Object<string, !Array<Quote>>}
     */
    this._quotes = {};
  }

  /** @return {string} */
  language () {
    return this._language;
  }

  /**
   * @param {string} value
   * @return {void}
   */
  setLanguage (value) {
    this._language = value;
  }

  /** @return {string} */
  source () {
    return this._source;
  }

  /**
   * @param {string} value
   * @return {void}
   */
  setSource (value) {
    this._source = value;
  }

  /** @return {string} */
  page () {
    return this._page;
  }

  /** @return {boolean} */
  quoteTranslator () {
    return this._quoteTranslator;
  }

  /**
   * @param {boolean} value
   * @return {void}
   */
  setQuoteTranslator (value) {
    this._quoteTranslator = value;
  }

  /**
   * @param {string} value
   * @return {void}
   */
  setPage (value) {
    this._page = value;
  }

  /**
   * @return {!Object<string, string>}
   *   key   : nick
   *   value : Invertia identifier
   */
  invertiaId () {
    return this._invertiaId;
  }

  /** @return {!Object<string, string>} */
  infomercadosId () {
    return this._infomercadosId;
  }

  /** @return {string} */
  model () {
    return this._model;
  }

  /**
   * @param {string} value
   * @return {void}
   */
  setModel (value) {
    this._model = value;
  }

  /** @return {!Object<string, boolean>} */
  ibex () {
    return this._ibex;
  }

  /**
   * @return {!Object<string, string>}
   *   key   : nick
   *   value : "" -> quotes of nick are verified and well
   *           "?" -> quotes of nick are not verified yet
   *           "text" -> There are errors in quotes data
   */
  status () {
    return this._status;
  }

  /** @return {!Object<string, number>} */
  fixeds () {
    return this._fixeds;
  }


  /**
   * @return {!Object<string, !Array<Quote>>}
     *   key   : nick
     *   value : An array of 550 Quotes
   */
  quotes () {
    return this._quotes;
  }

  /** @return {!Object<string, ?>} */
  serialize () {
    const qs = this._quotes;
    return {
      "language" : this._language,
      "source" : this._source,
      "page" : this._page,
      "quoteTranslator" : this._quoteTranslator,
      "invertiaId" : this._invertiaId,
      "infomercadosId" : this._infomercadosId,
      "model": this._model,
      "ibex": this._ibex,
      "status" : this._status
    };
  }

  verify () {
    const ibex = this._ibex;
    if (!It.keys(this._invertiaId).hasNext()) {
      alert(_("There are no companies yet"));
      return;
    }
    let model = this._model;
    if (model === "" || !It.keys(this._invertiaId).contains(model)) {
      model = It.keys(this._invertiaId).next();
      this._model = model;
    }
    const dates = [];
    let err = "";
    It.from(this._quotes[model]).each(q => {
      try {
        const dt = DateDm.fromStr(q.date());
        dates.push(q.date());
      } catch (e) {
        err = _args(
          _("Error in %0 date %1:\n%2"), model, q.date(), e.message/**/
        );
      }
    });
    this._status[model] = "";
    if (err !== "") {
      this._status[model] = err;
    }
    It.keys(this._invertiaId).each(k => {
      if (ibex[k] === undefined) {
        ibex[k] = false;
      }
      let fixCount = 0;
      let previousClose = -1;
      let err = "";
      if (k !== model) {
        this._status[k] = "";
      } else {
        err = this._status[k];
      }
      It.zip(
        It.from(dates),
        It.from(this._quotes[k])
      ).each(tp => {
        const d = tp.e1();
        const q = tp.e2();
        if (q.force()) {
          ++fixCount
        }

        if (err === "") {
          if (d !== q.date()) {
            err = _args(_("Date %0 does not match in %1"), d, k)
          }

          if (err === "" && !q.force()) {
            try {
              if (q.open() > q.max()) {
                throw(new Error(_("open > max")));
              }
              if (q.close() > q.max()) {
                throw(new Error(_("close > max")));
              }
              if (q.min() > q.max()) {
                throw(new Error(_("min > max")));
              }
              if (q.open() < q.min()) {
                throw(new Error(_("open < min")));
              }
              if (q.close() < q.min()) {
                throw(new Error(_("close < min")));
              }
              if (q.min() < q.min()) {
                throw(new Error(_("min < min")));
              }
              if (previousClose !== -1) {
                if (Math.abs(q.close() - previousClose) / previousClose > 0.2) {
                  throw(new Error(_("close: Wrong value (Difference > 20%)")));
                }
              }
              if (q.close() === -1) {
                throw(new Error(_("close: Wrong value (-1)")));
              }
              previousClose = q.close();
            } catch (e) {
              err = _args(_("Date %0 of %1:\n%2"), d, k, e.message/**/)
            }
          } else {
            if (q.close() !== -1) {
              previousClose = q.close();
            }
          }

          if (err !== "") {
            this._status[k] = err;
          }
        }

      });
      this._fixeds[k] = fixCount;
    });
  }

  /**
   * @param {!Object<string, ?>} serial
   * @return {!Db}
   */
  static restore (serial) {
    return new Db({
      "language" : serial["language"],
      "source" : serial["source"],
      "page" : serial["page"],
      "quoteTranslator" : serial["quoteTranslator"],
      "invertiaId" : serial["invertiaId"],
      "infomercadosId" : serial["infomercadosId"],
      "model" : serial["model"],
      "ibex" : serial["ibex"]
    });
  }
 }


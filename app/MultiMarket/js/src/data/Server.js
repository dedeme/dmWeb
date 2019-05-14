// Copyright 12-Apr-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/** Server and its auxiliar classes */

/** Configuration */
export class Conf {
  /* .
  _rc_ Conf : serial
  url : string
  templateDate : string
  isIsoNumbers : boolean
  fields : string
  tableStart : string
  tableEnd : string
  rowStart : string
  rowEnd : string
  cellsStart : !Array<string>
  cellsEnd : !Array<string>
  */

  /*--*/
  /**
   * @param {string} url
   * @param {string} templateDate
   * @param {boolean} isIsoNumbers
   * @param {string} fields
   * @param {string} tableStart
   * @param {string} tableEnd
   * @param {string} rowStart
   * @param {string} rowEnd
   * @param {!Array<string>} cellsStart
   * @param {!Array<string>} cellsEnd
   */
  constructor (
    url,
    templateDate,
    isIsoNumbers,
    fields,
    tableStart,
    tableEnd,
    rowStart,
    rowEnd,
    cellsStart,
    cellsEnd
  ) {

    /**
     * @private
     * @type {string}
     */
    this._url = url;

    /**
     * @private
     * @type {string}
     */
    this._templateDate = templateDate;

    /**
     * @private
     * @type {boolean}
     */
    this._isIsoNumbers = isIsoNumbers;

    /**
     * @private
     * @type {string}
     */
    this._fields = fields;

    /**
     * @private
     * @type {string}
     */
    this._tableStart = tableStart;

    /**
     * @private
     * @type {string}
     */
    this._tableEnd = tableEnd;

    /**
     * @private
     * @type {string}
     */
    this._rowStart = rowStart;

    /**
     * @private
     * @type {string}
     */
    this._rowEnd = rowEnd;

    /**
     * @private
     * @type {!Array<string>}
     */
    this._cellsStart = cellsStart;

    /**
     * @private
     * @type {!Array<string>}
     */
    this._cellsEnd = cellsEnd;

  }

  /**  @return {string} */
  get url () {
    return this._url;
  }

  /**  @return {string} */
  get templateDate () {
    return this._templateDate;
  }

  /**  @return {boolean} */
  get isIsoNumbers () {
    return this._isIsoNumbers;
  }

  /**  @return {string} */
  get fields () {
    return this._fields;
  }

  /**  @return {string} */
  get tableStart () {
    return this._tableStart;
  }

  /**  @return {string} */
  get tableEnd () {
    return this._tableEnd;
  }

  /**  @return {string} */
  get rowStart () {
    return this._rowStart;
  }

  /**  @return {string} */
  get rowEnd () {
    return this._rowEnd;
  }

  /**  @return {!Array<string>} */
  get cellsStart () {
    return this._cellsStart;
  }

  /**  @return {!Array<string>} */
  get cellsEnd () {
    return this._cellsEnd;
  }

  /** @return {!Array<?>} */
  toJs () {
    return [
      this._url,
      this._templateDate,
      this._isIsoNumbers,
      this._fields,
      this._tableStart,
      this._tableEnd,
      this._rowStart,
      this._rowEnd,
      this._cellsStart,
      this._cellsEnd
    ];
  }

  /**
   * @param {!Array<?>} serial
   * @return {!Conf}
   */
  static fromJs (serial) {
    return new Conf(
      serial[0],
      serial[1],
      serial[2],
      serial[3],
      serial[4],
      serial[5],
      serial[6],
      serial[7],
      serial[8],
      serial[9]
    );
  }
  /*--*/
}

/** Pair nickId-server code */
export class Code {
  /* .
  _rc_ Code : serial
  nickId : number
  code : string
  */

  /*--*/
  /**
   * @param {number} nickId
   * @param {string} code
   */
  constructor (nickId, code) {

    /**
     * @private
     * @type {number}
     */
    this._nickId = nickId;

    /**
     * @private
     * @type {string}
     */
    this._code = code;

  }

  /**  @return {number} */
  get nickId () {
    return this._nickId;
  }

  /**  @return {string} */
  get code () {
    return this._code;
  }

  /** @return {!Array<?>} */
  toJs () {
    return [
      this._nickId,
      this._code
    ];
  }

  /**
   * @param {!Array<?>} serial
   * @return {!Code}
   */
  static fromJs (serial) {
    return new Code(
      serial[0],
      serial[1]
    );
  }
  /*--*/
}

/** Server */
export default class Server {
  /* .
  _rc_ Server : serial
  id : number
  shortName : string
  longName : string
  companyConf : Conf
  dailyConf : Conf
  historicConf : Conf
  codes : Array<Code>
  */

  /*--*/
  /**
   * @param {number} id
   * @param {string} shortName
   * @param {string} longName
   * @param {Conf} companyConf
   * @param {Conf} dailyConf
   * @param {Conf} historicConf
   * @param {Array<Code>} codes
   */
  constructor (
    id,
    shortName,
    longName,
    companyConf,
    dailyConf,
    historicConf,
    codes
  ) {

    /**
     * @private
     * @type {number}
     */
    this._id = id;

    /**
     * @private
     * @type {string}
     */
    this._shortName = shortName;

    /**
     * @private
     * @type {string}
     */
    this._longName = longName;

    /**
     * @private
     * @type {Conf}
     */
    this._companyConf = companyConf;

    /**
     * @private
     * @type {Conf}
     */
    this._dailyConf = dailyConf;

    /**
     * @private
     * @type {Conf}
     */
    this._historicConf = historicConf;

    /**
     * @private
     * @type {Array<Code>}
     */
    this._codes = codes;

  }

  /**  @return {number} */
  get id () {
    return this._id;
  }

  /**  @return {string} */
  get shortName () {
    return this._shortName;
  }

  /**  @return {string} */
  get longName () {
    return this._longName;
  }

  /**  @return {Conf} */
  get companyConf () {
    return this._companyConf;
  }

  /**  @return {Conf} */
  get dailyConf () {
    return this._dailyConf;
  }

  /**  @return {Conf} */
  get historicConf () {
    return this._historicConf;
  }

  /**  @return {Array<Code>} */
  get codes () {
    return this._codes;
  }

  /** @return {!Array<?>} */
  toJs () {
    return [
      this._id,
      this._shortName,
      this._longName,
      this._companyConf ? this._companyConf.toJs() : null,
      this._dailyConf ? this._dailyConf.toJs() : null,
      this._historicConf ? this._historicConf.toJs() : null,
      this._codes ? this._codes.map(e => e ? e.toJs() : null) : null
    ];
  }

  /**
   * @param {!Array<?>} serial
   * @return {!Server}
   */
  static fromJs (serial) {
    return new Server(
      serial[0],
      serial[1],
      serial[2],
      serial[3] ? Conf.fromJs(serial[3]) : null,
      serial[4] ? Conf.fromJs(serial[4]) : null,
      serial[5] ? Conf.fromJs(serial[5]) : null,
      serial[6] ? serial[6].map(e =>
        e ? Code.fromJs(e) : null
      ) : null
    );
  }
  /*--*/
}

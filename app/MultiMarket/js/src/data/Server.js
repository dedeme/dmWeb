// Copyright 12-Apr-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/** Server and its auxiliar classes */

/** Configuration */
export class Rconf {
  /* .
  _rc_ Rconf : serial
  cmd : string
  url : string
  regex: string
  sel : number
  isDateEu : boolean
  dateSeparator : string
  isIsoNumber : boolean
  fieldsType : string
  tableStart : string
  tableEnd : string
  rowStart : string
  rowEnd : string
  cellsStart : !Array<string>
  cellsEnd : !Array<string>
  */

  /*--*/
  /**
      @param {string} cmd
      @param {string} url
      @param {string} regex
      @param {number} sel
      @param {boolean} isDateEu
      @param {string} dateSeparator
      @param {boolean} isIsoNumber
      @param {string} fieldsType
      @param {string} tableStart
      @param {string} tableEnd
      @param {string} rowStart
      @param {string} rowEnd
      @param {!Array<string>} cellsStart
      @param {!Array<string>} cellsEnd
  **/
  constructor (
    cmd,
    url,
    regex,
    sel,
    isDateEu,
    dateSeparator,
    isIsoNumber,
    fieldsType,
    tableStart,
    tableEnd,
    rowStart,
    rowEnd,
    cellsStart,
    cellsEnd
  ) {

    /**
        @private
        @type {string}
    **/
    this._cmd = cmd;

    /**
        @private
        @type {string}
    **/
    this._url = url;

    /**
        @private
        @type {string}
    **/
    this._regex = regex;

    /**
        @private
        @type {number}
    **/
    this._sel = sel;

    /**
        @private
        @type {boolean}
    **/
    this._isDateEu = isDateEu;

    /**
        @private
        @type {string}
    **/
    this._dateSeparator = dateSeparator;

    /**
        @private
        @type {boolean}
    **/
    this._isIsoNumber = isIsoNumber;

    /**
        @private
        @type {string}
    **/
    this._fieldsType = fieldsType;

    /**
        @private
        @type {string}
    **/
    this._tableStart = tableStart;

    /**
        @private
        @type {string}
    **/
    this._tableEnd = tableEnd;

    /**
        @private
        @type {string}
    **/
    this._rowStart = rowStart;

    /**
        @private
        @type {string}
    **/
    this._rowEnd = rowEnd;

    /**
        @private
        @type {!Array<string>}
    **/
    this._cellsStart = cellsStart;

    /**
        @private
        @type {!Array<string>}
    **/
    this._cellsEnd = cellsEnd;

  }

  /**
      @return {string}
  **/
  get cmd () {
    return this._cmd;
  }

  /**
      @return {string}
  **/
  get url () {
    return this._url;
  }

  /**
      @return {string}
  **/
  get regex () {
    return this._regex;
  }

  /**
      @return {number}
  **/
  get sel () {
    return this._sel;
  }

  /**
      @return {boolean}
  **/
  get isDateEu () {
    return this._isDateEu;
  }

  /**
      @return {string}
  **/
  get dateSeparator () {
    return this._dateSeparator;
  }

  /**
      @return {boolean}
  **/
  get isIsoNumber () {
    return this._isIsoNumber;
  }

  /**
      @return {string}
  **/
  get fieldsType () {
    return this._fieldsType;
  }

  /**
      @return {string}
  **/
  get tableStart () {
    return this._tableStart;
  }

  /**
      @return {string}
  **/
  get tableEnd () {
    return this._tableEnd;
  }

  /**
      @return {string}
  **/
  get rowStart () {
    return this._rowStart;
  }

  /**
      @return {string}
  **/
  get rowEnd () {
    return this._rowEnd;
  }

  /**
      @return {!Array<string>}
  **/
  get cellsStart () {
    return this._cellsStart;
  }

  /**
      @return {!Array<string>}
  **/
  get cellsEnd () {
    return this._cellsEnd;
  }

  /**
      @return {!Array<?>}
  **/
  toJs () {
    return [
      this._cmd,
      this._url,
      this._regex,
      this._sel,
      this._isDateEu,
      this._dateSeparator,
      this._isIsoNumber,
      this._fieldsType,
      this._tableStart,
      this._tableEnd,
      this._rowStart,
      this._rowEnd,
      this._cellsStart,
      this._cellsEnd
    ];
  }

  /**
      @param {!Array<?>} serial
      @return {!Rconf}
  **/
  static fromJs (serial) {
    return new Rconf(
      serial[0],
      serial[1],
      serial[2],
      serial[3],
      serial[4],
      serial[5],
      serial[6],
      serial[7],
      serial[8],
      serial[9],
      serial[10],
      serial[11],
      serial[12],
      serial[13]
    );
  }
  /*--*/
}

/** Pair nickId-server code */
export class ServerCode {
  /* .
  _rc_ ServerCode : serial
  nickId : number
  code : ?string
  */

  /*--*/
  /**
   * @param {number} nickId
   * @param {?string} code
   */
  constructor (nickId, code) {

    /**
     * @private
     * @type {number}
     */
    this._nickId = nickId;

    /**
     * @private
     * @type {?string}
     */
    this._code = code;

  }

  /**  @return {number} */
  get nickId () {
    return this._nickId;
  }

  /**  @return {?string} */
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
   * @return {!ServerCode}
   */
  static fromJs (serial) {
    return new ServerCode(
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
  name : string
  dailyConf : Rconf
  historicConf : Rconf
  codes : !Array<!ServerCode>
  */

  /*--*/
  /**
   * @param {number} id
   * @param {string} shortName
   * @param {string} name
   * @param {Rconf} dailyConf
   * @param {Rconf} historicConf
   * @param {!Array<!ServerCode>} codes
   */
  constructor (
    id,
    shortName,
    name,
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
    this._name = name;

    /**
     * @private
     * @type {Rconf}
     */
    this._dailyConf = dailyConf;

    /**
     * @private
     * @type {Rconf}
     */
    this._historicConf = historicConf;

    /**
     * @private
     * @type {!Array<!ServerCode>}
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
  get name () {
    return this._name;
  }

  /**  @return {Rconf} */
  get dailyConf () {
    return this._dailyConf;
  }

  /**  @return {Rconf} */
  get historicConf () {
    return this._historicConf;
  }

  /**  @return {!Array<!ServerCode>} */
  get codes () {
    return this._codes;
  }

  /** @return {!Array<?>} */
  toJs () {
    return [
      this._id,
      this._shortName,
      this._name,
      this._dailyConf ? this._dailyConf.toJs() : null,
      this._historicConf ? this._historicConf.toJs() : null,
      this._codes.map(e => e.toJs())
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
      serial[3] ? Rconf.fromJs(serial[3]) : null,
      serial[4] ? Rconf.fromJs(serial[4]) : null,
      serial[5].map(e =>
        ServerCode.fromJs(e)
      )
    );
  }
  /*--*/
}

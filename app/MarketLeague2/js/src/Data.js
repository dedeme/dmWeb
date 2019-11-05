// Copyright 18-Jul-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/** Data base */

export class Match {
  /* .
  _rc_ Match
    up: number
    down: number
  */
  /*--*/
  /**
   * @param {number} up
   * @param {number} down
   */
  constructor (up, down) {

    /**
     * @private
     * @type {number}
     */
    this._up = up;

    /**
     * @private
     * @type {number}
     */
    this._down = down;

  }

  /**  @return {number} */
  get up () {
    return this._up;
  }

  /**  @return {number} */
  get down () {
    return this._down;
  }
  /*--*/
}

/** Data from previous days of one group */
export class PreviousGroup {
  /* .
  _rc_ PreviousGroup: serial
    first: !Array<string>
    second: !Array<string>
    third: !Array<string>
  */

  /*--*/
  /**
   * @param {!Array<string>} first
   * @param {!Array<string>} second
   * @param {!Array<string>} third
   */
  constructor (first, second, third) {

    /**
     * @private
     * @type {!Array<string>}
     */
    this._first = first;

    /**
     * @private
     * @type {!Array<string>}
     */
    this._second = second;

    /**
     * @private
     * @type {!Array<string>}
     */
    this._third = third;

  }

  /**  @return {!Array<string>} */
  get first () {
    return this._first;
  }

  /**  @return {!Array<string>} */
  get second () {
    return this._second;
  }

  /**  @return {!Array<string>} */
  get third () {
    return this._third;
  }

  /** @return {!Array<?>} */
  toJs () {
    return [
      this._first,
      this._second,
      this._third
    ];
  }

  /**
   * @param {!Array<?>} serial
   * @return {!PreviousGroup}
   */
  static fromJs (serial) {
    return new PreviousGroup(
      serial[0],
      serial[1],
      serial[2]
    );
  }
  /*--*/

}

/** Data from previous days */
export class Previous {
  /* .
  _rc_ Previous: serial
    dailyG: !PreviousGroup
    shortG: !PreviousGroup
    mediumG: !PreviousGroup
    longG: !PreviousGroup
  */

  /*--*/
  /**
   * @param {!PreviousGroup} dailyG
   * @param {!PreviousGroup} shortG
   * @param {!PreviousGroup} mediumG
   * @param {!PreviousGroup} longG
   */
  constructor (dailyG, shortG, mediumG, longG) {

    /**
     * @private
     * @type {!PreviousGroup}
     */
    this._dailyG = dailyG;

    /**
     * @private
     * @type {!PreviousGroup}
     */
    this._shortG = shortG;

    /**
     * @private
     * @type {!PreviousGroup}
     */
    this._mediumG = mediumG;

    /**
     * @private
     * @type {!PreviousGroup}
     */
    this._longG = longG;

  }

  /**  @return {!PreviousGroup} */
  get dailyG () {
    return this._dailyG;
  }

  /**  @return {!PreviousGroup} */
  get shortG () {
    return this._shortG;
  }

  /**  @return {!PreviousGroup} */
  get mediumG () {
    return this._mediumG;
  }

  /**  @return {!PreviousGroup} */
  get longG () {
    return this._longG;
  }

  /** @return {!Array<?>} */
  toJs () {
    return [
      this._dailyG.toJs(),
      this._shortG.toJs(),
      this._mediumG.toJs(),
      this._longG.toJs()
    ];
  }

  /**
   * @param {!Array<?>} serial
   * @return {!Previous}
   */
  static fromJs (serial) {
    return new Previous(
      PreviousGroup.fromJs(serial[0]),
      PreviousGroup.fromJs(serial[1]),
      PreviousGroup.fromJs(serial[2]),
      PreviousGroup.fromJs(serial[3])
    );
  }
  /*--*/
}

/** Match result */
export class MatchResult {
  /* .
  _rc_ MatchResult: serial
    # Positive -> up wins. Negative down wins.
    dif: number
    # 1 (up wins), 0 (draw), 2 (down wins) or '-1' (Resting)
    result: number
  */

  /*--*/
  /**
   * @param {number} dif Positive -> up wins. Negative down wins.
   * @param {number} result 1 (up wins), 0 (draw), 2 (down wins) or '-1' (Resting)
   */
  constructor (dif, result) {

    /**
     * @private
     * @type {number}
     */
    this._dif = dif;

    /**
     * @private
     * @type {number}
     */
    this._result = result;

  }

  /**  @return {number} */
  get dif () {
    return this._dif;
  }

  /**  @return {number} */
  get result () {
    return this._result;
  }

  /** @return {!Array<?>} */
  toJs () {
    return [
      this._dif,
      this._result
    ];
  }

  /**
   * @param {!Array<?>} serial
   * @return {!MatchResult}
   */
  static fromJs (serial) {
    return new MatchResult(
      serial[0],
      serial[1]
    );
  }
  /*--*/
}

export class RoundResult {
  /* .
  _rc_ RoundResult: serial
    results: !Array<!MatchResult>
  */
  /*--*/
  /**
   * @param {!Array<!MatchResult>} results
   */
  constructor (results) {

    /**
     * @private
     * @type {!Array<!MatchResult>}
     */
    this._results = results;

  }

  /**  @return {!Array<!MatchResult>} */
  get results () {
    return this._results;
  }

  /** @return {!Array<?>} */
  toJs () {
    return [
      this._results.map(e => e.toJs())
    ];
  }

  /**
   * @param {!Array<?>} serial
   * @return {!RoundResult}
   */
  static fromJs (serial) {
    return new RoundResult(
      serial[0].map(e =>
        MatchResult.fromJs(e)
      )
    );
  }
  /*--*/
}

export class League {
  /* .
  _rc_ League: serial
    nicks: !Array<string>
    results: !Array<!RoundResult>
  */
  /*--*/
  /**
   * @param {!Array<string>} nicks
   * @param {!Array<!RoundResult>} results
   */
  constructor (nicks, results) {

    /**
     * @private
     * @type {!Array<string>}
     */
    this._nicks = nicks;

    /**
     * @private
     * @type {!Array<!RoundResult>}
     */
    this._results = results;

  }

  /**  @return {!Array<string>} */
  get nicks () {
    return this._nicks;
  }

  /**  @return {!Array<!RoundResult>} */
  get results () {
    return this._results;
  }

  /** @return {!Array<?>} */
  toJs () {
    return [
      this._nicks,
      this._results.map(e => e.toJs())
    ];
  }

  /**
   * @param {!Array<?>} serial
   * @return {!League}
   */
  static fromJs (serial) {
    return new League(
      serial[0],
      serial[1].map(e =>
        RoundResult.fromJs(e)
      )
    );
  }
  /*--*/
}

export class LeagueGroup {
  /* .
  _rc_ LeagueGroup: serial
    # 'code' is sum of quotes for daily group and last date for the rest
    code: string
    first: !League
    second: !League
    third: !League
  */
  /*--*/
  /**
   * @param {string} code 'code' is sum of quotes for daily group and last date for the rest
   * @param {!League} first
   * @param {!League} second
   * @param {!League} third
   */
  constructor (code, first, second, third) {

    /**
     * @private
     * @type {string}
     */
    this._code = code;

    /**
     * @private
     * @type {!League}
     */
    this._first = first;

    /**
     * @private
     * @type {!League}
     */
    this._second = second;

    /**
     * @private
     * @type {!League}
     */
    this._third = third;

  }

  /**  @return {string} */
  get code () {
    return this._code;
  }

  /**  @return {!League} */
  get first () {
    return this._first;
  }

  /**  @return {!League} */
  get second () {
    return this._second;
  }

  /**  @return {!League} */
  get third () {
    return this._third;
  }

  /** @return {!Array<?>} */
  toJs () {
    return [
      this._code,
      this._first.toJs(),
      this._second.toJs(),
      this._third.toJs()
    ];
  }

  /**
   * @param {!Array<?>} serial
   * @return {!LeagueGroup}
   */
  static fromJs (serial) {
    return new LeagueGroup(
      serial[0],
      League.fromJs(serial[1]),
      League.fromJs(serial[2]),
      League.fromJs(serial[3])
    );
  }
  /*--*/
}

export class Current {
  /* .
  _rc_ Current: serial
    dailyG: !LeagueGroup
    shortG: !LeagueGroup
    mediumG: !LeagueGroup
    longG: !LeagueGroup
  */
  /*--*/
  /**
   * @param {!LeagueGroup} dailyG
   * @param {!LeagueGroup} shortG
   * @param {!LeagueGroup} mediumG
   * @param {!LeagueGroup} longG
   */
  constructor (dailyG, shortG, mediumG, longG) {

    /**
     * @private
     * @type {!LeagueGroup}
     */
    this._dailyG = dailyG;

    /**
     * @private
     * @type {!LeagueGroup}
     */
    this._shortG = shortG;

    /**
     * @private
     * @type {!LeagueGroup}
     */
    this._mediumG = mediumG;

    /**
     * @private
     * @type {!LeagueGroup}
     */
    this._longG = longG;

  }

  /**  @return {!LeagueGroup} */
  get dailyG () {
    return this._dailyG;
  }

  /**  @return {!LeagueGroup} */
  get shortG () {
    return this._shortG;
  }

  /**  @return {!LeagueGroup} */
  get mediumG () {
    return this._mediumG;
  }

  /**  @return {!LeagueGroup} */
  get longG () {
    return this._longG;
  }

  /** @return {!Array<?>} */
  toJs () {
    return [
      this._dailyG.toJs(),
      this._shortG.toJs(),
      this._mediumG.toJs(),
      this._longG.toJs()
    ];
  }

  /**
   * @param {!Array<?>} serial
   * @return {!Current}
   */
  static fromJs (serial) {
    return new Current(
      LeagueGroup.fromJs(serial[0]),
      LeagueGroup.fromJs(serial[1]),
      LeagueGroup.fromJs(serial[2]),
      LeagueGroup.fromJs(serial[3])
    );
  }
  /*--*/
}

/** Main data class */
export class Data {
  /* .
  _rc_ Data: serial
    previous: !Previous
    current: !Current
  */

  /*--*/
  /**
   * @param {!Previous} previous
   * @param {!Current} current
   */
  constructor (previous, current) {

    /**
     * @private
     * @type {!Previous}
     */
    this._previous = previous;

    /**
     * @private
     * @type {!Current}
     */
    this._current = current;

  }

  /**  @return {!Previous} */
  get previous () {
    return this._previous;
  }

  /**  @return {!Current} */
  get current () {
    return this._current;
  }

  /** @return {!Array<?>} */
  toJs () {
    return [
      this._previous.toJs(),
      this._current.toJs()
    ];
  }

  /**
   * @param {!Array<?>} serial
   * @return {!Data}
   */
  static fromJs (serial) {
    return new Data(
      Previous.fromJs(serial[0]),
      Current.fromJs(serial[1])
    );
  }
  /*--*/

  /**
   * @param {string} oldNick
   * @param {string} newNick
   * @return {void }
   */
  replace (oldNick, newNick) {
    /** @param {!Array<string>} a */
    function replaceArr (a) {
      for (let i = 0; i < a.length; ++i) {
        if (a[i] === oldNick) {
          a[i] = newNick;
          break;
        }
      }
    }

    /** @param {!PreviousGroup} gr */
    function replaceGroup (gr) {
      replaceArr(gr.first);
      replaceArr(gr.second);
      replaceArr(gr.third);
    }

    replaceGroup(this._previous.dailyG);
    replaceGroup(this._previous.shortG);
    replaceGroup(this._previous.mediumG);
    replaceGroup(this._previous.longG);

    /** @param {!LeagueGroup} l */
    function replaceLeagueGroup (l) {
      replaceArr(l.first.nicks);
      replaceArr(l.second.nicks);
      replaceArr(l.third.nicks);
    }

    replaceLeagueGroup(this._current.dailyG);
    replaceLeagueGroup(this._current.shortG);
    replaceLeagueGroup(this._current.mediumG);
    replaceLeagueGroup(this._current.longG);
  }
}



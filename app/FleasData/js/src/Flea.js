// Copyright 10-Dic-2017 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

goog.provide("Flea");

goog.require("Stat");
goog.require("Gen");
goog.require("families_Family");
goog.require("families_BuyAndHold");
goog.require("families_UpDown");
goog.require("families_MovingAverage");
goog.require("families_WmovingAverage");
goog.require("families_Rsi");
goog.require("families_Follow");
goog.require("families_FollowMa");
goog.require("families_FollowWma");
goog.require("families_Best");
goog.require("families_Ibest");

Flea = class {
  /**
   * @param {number} id
   * @param {number} cycle
   * @param {!families_Family} family
   * @param {number} bet
   * @param {number} sel
   * @param {!Stat} stats
   * @param {!Family} extra
   */
  constructor (id, cycle, family, bet, sel, stats, extra) {
    /** @private */
    this._id = id;
    /** @private */
    this._cycle = cycle;
    /** @private */
    this._family = family;
    /** @private */
    this._bet = bet;
    /** @private */
    this._sel = sel;
    /** @private */
    this._stats = stats;
    /**
     * @private
     * @type {!Family}
     */
    this._extra = extra;
  }

  /** @return {number} */
  id () {
    return this._id;
  }

  /** @return {number} */
  cycle () {
    return this._cycle;
  }

  /** @return {!families_Family} */
  family () {
    return this._family;
  }

  /** @return {number} */
  bet () {
    return this._bet;
  }

  /** @return {number} */
  ibex () {
    return this._sel;
  }

  /** @return {!Stat} */
  stats () {
    return this._stats;
  }

  /** @return {!Family} */
  extra () {
    return this._extra;
  }

  /**
   * @param {!Array<?>} serial
   * @return {Flea}
   */
  static restore (serial) {
    function gen (g) {
      return Gen.restore(g).actualOption();
    }
    function extra(fm) {
      if (fm.opt() == Flea.familyBest()) {
        return new families_Best(
            gen(serial[6][0]), gen(serial[6][1]), gen(serial[6][2])
          ).mkFamily();
      } else if (fm.opt() == Flea.familyIbexBest()) {
        return new families_Ibest(
            gen(serial[6][0]), gen(serial[6][1]), gen(serial[6][2])
          ).mkFamily();
      } else {
        const family = fm.gen().actualOption();
        switch (family) {
          case Flea.buyAndHold(): return new families_BuyAndHold(
            ).mkFamily();
          case Flea.upDown(): return new families_UpDown(
              gen(serial[6][0]), gen(serial[6][1]), gen(serial[6][2])
            ).mkFamily();
          case Flea.movingAverage(): return new families_MovingAverage(
              gen(serial[6][0]), gen(serial[6][1]), gen(serial[6][2])
            ).mkFamily();
          case Flea.wmovingAverage(): return new families_WmovingAverage(
              gen(serial[6][0]), gen(serial[6][1]), gen(serial[6][2])
            ).mkFamily();
          case Flea.rsi(): return new families_Rsi(
              gen(serial[6][0]), gen(serial[6][1]), gen(serial[6][2])
            ).mkFamily();
          case Flea.follow(): return new families_Follow(
              gen(serial[6][0]), gen(serial[6][1]), gen(serial[6][2])
            ).mkFamily();
          case Flea.followMa(): return new families_FollowMa(
              gen(serial[6][0]), gen(serial[6][1]), gen(serial[6][2]),
              gen(serial[6][3])
            ).mkFamily();
          case Flea.followWma(): return new families_FollowWma(
              gen(serial[6][0]), gen(serial[6][1]), gen(serial[6][2]),
              gen(serial[6][3])
            ).mkFamily();
          default: throw ("'" + family + "': Unkon family");
        }
      }
    }

    if (serial === null) {
      return null;
    }

    const family = families_Family.restore(serial[2]);
    return new Flea (
      serial[0],
      serial[1],

      family,
      gen(serial[3]),
      gen(serial[4]),

      Stat.restore(serial[5]),

      extra(family)
    );
  }

  /** @return {number} */
  static buyAndHold () {
    return 0;
  }

  /** @return {number} */
  static upDown () {
    return 1;
  }

  /** @return {number} */
  static movingAverage () {
    return 2;
  }

  /** @return {number} */
  static wmovingAverage () {
    return 3;
  }

  /** @return {number} */
  static rsi () {
    return 4;
  }

  /** @return {number} */
  static follow () {
    return 5;
  }

  /** @return {number} */
  static followMa () {
    return 6;
  }

  /** @return {number} */
  static followWma () {
    return 7;
  }

  /** @return {number} */
  static familyNumber () {
    return 8;
  }

  /** @return {number} */
  static familiesAll () {
    return 0;
  }

  /** @return {number} */
  static familyBest () {
    return 1;
  }

  /** @return {number} */
  static familyIbexBest () {
    return 2;
  }

  /**
   * @param {number} family
   * @return {string}
   */
  static familyNamesById(family) {
    switch(family) {
      case Flea.buyAndHold(): return "BuyAndHold";
      case Flea.upDown(): return "Up-Down";
      case Flea.movingAverage(): return "MovingAverage";
      case Flea.wmovingAverage(): return "WmovingAverage";
      case Flea.rsi(): return "Rsi";
      case Flea.follow(): return "Follow";
      case Flea.followMa(): return "FollowMa";
      case Flea.followWma(): return "FollowWma";
      default: throw ("'" + family + "': Unkown family");
    }
  }

  /**
   * @param {!families_Family} fm
   * @return {string}
   */
  static familyNames(fm) {
    if (fm.opt() == Flea.familyBest()) {
      return "Best";
    } else if (fm.opt() == Flea.familyIbexBest()) {
      return "Ibest";
    } else {
      return Flea.familyNamesById(fm.gen().actualOption());
    }
  }

}


// Copyright 06-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/**
    Application constants and global functions.
**/
export default class Defs {
  /**
      Application name.
      @return {string}
  **/
  static get app () {
    return "MMarket";
  }

  /**
      Application version.
      @return {string}
  **/
  static get version () {
    return "202003";
  }

  /**
      @return {number}
  **/
  static get bet () {
    return 15000;
  }

  /**
      @return {number}
  **/
  static get initialCapital () {
    return 150000;
  }

  /**
      @private
      @param {string} module
      @param {string} page
  **/
  static go (module, page) {
    location.assign("?" + module + "&" + page);
  }

}

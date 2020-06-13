// Copyright 30-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/**
    Application constants and global functions.
**/
export default class Cts {
  /**
      Application name.
      @return {string}
  **/
  static get app () {
    return "GoDoc";
  }

  /**
      Application version.
      @return {string}
  **/
  static get version () {
    return "2020.04";
  }

  /**
      Maximum number of elements in most used list.
  **/
  static get mostUsedEntries () {
    return 20;
  }

}

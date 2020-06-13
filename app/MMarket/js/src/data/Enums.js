// Copyright 26-Apr-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/**
    Enumerations.
**/

/**
    Error messages
**/
export class MsgType {
  /**
      @return number
  **/
  static get ok () { return 0 }

  /**
      @return number
  **/
  static get warning () { return 1 }

  /**
      @return number
  **/
  static get error () { return 2 }
}

/**
    Server configuration state
**/
export class ServerState {
  /**
      @return number
  **/
  static get stopped () { return 0 }

  /**
      @return number
  **/
  static get active () { return 1 }

  /**
      @return number
  **/
  static get selected () { return 2 }
}

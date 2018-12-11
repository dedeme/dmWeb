// Copyright 11-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/** Nick */
export default class Nick {
  constructor (id, nick, isIbex, isSel) {
    this._id = id;
    this._nick = nick;
    this._isIbex = isIbex;
    this._isSel = isSel;
  }

  /** @return {string} */
  get id () {
    return this._id;
  }

  /** @return {string} */
  get nick () {
    return this._nick;
  }

  /** @return {boolean} */
  get isIbex () {
    return this._isIbex;
  }

  /** @return {boolean} */
  get isSel () {
    return this._isSel;
  }

  static fromJson (serial) {
    return new Nick(
      serial[0],
      serial[1],
      serial[2],
      serial[3]
    );
  }
}

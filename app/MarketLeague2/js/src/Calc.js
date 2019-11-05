// Copyright 25-Jul-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import It from "./dmjs/It.js";
import {Match, League} from "./Data.js"; //eslint-disable-line

export class Mark {
  /**
   * @param {string} nick
   * @param {number} points
   * @param {number} dif
   */
  constructor (nick, points, dif) {
    this._nick = nick;
    this._points = points;
    this._dif = dif;
  }

  get nick () {
    return this._nick;
  }

  get points () {
    return this._points;
  }

  get dif () {
    return this._dif;
  }
}


/** Several functions */
export default class Calc {
  /**
   * @param {number} nplayers
   * @return {!Array<!Array<!Match>>}
   */
  static rounds (nplayers) {
    const odd = nplayers % 2;
    nplayers += odd;

    const r = [];
    const list = [...It.range(nplayers)];
    const nplayers1 = nplayers - 1;
    const mid = nplayers / 2;
    It.range(nplayers1).each(() => {
      const matches = [];
      It.range(mid).each(i => matches.push(
        new Match(list[i], list[nplayers1 - i])
      ));
      r.push(matches);
      const last = list[nplayers1];
      for (let i = nplayers1; i > 1; --i) {
        list[i] = list[i - 1];
      }
      list[1] = last;
    });

    return r;
  }

  /**
   * Returns a division ranking at one round
   * @param {!League} league
   * @param {number} round (first round is 1)
   * @return {!Array<!Mark>} Nicks list sorted from winners to lossers
   */
  static ranking (league, round) {
    const lresults = league.results;
    const rounds = Calc.rounds(league.nicks.length);
    const /** !Array{!Mark} */ marks = [];
    for (let nnk = 0; nnk < league.nicks.length; ++nnk) {
      let points = 0;
      let dif = 0;
      for (let nr = 0; nr < round; ++nr) {
        const matches = rounds[nr];
        const rresults = lresults[nr].results;
        for (let nm = 0; nm < matches.length; ++nm) {
          const match = matches[nm];
          if (match.up === nnk || match.down === nnk) {
            const result = rresults[nm];
            if (result.result === -1) {
              break;
            }
            dif += result.dif;
            if (result.result === 1) {
              if (match.up === nnk) {
                points += 2;
              }
            } else if (result.result === 2) {
              if (match.down === nnk) {
                points += 2;
              }
            } else {
              ++points;
            }
            break;
          }
        }
      }

      marks.push(new Mark(league.nicks[nnk], points, dif));
    }

    return marks.sort((m1, m2) =>
      m1.points === m2.points ? m2.dif - m1.dif : m2.points - m1.points
    );
  }
}

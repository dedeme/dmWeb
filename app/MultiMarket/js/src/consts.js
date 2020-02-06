// Copyright 04-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/** Application constants */

/** Error messages */
export class MSG {
  static get OK () { return 0 }

  static get WARNING () { return 1 }

  static get ERROR () { return 2 }
}

/** Server configuration status */
export class SERVER {
  static get STOPPED () { return 0 }

  static get ACTIVE () { return 1 }

  static get SELECTED () { return 2 }
}

export const HISTORIC_QUOTES = 610;

export const WGET = "Wget";
export const PUPPETEER = "Puppeteer";

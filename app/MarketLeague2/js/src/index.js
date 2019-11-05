// Copyright 18-Jul-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Main from "./Main.js";

/**
 * Application entry.
 * @return {void}
 */
export function main () {
  new Main().show();
}

/** @type {!Object<string, function():void>} */
window["main"] = main;

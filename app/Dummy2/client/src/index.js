// Copyright 06-Oct-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Main from "./Main.js";

/** Application entry. */
export function main () {
  new Main().start();
}

window["main"] = main;

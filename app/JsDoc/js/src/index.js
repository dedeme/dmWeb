// Copyright 04-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/** Application entry. */

import Main from "./Main.js";

export function main () {
  new Main().start();
}

window["main"] = main;

// Copyright 30-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Main from "./Main.js";

/**
    Application entry.
    @return {void}
**/
export function main () {
  Main.show();
}

/**
    @type {!Object<string, function():void>}
**/
window["main"] = main;

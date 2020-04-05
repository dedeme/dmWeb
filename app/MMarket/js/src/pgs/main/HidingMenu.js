// Copyright 08-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Domo from "../../dmjs/Domo.js"; //eslint-disable-line
import Ui from "../../dmjs/Ui.js";
import Maybe from "../../dmjs/Maybe.js";
import {Menu, MenuEntry} from "../../dmjs/Menu.js"; //eslint-disable-line

const $ = e => Ui.$(e);

/**
    Concealable menu.
**/
export default class HidingMenu {
  /**
      Constructor.
  **/
  constructor () {
    /**
        @private
        @type {!Maybe<!Menu>}
    **/
    this._menu = Maybe.nothing;

    /**
        @private
        @type {boolean}
    **/
    this._hidden = true;

    /**
        @private
        @type {!Domo}
    **/
    this._wg = $("div");
  }

  /**
      @return {!Domo}
  **/
  get wg () {
    return this._wg;
  }

  /**
      @return {boolean}
  **/
  get isHidden () {
    return this._hidden;
  }

  /**
      @return {!MenuEntry}
  **/
  get button () {
    return new MenuEntry(
      Maybe.nothing,
      Ui.link(() => this.change())
        .add(Ui.img("menu").style("vertical-align:middle"))
    );
  }

  // View ----------------------------------------------------------------------

  /**
      @private
  **/
  view () {
    this._wg.removeAll();

    if (!this._hidden) this._wg.add(this._menu.fromJust().wg);
  }


  // Control -------------------------------------------------------------------

  /**
      @param {!Menu} m
  **/
  set menu (m) {
    this._menu = Maybe.just(m);
  }

  hide () {
    this._hidden = true;
    this.view();
  }

  show () {
    this._hidden = false;
    this.view();
  }

  change () {
    if (this._hidden) this.show();
    else this.hide();
  }
}

// Copyright 09-Mar-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

goog.provide("portfolio_View");

portfolio_View = class {
  /**
   *  @param {!Main} control
   */
  constructor (control) {
    /** @private */
    this._control = control;
  }

  /**
   * @return {void}
   */
  run () {
    const self = this;
    self.show();
  }

  /**
   * @private
   * @return {void}
   */
  show () {
    const control = this._control;
    const dom = control.dom();

    dom.show("portfolio", "view", $("div").html("here"));
  }

}

// Copyright 01-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Domo from "../dmjs/Domo.js"; //eslint-disable-line
import Ui from "../dmjs/Ui.js";
import ModalBox from "../dmjs/ModalBox.js";
import {_} from "../I18n.js";

const $ = e => Ui.$(e);

const wg = $("div");
const box = new ModalBox(wg, false);

/** MsgModal widget */
export default class Msg {

  /**
      @return !Domo
  **/
  static get wg () {
    return box.wg;
  }

  /**
      @return void
  **/
  static close () {
    box.show(false);
  }

  /**
      @param {!Domo} swg
      @return void
  **/
  static showWg (swg) {
    wg
      .removeAll()
      .add($("table")
        .add($("tr")
          .add($("td")
            .add(swg)))
        .add($("tr")
          .add($("td")
            .add($("hr"))))
        .add($("tr")
          .add($("td")
            .style("text-align: center")
            .add($("button")
              .text(_("Close"))
              .on("click", () => {
                box.show(false);
              })))))
    ;
    box.show(true);
  }

  /**
      @private
      @param {string} icon
      @param {string} msg Html message
      @param {function():void =} fn Function to call after closing the bpx.
      @return {void}
  **/
  static show (icon, msg, fn) {
    wg
      .removeAll()
      .add($("table")
        .add($("tr")
          .add($("td")
            .style("valign:middle;width:50px")
            .att("rowspan", 3)
            .add(Ui.img(icon)))
          .add($("td")
            .style("text-align:left")
            .html(msg)))
        .add($("tr")
          .add($("td")
            .add($("hr"))))
        .add($("tr")
          .add($("td")
            .style("text-align: right")
            .add($("button")
              .text(_("Close"))
              .on("click", () => {
                box.show(false);
                if (fn !== undefined) {
                  fn();
                }
              })))))
    ;
    box.show(true);
  }

  /**
      @param {string} msg Html message
      @param {function():void =}  fn Function to call after closing the bpx.
      @return void
  **/
  static ok (msg, fn) {
    Msg.show("well2", msg, fn);
  }

  /**
      @param {string} msg Html message
      @param {function():void =}  fn Function to call after closing the bpx.
      @return void
  **/
  static info (msg, fn) {
    Msg.show("info", msg, fn);
  }

  /**
      @param {string} msg Html message
      @param {function():void =}  fn Function to call after closing the bpx.
      @return void
  **/
  static error (msg, fn) {
    Msg.show("error2", msg, fn);
  }


}

// Copyright 19-Aug-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package wgs;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.ModalBox;
import I18n._;

class Msg {
  static final iwg = Q("div");
  static final box = new ModalBox(iwg, false);
  public static var wg(get, never): Domo;
  static function get_wg () {
    return box.wg;
  }

  static function show (icon, msg: String, fn: () -> Void) {
    iwg
      .removeAll()
      .add(Q("table")
        .add(Q("tr")
          .add(Q("td")
            .style("valign:middle;width:50px")
            .att("rowspan", 3)
            .add(Ui.img(icon)))
          .add(Q("td")
            .style("text-align:left")
            .html(msg)))
        .add(Q("tr")
          .add(Q("td")
            .add(Q("hr"))))
        .add(Q("tr")
          .add(Q("td")
            .style("text-align: right")
            .add(Q("button")
              .text(_("Close"))
              .on(CLICK, e -> {
                box.show(false);
                fn();
              })))))
    ;
    box.show(true);
  }

  /// Html ok message.
  ///   msg: Message.
  ///   fn : Function to do after closing the message.
  public static function ok (msg: String, fn: () -> Void) {
    show("well2", msg, fn);
  }

  /**
      @param {string} msg Html message
      @param {function():void =}  fn Function to call after closing the bpx.
      @return void
  **/
  /// Html info message.
  ///   msg: Message.
  ///   fn : Function to do after closing the message.
  public static function info (msg: String, fn: () -> Void) {
    show("info", msg, fn);
  }

  /**
      @param {string} msg Html message
      @param {function():void =}  fn Function to call after closing the bpx.
      @return void
  **/
  /// Html error message.
  ///   msg: Message.
  ///   fn : Function to do after closing the message.
  public static function error (msg: String, fn: () -> Void) {
    show("error2", msg, fn);
  }

}

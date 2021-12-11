// Copyright 16-Oct-2021 ºDeme
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

  static function show (icon: String, msg: String, ?fn: () -> Void) {
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
                if (fn != null) {
                  fn();
                }
              })))))
    ;
    box.show(true);
  }

  /// Html ok message.
  ///   msg: Message.
  ///   fn : Function to do after closing the message.
  public static function ok (msg: String, ?fn: () -> Void): Void {
    show("well2", msg, fn);
  }

  /// Html info message.
  ///   msg: Message.
  ///   fn : Function to do after closing the message.
  public static function info (msg: String, ?fn: () -> Void): Void {
    show("info", msg, fn);
  }

  /// Html error message.
  ///   msg: Message.
  ///   fn : Function to do after closing the message.
  public static function error (msg: String, ?fn: () -> Void): Void {
    show("error2", msg, fn);
  }

  /// Show a Widget.
  ///   wg: Widget to show.
  ///   fn : Function to do after closing the message.
  public static function showWg (wg: Domo, ?fn: () -> Void): Void {
    iwg
      .removeAll()
      .add(Q("table")
        .add(Q("tr")
          .add(Q("td")
            .style("width:100%;text-align:right;padding-bottom:5px")
            .add(Q("span")
              .text("[ "))
            .add(Ui.link(e -> box.show(false))
              .style(
                "cursor:pointer;text-decoration: none; font-family: sans;" +
                "color: #000080;font-weight: normal;font-size:14px;"
              ).text("X"))
            .add(Q("span")
              .text(" ]"))))
        .add(Q("tr")
          .add(Q("td")
            .add(wg)))
        .add(Q("tr")
          .add(Q("td")
            .add(Q("hr"))))
        .add(Q("tr")
          .add(Q("td")
            .style("text-align: center")
            .add(Q("button")
              .text(_("Close"))
              .on(CLICK, e -> {
                box.show(false);
                if (fn != null) {
                  fn();
                }
              })))))
    ;
    box.show(true);
  }
}

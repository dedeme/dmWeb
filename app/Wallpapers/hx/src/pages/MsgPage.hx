// Copyright 26-Apr-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pages;

import dm.Domo;
import dm.Ui.Q;
import I18n._;
import I18n._args;

class MsgPage {

  // STATIC

  /// Constructor.
  ///   app       : Application name.
  ///   msg       : Message to show.
  ///   withReload: If a reload message is shown (Default true).
  public static function mk (
    wg: Domo, app: String, msg: String, withReload = true
  ): Void {
    final tx = "<a href=''>" + _("here") + "</a>";
    final reload =
      "<p><b>" +
      _args(_("Click %0 to continue."), [tx]) +
      "</b></p>";
    wg
      .removeAll()
      .klass("margin")
      .add(Q("div")
        .klass("head")
        .style("padding-bottom:20px;")
        .text(app))
      .add(Q("table")
        .klass("main")
        .add(Q("tr")
          .add(Q("td")
            .add(Q("table")
              .klass("border")
              .att("width", "100%")
              .style("background-color: #f8f8f8; border-collapse: collapse")
              .add(Q("tr")
                .add(Q("td")
                  .style("padding:0px 10px 0px 10px;")
                  .html('<p>${msg}<p>${withReload ? reload : ""}')
                ))))));

  }
}

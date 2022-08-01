// Copyright 31-Jul-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import dm.Domo;
import dm.Ui.Q;
import I18n._;
import I18n._args;

class MsgPg {
  static final tx = "<a href=''>" + _("here") + "</a>";

  final wg: Domo;
  final msg: String;
  final withReload: Bool;

  /// Constructor.
  ///   msg       : Message to show.
  ///   withReload: If a reload message is shown (Default true).
  public function new (wg: Domo, msg: String, withReload = true) {
    this.wg = wg;
    this.msg = msg;
    this.withReload = withReload;
  }

  // View ----------------------------------------------------------------------

  /// Shows page.
  public function show () {
    final reload =
      "<p><b>" +
      _args(_("Click %0 to continue."), [tx]) +
      "</b></p>";
    wg
      .removeAll()
      .add(Q("div")
        .klass("head")
        .style("padding-bottom:20px;")
        .text(Cts.appName))
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

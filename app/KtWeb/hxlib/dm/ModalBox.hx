// Copyright 17-Jun-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package dm;

import dm.Domo;
import dm.Ui.Q;
import dm.Ui;
import dm.It;

/// ModalBox to show components.
class ModalBox {
  /// ModalBox widget.
  public var wg(default, null): Domo;

  /// Constructor.
  ///   content  : Widget to show.
  ///   withClose: If it is 'true', ModalBox show a symbol [ X ] to close
  ///              itself (Default 'true')
  public function new (content: Domo, withClose = true) {
    final tb = Q("table")
      .att("align", "center")
      .style(
        "background-color: rgb(250, 250, 250);" +
        "border: 1px solid rgb(110,130,150);" +
        "padding: 4px;border-radius: 4px;"
      );

    if (withClose)
      tb.add(Q("tr")
        .add(Q("td")
          .style("width:100%;text-align:right;padding-bottom:5px")
          .add(Q("span")
            .text("["))
          .add(Ui.link(_ -> show(false))
            .style(
              "cursor:pointer;text-decoration: none; font-family: sans;" +
              "color: #000080;font-weight: normal;font-size:14px;"
            ).text(" X "))
          .add(Q("span")
            .text("]"))));

    tb.add(Q("tr")
      .add(Q("td")
        .add(content)));

    wg = Q("div")
      .style(
        "display: none;" + //Hidden by default
        "position: fixed;" + //Stay in place
        "z-index: 1;" + //Sit on top
        "padding-top: 100px;" + //Location of the box
        "left: 0;" +
        "top: 0;" +
        "width: 100%;" + //Full width
        "height: 100%;" + //Full height
        "overflow: auto;" + //Enable scroll if needed
        "background-color: rgb(0,0,0);" + //Fallback color
        "background-color: rgba(0,0,0,0.4);" + //Black opacity
        "text-align: center;"
      ).add(tb);
  }

  /// Show or hidde the box.
  public function show (value: Bool): Void {
    if (value) wg.setStyle("display", "block");
    else wg.setStyle("display", "none");
  }
}

// Copyright 24-Jan-2023 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Null editor.

package wgs;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Opt;
import data.JsData;
import data.Type;
import data.Tpath;
import data.FieldChange;
import I18n._;
import I18n._args;

class NullEditor {
  final wg: Domo;
  final jsData: JsData;
  final fn: (FieldChange, Option<Tpath>) -> Void;

  public function new (
    wg: Domo, jsData: JsData, fn: (FieldChange, Option<Tpath>) -> Void
  ) {
    this.wg = wg;
    this.jsData = jsData;
    this.fn = fn;
  }

  // View ----------------------------------------------------------------------

  public function show () {
    final trs: Array<Domo> = [];
    for (t in 1...Type.MAP+1) {
      trs.push(
        Q("tr")
          .add(Q("td")
            .style("text-align:center")
            .add(Ui.link(() -> setTo(t))
              .klass("link")
              .text(Type.toString(t))))
      );
    }
    final tb = jsData.tooBig
      ? Q("table")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td")
            .klass("frame")
            .text(_("Table is too big to be modified"))))
      : Q("table")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td")
            .style("text-align:center")
            .text(_("Set to"))))
        .adds(trs)
    ;
    wg
      .removeAll()
      .add(Q("div")
        .klass("head")
        .text("Null"))
      .add(tb)
    ;
  }

  // Control -------------------------------------------------------------------

  function setTo(type: Int): Void {
    if (!Ui.confirm(_args(_("Set field to %0?"), [Type.toString(type)]))) return;
    fn(FieldChange.mkNull(jsData.control, type), None);
  }
}

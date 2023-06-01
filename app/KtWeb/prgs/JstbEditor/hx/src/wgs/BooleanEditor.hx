// Copyright 25-Jan-2023 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Boolean editor.

package wgs;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Opt;
import data.JsData;
import data.Tpath;
import data.FieldChange;
import I18n._;
import I18n._args;

class BooleanEditor {
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
    final toNullDiv = Q("div");
    final type = jsData.tooBig ? -1 : jsData.type;
    new ToNullWg(toNullDiv, jsData.control, type, fn).show();

    final value = jsData.data.rb();
    final tb = Q("table")
      .att("align", "center")
      .add(Q("tr")
        .add(Q("td")
          .style("width:5px")
          .add(value ? Ui.img("ok") : Q("span")))
        .add(Q("td")
          .style("text-align:left")
          .add(value || jsData.tooBig
              ? Q("span").text("true")
              : Ui.link(() -> setTo(true))
                .klass("link")
                .text("true")
            )))
      .add(Q("tr")
        .add(Q("td")
          .style("width:5px")
          .add(!value ? Ui.img("ok") : Q("span")))
        .add(Q("td")
          .style("text-align:left")
          .add(!value || jsData.tooBig
              ? Q("span").text("false")
              : Ui.link(() -> setTo(false))
                .klass("link")
                .text("false")
            )))
    ;
    wg
      .removeAll()
      .add(Q("div")
        .klass("head")
        .text("Boolean"))
      .add(toNullDiv)
      .add(tb)
    ;
  }

  // Control -------------------------------------------------------------------

  function setTo(value: Bool): Void {
    final v = value ? "'true'" : "'false'";
    if (!Ui.confirm(_args(_("Set field to %0?"), [v]))) return;
    fn(FieldChange.mkBoolean(jsData.control, value), None);
  }
}

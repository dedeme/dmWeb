// Copyright 25-Jan-2023 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Number editor.

package wgs;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Opt;
import dm.Dec;
import data.Tpath;
import data.JsData;
import data.FieldChange;
import I18n._;
import I18n._args;

class NumberEditor {
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

    final value = jsData.data.rf();
    final valueIn = Q("input")
      .att("type", "text")
      .style("width:200px")
      .value("" + value)
    ;
    final tb = jsData.tooBig
      ? Q("table")
          .att("align", "center")
          .add(Q("tr")
            .add(Q("td")
              .klass("border")
              .text("" + value)))
      : Q("table")
          .att("align", "center")
          .add(Q("tr")
            .add(Q("td")
              .add(valueIn)))
          .add(Q("tr")
            .add(Q("td")
              .style("text-align:center")
              .add(Ui.link(() -> setTo(valueIn.getValue()))
                .klass("link")
                .text(_("Change")))))
    ;
    wg
      .removeAll()
      .add(Q("div")
        .klass("head")
        .text("Number"))
      .add(toNullDiv)
      .add(tb)
    ;
  }

  // Control -------------------------------------------------------------------

  function setTo(value: String): Void {
    switch (Dec.from(value)) {
      case Some(v): {
        if (!Ui.confirm(_args(_("Set field to %0?"), ["" + v]))) return;
        final r = Dec.round(v, 0);
        if (Dec.eq(v, r, 0.0000000001))
          fn(FieldChange.mkInt(jsData.control, Std.int(r)), None);
        else
          fn(FieldChange.mkFloat(jsData.control, v), None);
      }
      case None:
        Ui.alert(_args(_("'%0' is not a valid number"), [value]));
    }
  }
}

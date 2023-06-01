// Copyright 25-Jan-2023 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// String editor.

package wgs;

using StringTools;
import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Opt;
import data.Tpath;
import data.JsData;
import data.FieldChange;
import I18n._;
import I18n._args;

class StringEditor {
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

    final ltrim = Q("input")
      .att("type", "checkbox")
      .checked(true)
    ;
    final ltrimSpan = Q("span")
      .add(ltrim)
      .add(Q("span").text("ltrim"))
    ;
    final rtrim = Q("input")
      .att("type", "checkbox")
      .checked(true)
    ;
    final rtrimSpan = Q("span")
      .add(rtrim)
      .add(Q("span").text("rtrim"))
    ;

    final value = jsData.data.rs();
    final area = Q("textarea")
      .att("rows", 5)
      .att("cols", 80)
      .att("spellcheck", false)
      .disabled(false)
      .text(value)
    ;
    if (jsData.tooBig) area.disabled(true);

    final tb = jsData.tooBig
      ? Q("table")
          .att("align", "center")
          .add(Q("tr")
            .add(Q("td")
              .add(area)))
      : Q("table")
          .att("align", "center")
          .add(Q("tr")
            .add(Q("td")
              .style("width:5px;white-space:nowrap")
              .add(ltrimSpan))
            .add(Q("td")
              .style("width:5px;white-space:nowrap")
              .add(rtrimSpan))
            .add(Q("td")))
          .add(Q("tr")
            .add(Q("td")
              .att("colspan", 3)
              .add(area)))
          .add(Q("tr")
            .add(Q("td")
              .att("colspan", 3)
              .style("text-align:center")
              .add(Ui.link(() ->
                  setTo(ltrim.getChecked(), rtrim.getChecked(), area.getValue())
                )
                .klass("link")
                .text(_("Change")))))
    ;
    wg
      .removeAll()
      .add(Q("div")
        .klass("head")
        .text("String"))
      .add(toNullDiv)
      .add(tb)
    ;
  }

  // Control -------------------------------------------------------------------

  function setTo(ltrim: Bool, rtrim: Bool, value: String): Void {
    if (!Ui.confirm(_("Change field value?"))) return;
    final v = ltrim && rtrim
      ? value.trim()
      : ltrim
        ? value.ltrim()
        : rtrim
          ? value.rtrim()
          : value
    ;
    fn(FieldChange.mkString(jsData.control, v), None);
  }
}

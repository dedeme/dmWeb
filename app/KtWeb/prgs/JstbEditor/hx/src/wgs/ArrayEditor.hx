// Copyright 04-Feb-2023 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Array editor.

package wgs;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Dec;
import dm.Opt;
import data.Tpath;
import data.Field; // FieldEntry
import data.FieldCf; // IndexKeyFilter
import data.JsData;
import data.FieldChange;
import I18n._;
import I18n._args;

class ArrayEditor {
  final wg: Domo;
  final tpath: Tpath;
  final jsData: JsData;
  final arrSize: Int;
  final arrValues: Array<ArrEntry>;
  final fn: (FieldChange, Option<Tpath>) -> Void;

  public function new (
    wg: Domo, tpath: Tpath, jsData: JsData,
    fn: (FieldChange, Option<Tpath>) -> Void
  ) {
    this.wg = wg;
    this.tpath = tpath;
    this.jsData = jsData;
    this.fn = fn;

    final d = jsData.data.ra();
    arrSize = d[0].ri();
    arrValues = d[1].ra().map(e -> {
      final pair = e.ra();
      new ArrEntry(pair[0].ri(), pair[1].rs());
    });
  }

  // View ----------------------------------------------------------------------

  public function show () {
    final toNullDiv = Q("div");
    final filtersDiv = Q("div");

    final type = jsData.tooBig ? -1 : jsData.type;
    new ToNullWg(toNullDiv, jsData.control, type, fn).show();

    filterWg(filtersDiv);

    final tb = jsData.tooBig
      ? Q("table")
        .klass("main")
        .add(Q("tr")
          .add(Q("td")
            .add(Q("table")
              .klass("main")
              .add(Q("tr")
                .add(Q("td"))
                .add(Q("td")
                  .style("text-align:right")
                  .text("" + arrValues.length + " / " + arrSize))))))
        .add(Q("tr")
          .add(Q("table")
            .att("align", "center")
            .adds(arrSize == 0
                ? [Q("tr")
                    .add(Q("td")
                      .klass("frame")
                      .style("text-align:center")
                      .text(_("Without Entries")))
                  ]
                : arrValues.map(v -> Q("tr")
                    .add(Q("td")
                      .add(Ui.link(() -> goto(v.ix))
                        .klass("link")
                        .text("" + v.ix)))
                    .add(Q("td")
                      .add(Q("textarea")
                        .att("rows", 2)
                        .att("cols", 80)
                        .att("spellcheck", false)
                        .disabled(true)
                        .text(v.s)))))))
      : Q("table")
        .klass("main")
        .add(Q("tr")
          .add(Q("td")
            .add(Q("table")
              .klass("main")
              .add(Q("tr")
                .add(Q("td")
                  .add(Ui.link(newEntry)
                    .klass("link")
                    .text(_("Add a 'null' entry"))))
                .add(Q("td")
                  .style("text-align:right")
                  .text("" + arrValues.length + " / " + arrSize))))))
        .add(Q("tr")
          .add(Q("table")
            .att("align", "center")
            .adds(arrSize == 0
                ? [Q("tr")
                    .add(Q("td")
                      .klass("frame")
                      .style("text-align:center")
                      .text(_("Without Entries")))
                  ]
                : arrValues.map(v -> Q("tr")
                    .add(Q("td")
                      .style("text-align:right")
                      .add(Ui.link(() -> goto(v.ix))
                        .klass("link")
                        .text("" + v.ix)))
                    .add(Q("td")
                      .add(Ui.link(() -> copy(v.ix))
                        .klass("link")
                        .add(Ui.img("copy")
                          .att("title", _("Duplicate"))
                          .style("vertical-align:top"))))
                    .add(Q("td")
                      .add(Ui.link(() -> del(v.ix))
                        .klass("link")
                        .add(Ui.img("delete")
                          .att("title", _("Delete"))
                          .style("vertical-align:top"))))
                    .add(Q("td")
                      .add(Q("textarea")
                        .att("rows", 2)
                        .att("cols", 80)
                        .att("spellcheck", false)
                        .disabled(true)
                        .text(v.s)))))))
    ;

    wg
      .removeAll()
      .add(Q("div")
        .klass("head")
        .text("Array"))
      .add(toNullDiv)
      .add(filtersDiv)
      .add(tb)
    ;
  }

  function filterWg (wg: Domo): Void {

    final indexCheck = Q("input").att("type", "checkbox");
    final from = Q("input").att("type", "text").style("width:80px").disabled(true);
    final to = Q("input").att("type", "text").style("width:80px").disabled(true);
    indexCheck.on(CHANGE, () -> {
      if (indexCheck.getChecked()) {
        from.disabled(false);
        to.disabled(false);
      } else {
        from.disabled(true).value("");
        to.disabled(true).value("");
      }
    });

    final valueCheck = Q("input").att("type", "checkbox");
    final valueFilter = Q("input").att("type", "text").style("width:200px").disabled(true);
    valueCheck.on(CHANGE, () -> {
      if (valueCheck.getChecked()) {
        valueFilter.disabled(false);
      } else {
        valueFilter.disabled(true).value("");
      }
    });

    switch (tpath.fieldCf) {
      case None: {};
      case Some(cf): {
        switch (cf.indexKeyFilter) {
          case None: {}
          case Some(ixf): {
            indexCheck.checked(true);
            from.value("" + ixf.startRange).disabled(false);
            to.value("" + ixf.endRange).disabled(false);
          }
        }
        switch (cf.valueFilter) {
          case None: {}
          case Some(vf): {
            valueCheck.checked(true);
            valueFilter.value(vf).disabled(false);
          }
        }
      }
    }

    wg
      .removeAll()
      .add(Q("table")
        .klass("main")
        .add(Q("tr")
          .add(Q("td")
            .att("colspan", 2)
            .add(Ui.hrule(_("Filters")))))
        .add(Q("tr")
          .add(Q("td")
            .style("width:50%")
            .add(Q("table")
              .klass("main")
              .add(Q("tr")
                .add(Q("td")
                  .style("width:5px")
                  .add(indexCheck))
                .add(Q("td")
                  .att("colspan", 2)
                  .style("text-align:left")
                  .text(_("Index filter"))))
              .add(Q("tr")
                .add(Q("td")
                  .att("colspan", 2)
                  .style("width:5px;text-align:right")
                  .add(Ui.link(() -> if (indexCheck.getChecked()) from.value("0"))
                    .klass("link")
                    .text(_("From:"))))
                .add(Q("td")
                  .style("text-align:left")
                  .add(from)))
              .add(Q("tr")
                .add(Q("td")
                  .att("colspan", 2)
                  .style("width:5px;text-align:right")
                  .add(Ui.link(() -> if (indexCheck.getChecked()) to.value(arrSize))
                    .klass("link")
                    .text(_("To:"))))
                .add(Q("td")
                  .style("text-align:left")
                  .add(to)))))
          .add(Q("td")
            .style("vertical-align: top")
            .add(Q("table")
              .add(Q("tr")
                .add(Q("td")
                  .style("width:5px")
                  .add(valueCheck))
                .add(Q("td")
                  .style("text-align:left")
                  .text(_("Values filter"))))
              .add(Q("tr")
                .add(Q("td"))
                .add(Q("td")
                  .style("text-align:left")
                  .add(valueFilter))))))
        .add(Q("tr")
          .add(Q("td")
            .att("colspan", 2)
            .style("text-align:right")
            .add(Ui.link(() -> filter (
                  indexCheck.getChecked(), from.getValue(), to.getValue(),
                  valueCheck.getChecked(), valueFilter.getValue()
                ))
              .klass("link")
              .text(_("Filter")))))
        .add(Q("tr")
          .add(Q("td")
            .att("colspan", 2)
            .add(Q("hr")))))
    ;
  }

  // Control -------------------------------------------------------------------

  function filter (
    indexActive: Bool, from: String, to: String,
    valueActive: Bool, valueFilter: String
  ): Void {
    var err = "";

    if (indexActive || valueActive) {
      var ikf = None;
      if (indexActive) {
        if (from == "") err = _("'From' value is missing");
        else if (to == "") err = _("'To' value is missing");
        else if (!Dec.digits(from)) err = _("'From' is not a valid index");
        else if (!Dec.digits(to)) err = _("'To' is not a valid index");
        if (err == "") {
          final fromN = Std.parseInt(from);
          final toN = Std.parseInt(to);
          if (fromN < 0) err = _("'From' < 0");
          else if (toN > arrSize) err = _args(_("'To' > array size (%0)"), [""+arrSize]);
          else if (toN <= fromN) err = _("'To' is not greater than 'From'");
          else ikf = Some(IndexKeyFilter.mkArray(fromN, toN));
        }
      }

      var vf = None;
      if (valueActive) {
        if (valueFilter == "") err = _("'Values filter' is missing");
        else vf = Some(valueFilter);
      }
      tpath.fieldCf = Some(new FieldCf(ikf, vf));
    } else {
      tpath.fieldCf = None;
    }

    if (err != "") {
      Ui.alert(err);
      return;
    }

    pgs.Editor.reload(tpath);
  }

  function goto (ix: Int): Void {
    tpath.field.fieldPath.push(FieldEntry.mkArray(ix));
    tpath.fieldCf = None;
    pgs.Editor.reload(tpath);
  }

  function newEntry (): Void {
    if (!Ui.confirm(_("Add a 'null' entry?"))) return;
    final newTpath = new Tpath(
      tpath.table,
      tpath.field,
      Some(new FieldCf(Some(IndexKeyFilter.mkArray(0, 1)), None))
    );
    fn(FieldChange.mkArray(jsData.control, -1, false), Some(newTpath));
  }

  function copy(ix: Int): Void {
    if (!Ui.confirm(_args(_("Add a copy of '%0'?"), [""+ix]))) return;
    final newTpath = new Tpath(
      tpath.table,
      tpath.field,
      Some(new FieldCf(Some(IndexKeyFilter.mkArray(0, 1)), None))
    );
    fn(FieldChange.mkArray(jsData.control, ix, true), Some(newTpath));
  }

  function del(ix: Int): Void {
    if (!Ui.confirm(_args(_("Remove entry '%0'?"), [""+ix]))) return;
    fn(FieldChange.mkArray(jsData.control, ix, false), None);
  }

}

private class ArrEntry {
  public final ix: Int;
  public final s: String;

  public function new (ix: Int, s: String) {
    this.ix = ix;
    this.s = s;
  }
}

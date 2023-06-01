// Copyright 06-Feb-2023 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Map editor.

package wgs;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Dec;
import dm.Opt;
import dm.It;
import dm.ModalBox;
import data.Tpath;
import data.Field; // FieldEntry
import data.FieldCf; // IndexKeyFilter
import data.JsData;
import data.FieldChange;
import I18n._;
import I18n._args;

class MapEditor {
  final wg: Domo;
  final tpath: Tpath;
  final jsData: JsData;
  final allKeys: Array<String>;
  final mapValues: Map<String, String>;
  final fn: (FieldChange, Option<Tpath>) -> Void;

  final boxWg = Q("div");
  final modalBox: ModalBox;

  public function new (
    wg: Domo, tpath: Tpath, jsData: JsData,
    fn: (FieldChange, Option<Tpath>) -> Void
  ) {
    this.wg = wg;
    this.tpath = tpath;
    this.jsData = jsData;
    this.fn = fn;

    final d = jsData.data.ra();
    allKeys = d[0].ra().map(k -> k.rs());
    mapValues = [];
    for (k => v in d[1].ro()) mapValues[k] = v.rs();

    modalBox = new ModalBox(boxWg, false);
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
                  .text("" + It.fromMap(mapValues).count() + " / " + allKeys.length))))))
        .add(Q("tr")
          .add(Q("table")
            .att("align", "center")
            .adds(allKeys.length == 0
                ? [Q("tr")
                    .add(Q("td")
                      .klass("frame")
                      .style("text-align:center")
                      .text(_("Without Entries")))
                  ]
                : It.fromMap(mapValues).map(tp -> Q("tr")
                    .add(Q("td")
                      .add(Ui.link(() -> goto(tp.e1))
                        .klass("link")
                        .text(tp.e1)))
                    .add(Q("td")
                      .add(Q("textarea")
                        .att("rows", 2)
                        .att("cols", 80)
                        .att("spellcheck", false)
                        .disabled(true)
                        .text(tp.e2)))))))
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
                  .text("" + It.fromMap(mapValues).count() + " / " + allKeys.length))))))
        .add(Q("tr")
          .add(Q("table")
            .att("align", "center")
            .adds(allKeys.length == 0
                ? [Q("tr")
                    .add(Q("td")
                      .klass("frame")
                      .style("text-align:center")
                      .text(_("Without Entries")))
                  ]
                : It.fromMap(mapValues).map(tp -> Q("tr")
                    .add(Q("td")
                      .style("text-align:right")
                      .add(Ui.link(() -> goto(tp.e1))
                        .klass("link")
                        .text("" + tp.e1)))
                    .add(Q("td")
                      .add(Ui.link(() -> changeKey(tp.e1))
                        .klass("link")
                        .add(Ui.img("key")
                          .att("title", _("Change Key"))
                          .style("vertical-align:top"))))
                    .add(Q("td")
                      .add(Ui.link(() -> copy(tp.e1))
                        .klass("link")
                        .add(Ui.img("copy")
                          .att("title", _("Duplicate"))
                          .style("vertical-align:top"))))
                    .add(Q("td")
                      .add(Ui.link(() -> del(tp.e1))
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
                        .text(tp.e2)))))))
    ;

    wg
      .removeAll()
      .add(Q("div")
        .klass("head")
        .text("Object"))
      .add(toNullDiv)
      .add(filtersDiv)
      .add(tb)
      .add(modalBox.wg)
    ;
  }

  function filterWg (wg: Domo): Void {

    final keyCheck = Q("input").att("type", "checkbox");
    final keyFilter = Q("input").att("type", "text").style("width:250px").disabled(true);
    final startsRd = Q("input").att("type", "radio").att("name", "opt").disabled(true);
    final inRd = Q("input").att("type", "radio").att("name", "opt").disabled(true);
    final equalsRd = Q("input").att("type", "radio").att("name", "opt").disabled(true);
    final upCaseCheck = Q("input").att("type", "checkbox").disabled(true);
    keyCheck.on(CHANGE, () -> {
      if (keyCheck.getChecked()) {
        keyFilter.disabled(false);
        startsRd.disabled(false).checked(true);
        inRd.disabled(false);
        equalsRd.disabled(false);
        upCaseCheck.disabled(false);
      } else {
        keyFilter.disabled(true).value("");
        startsRd.disabled(true).checked(false);
        inRd.disabled(true).checked(false);
        equalsRd.disabled(true).checked(false);
        upCaseCheck.disabled(true).checked(false);
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
          case Some(kf): {
            keyCheck.checked(true);
            keyFilter.value(kf.text).disabled(false);
            startsRd.disabled(false).checked(kf.searchType == FieldCf.STARTS);
            inRd.disabled(false).checked(kf.searchType == FieldCf.IN);
            equalsRd.disabled(false).checked(kf.searchType == FieldCf.EQUALS);
            upCaseCheck.disabled(false).checked(kf.isUpper);
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
                  .add(keyCheck))
                .add(Q("td")
                  .style("text-align:left")
                  .text(_("Key filter"))))
              .add(Q("tr")
                .add(Q("td"))
                .add(Q("td")
                  .style("text-align:left")
                  .add(keyFilter)))
              .add(Q("tr")
                .add(Q("td"))
                .add(Q("td")
                  .add(Q("table")
                    .add(Q("tr")
                      .add(Q("td")
                        .add(startsRd)
                        .add(Q("span").text(_("Starts")))
                        .add(Q("span").html("&nbsp;&nbsp;"))
                        .add(inRd)
                        .add(Q("span").text(_("In")))
                        .add(Q("span").html("&nbsp;&nbsp;"))
                        .add(equalsRd)
                        .add(Q("span").text(_("Equals"))))))))
              .add(Q("tr")
                .add(Q("td"))
                .add(Q("td")
                  .add(Q("table")
                    .add(Q("tr")
                      .add(Q("td")
                        .add(upCaseCheck)
                        .add(Q("span").text(_("Uppercase"))))))))))

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
                  keyCheck.getChecked(),
                  equalsRd.getChecked()
                    ? FieldCf.EQUALS
                    : inRd.getChecked()
                      ? FieldCf.IN
                      : FieldCf.STARTS,
                  upCaseCheck.getChecked(), keyFilter.getValue(),
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

  function setBoxWg (fn: String -> Void): Void {
    function control (k: String): String {
      if (k == "") return _("Key entry is empty");
      else if (allKeys.contains(k)) return _("Key is duplicate");
      else return "";
    }

    final keyIn = Q("input")
      .att("id", "keyIn")
      .att("type", "text")
      .style("width:250px");
    boxWg
      .removeAll()
      .add(Q("div")
        .klass("head")
        .text(_("New Key")))
      .add(Q("table")
        .add(Q("tr")
          .add(Q("td")
            .add(keyIn)))
        .add(Q("tr")
          .add(Q("td")
            .style("text-align:right")
            .add(Q("button")
              .text(_("Cancel"))
              .on(CLICK, () -> modalBox.show(false)))
            .add(Q("span").html("&nbsp;"))
            .add(Q("button")
              .text(_("Accept"))
              .on(CLICK, () -> {
                  final k = keyIn.getValue();
                  final err = control(k);
                  if (err == "") {
                    fn(k);
                    modalBox.show(false);
                  } else {
                    Ui.alert(err);
                  }
                })))))
    ;
  }

  // Control -------------------------------------------------------------------

  function filter (
    keyActive: Bool, searchType: Int, isUpper: Bool, text: String,
    valueActive: Bool, valueFilter: String
  ): Void {
    var err = "";

    if (keyActive || valueActive) {
      var ikf = None;
      if (keyActive) {
        if (text == "") err = _("'Key filter' is missing");
        else ikf = Some(IndexKeyFilter.mkMap(searchType, isUpper, text));
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

  function goto (key: String): Void {
    tpath.field.fieldPath.push(FieldEntry.mkMap(key));
    tpath.fieldCf = None;
    pgs.Editor.reload(tpath);
  }

  function newEntry (): Void {
    if (!Ui.confirm(_("Add a 'null' entry?"))) return;
    setBoxWg(newKey -> {
      final newTpath = new Tpath(
        tpath.table,
        tpath.field,
        Some(new FieldCf(
          Some(IndexKeyFilter.mkMap(FieldCf.EQUALS, false, newKey)),
          None
        ))
      );
      fn(
        FieldChange.mkMap(jsData.control, FieldChange.MAP_ADD, "", newKey),
        Some(newTpath)
      );
    });
    modalBox.show(true);
    Q("#keyIn").e.focus();
  }

  function changeKey(key: String): Void {
    if (!Ui.confirm(_args(_("Change key '%0'?"), [key]))) return;
    setBoxWg(newKey -> {
      final newTpath = new Tpath(
        tpath.table,
        tpath.field,
        Some(new FieldCf(
          Some(IndexKeyFilter.mkMap(FieldCf.EQUALS, false, newKey)),
          None
        ))
      );
      fn(
        FieldChange.mkMap(jsData.control, FieldChange.MAP_KEY, key, newKey),
        Some(newTpath)
      );
    });
    modalBox.show(true);
    Q("#keyIn").e.focus();
  }

  function copy(key: String): Void {
    if (!Ui.confirm(_args(_("Add a copy of '%0'?"), [key]))) return;
    setBoxWg(newKey -> {
      final newTpath = new Tpath(
        tpath.table,
        tpath.field,
        Some(new FieldCf(
          Some(IndexKeyFilter.mkMap(FieldCf.EQUALS, false, newKey)),
          None
        ))
      );
      fn(
        FieldChange.mkMap(jsData.control, FieldChange.MAP_DUP, key, newKey),
        Some(newTpath)
      );
    });
    modalBox.show(true);
    Q("#keyIn").e.focus();
  }

  function del(key: String): Void {
    if (!Ui.confirm(_args(_("Remove entry '%0'?"), [key]))) return;
    fn(
      FieldChange.mkMap(jsData.control, FieldChange.MAP_DEL, key, ""),
      None
    );
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

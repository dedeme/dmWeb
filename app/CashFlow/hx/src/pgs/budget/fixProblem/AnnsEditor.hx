// Copyright 30-Aug-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.budget.fixProblem;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.It;
import dm.Dec;
import data.Diary; // DiaryEntry
import wgs.NumberField;
import I18n._;
import I18n._args;

class AnnsEditor {
  final wg: Domo;
  final fnOk: Array<DiaryAnnotation> -> Void;

  final am: Float;
  final anns: Array<DiaryAnnotation>;
  final sels: Array<Domo> = [];

  var activated(default, null): Bool;



  public function new (
    wg: Domo, entry: DiaryEntry, fnOk: Array<DiaryAnnotation> -> Void
  ) {
    this.wg = wg;
    this.fnOk = fnOk;
    am = entry.am;
    anns = entry.anns;
    It.range(anns.length).each(i ->
      sels.push(Q("input")
        .att("type", "radio")
        .att("id", "in" + i)
        .att("name", "selAnn")
      )
    );
    sels[0].checked(true);

    activated = false;

    view();
  }

  // View ----------------------------------------------------------------------

  function view (): Void {
    wg.removeAll();
    if (!activated) return;

    wg
      .add(Q("table")
      .klass("main")
      .adds(It.range(anns.length).map(i ->
          Q("tr")
            .add(Q("td")
              .style("width: 5px")
              .add(sels[i]))
            .add(Q("td")
              .klass("frameTx")
              .text(anns[i].accId == "" ? "---" : anns[i].accId))
            .add(Q("td")
              .add(new NumberField(
                  "nf" + i,
                  "in" + (i == anns.length - 1 ? 0 : i + 1),
                  anns[i].am,
                  updateAmount
                ).wg))

        ))
      .add(Q("tr")
        .add(Q("td")
          .att("colspan", "3")
          .add(Q("hr"))))
      .add(Q("tr")
        .add(Q("td")
          .style("text-align: left")
          .add(Ui.link(addAcc).add(Ui.img("add"))))
        .add(Q("td")
          .style("text-align: left")
          .add(anns.length > 1
              ? Ui.link(delAcc).add(Ui.img("delete"))
              : Ui.lightImg("delete")
            ))
        .add(Q("td")
          .style("text-align: right")
          .add(Ui.link(ok).add(Ui.img("ok"))))))
    ;
  }

  // Control -------------------------------------------------------------------

  public function active (value: Bool): Void {
    activated = value;
    view();
  }

  public function updateAccount (acc: String): Void {
    var ix = 0;
    It.range(anns.length)
      .each(i -> {
        if (sels[i].getChecked()) {
          anns[i] = new DiaryAnnotation(acc, anns[i].am);
          ix = i;
        }
      });
    view();
    Q("#nf" + ix).e.focus();
  }

  public function updateAmount (
    fieldId: String, newValue: Float
  ): Void {
    final i = Std.parseInt(fieldId.substring(2));
    final oldAnn = anns[i];
    anns[i] = new DiaryAnnotation(oldAnn.accId, newValue);
    final sum = It.range(anns.length - 1).reduce(0.0, (r, i) -> r + anns[i].am);

    if (sum > am) {
      Ui.alert(_args(
        _("Sum of account values (%0) is greater than cash value (%1)"),
        [Dec.toIso(sum, 2), Dec.toIso(am, 2)]
      ));
      anns[i] = oldAnn;
      view();
      Q("#" + fieldId).e.focus();
      return;
    }

    final lastAnn = anns[anns.length - 1];
    anns[anns.length - 1] = new DiaryAnnotation(lastAnn.accId, am - sum);
    view();
  }

  public function addAcc (): Void {
    final i = anns.length;
    sels.push(Q("input")
      .att("type", "radio")
      .att("id", "in" + i)
      .att("name", "selAnn")
      .checked(true)
    );
    anns.push(new DiaryAnnotation("", 0));
    view();
  }

  public function delAcc (): Void {
    final i = anns.length - 1;
    sels.splice(i, 1);
    anns.splice(i, 1);

    sels[0].checked(true);

    final sum = It.range(anns.length - 1).reduce(0.0, (r, i) -> r + anns[i].am);
    final lastAnn = anns[anns.length - 1];
    anns[anns.length - 1] = new DiaryAnnotation(lastAnn.accId, am - sum);

    view();
  }

  public function ok (): Void {
    var sum = 0.0;
    var ix = 1;
    for (a in anns) {
      if (a.accId == "") {
        Ui.alert(_args(
          _("Account of annotation '%0' is missing"), [Std.string(ix)]
        ));
        return;
      }
      sum += a.am;
      ++ix;
    }

    if (!Dec.eq(sum, am, 0.001)) {
      Ui.alert(_args(
        _("Sum of annotations (%0) does not match the cash value (%1)"),
        [Dec.toIso(sum, 2), Dec.toIso(am, 2)]
      ));
      return;
    }

    fnOk(anns);
  }

}



// Copyright 21-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.It;
import dm.Js;
import data.Ann;
import wgs.GroupWg;
import I18n._;

/// Notes page.
class Notes {
  static final typeHelp = "Msg : " + _("Message") +
    "\nCm: " + _("Command")
  ;
  final wg: Domo;
  final anns: Array<Ann>;
  final groups: Array<String>;

  var id: Int;

  function new (wg: Domo, anns: Array<Ann>) {
    anns.sort((a1, a2) -> a1.group() == a2.group()
      ? a1.text > a2.text ? 1 : -1
      : a1.group() > a2.group() ? 1 : -1);
    this.wg = wg;
    this.anns = anns;
    final tmp = new Map<String, String>();
    for (a in anns) tmp.set(a.group(), "");
    groups = It.fromMap(tmp).map(tp -> tp.e1).to();

    id = -1;
  }

  // View ----------------------------------------------------------------------

  function show () {
    final tb = Q("table")
      .att("align", "center")
      .adds(trsNew())
      .adds(trs())
    ;

    wg
      .removeAll()
      .add(Q("div")
        .klass("head")
        .text(_("Notes")))
      .add(Q("div")
        .klass("separator"))
      .add(tb)
    ;
  }

  function trsNew () {
    final groupContainer = Q("div");
    final groupWg = new GroupWg(groupContainer, groups, "");
    if (id == -1) groupWg.editable = true;
    groupWg.show();
    final txWg = Q("textarea")
      .att("cols", 80)
      .att("rows", 3)
      .disabled(id != -1)
    ;

    return [
      Q("tr")
        .add(Q("td")
          .att("colspan", "2"))
        .add(th()
          .text(_("Group")))
        .add(th()
          .text(_("Text"))),
      Q("tr").add(Q("td").klass("line").att("colspan", "4")),
      Q("tr")
        .add(Q("td"))
        .add(Q("td")
          .add(id != -1
            ? Ui.lightImg("add")
                .setStyle("vertical-align", "middle")
            : Ui.link(() -> update(-1, groupWg, txWg))
              .add(Ui.img("add")
                .style("vertical-align:middle"))))
        .add(Q("td")
          .add(groupContainer))
        .add(Q("td")
          .add(txWg)),
      Q("tr").add(Q("td").att("colspan", "8").add(Q("hr")))
    ];
  }

  function mkTr (ann: Ann): Domo {
    final isSel = ann.id == id;
    final isNew = id == -1;

    final groupContainer = Q("div");
    final groupWg = new GroupWg(groupContainer, groups, ann.group());
    if (isSel) groupWg.editable = true;
    groupWg.show();
    final txWg = Q("textarea")
      .att("cols", 80)
      .att("rows", 3)
      .disabled(!isSel)
      .value(ann.text)
    ;

    return Q("tr")
      .add(Q("td")
        .add(isNew
          ? Ui.link(() -> edit(ann.id))
            .add(Ui.img("edit"))
          : isSel
            ? Ui.link(() -> editCancel())
              .add(Ui.img("cancel"))
            : Ui.lightImg("edit")))
      .add(Q("td")
        .add(isNew
          ? Ui.link(() -> del(ann.id))
            .add(Ui.img("delete"))
          : isSel
            ? Ui.link(() -> update(ann.id, groupWg, txWg))
              .add(Ui.img("enter"))
            : Ui.lightImg("delete")))
      .add(Q("td")
        .add(groupContainer))
      .add(Q("td")
        .add(txWg))
    ;
  }

  function trs () {
    if (anns.length == 0) {
      return [
        Q("tr")
          .add(th()
          .att("colspan", "7")
          .klass("frame")
          .text(_("Without entries")))
      ];
    }

    return It.from(anns).map(ann -> mkTr(ann)).to();
  }

  function th () {
    return Q("td").style("text-align:center");
  }

  // Control -------------------------------------------------------------------

  function update (
    id: Int, groupWg: GroupWg, txWg: Domo
  ): Void {
    final group = groupWg.entry.getValue().trim();
    final tx = txWg.getValue().trim();

    var err = "";
    if (group == "") err = _("Group is missing");
    if (tx == "") err = _("'Text' value is missing");

    if (err != "") {
      Ui.alert(err);
      return;
    }

    Cts.client.ssend([
      "source" => Js.ws("Notes"),
      "rq" => Js.ws(id == -1 ? "new" : "modify"),
      "ann" => new Ann(
          id, Ann_NOTE, Js.ws(group), Ann_MESSAGE, tx
        ).toJs()
    ], rp -> {
      mk(wg);
    });
  }

  function edit (id: Int): Void {
    this.id = id;
    show();
  }

  function editCancel (): Void {
    id = -1;
    show();
  }

  function del (id: Int): Void {
    if (!Ui.confirm(_("Delete annotation?"))) {
      return;
    }
    Cts.client.ssend([
      "source" => Js.ws("Notes"),
      "rq" => Js.ws("delete"),
      "id" => Js.wi(id)
    ], rp -> {
      mk(wg);
    });
  }

  // Static --------------------------------------------------------------------

  public static function mk (wg: Domo) {
    Cts.client.send([
      "source" => Js.ws("Notes"),
      "rq" => Js.ws("idata")
    ], rp -> {
      final anns = rp["anns"].ra().map(Ann.fromJs);
      new Notes(wg, anns).show();
    });
  }
}

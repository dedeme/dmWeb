// Copyright 20-Nov-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.It;
import dm.Js;
import dm.Dt;
import data.Ann;
import wgs.DaySelector;
import I18n._;

/// Periodic commands page.
class Periodic {
  final wg: Domo;
  final anns: Array<Ann>;

  var id: Int;

  function new (wg: Domo, anns: Array<Ann>) {
    anns.sort((a1, a2) -> a1.id - a2.id);
    this.wg = wg;
    this.anns = anns;

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
        .text(_("Periodic days")))
      .add(Q("div")
        .klass("separator"))
      .add(tb)
    ;
  }

  function trsNew () {
    final dayWg = Q("div");
    final dayDs = new DaySelector(dayWg, []);
    if (id == -1) dayDs.editable = true;
    dayDs.show();
    final hWg = Ui.select(
      "h-new", It.range(24).map(i -> Fns.formatN00(i)).to()
    ).disabled(id != -1);
    final mWg = Ui.select(
      "m-new", It.range(60).map(i -> Fns.formatN00(i)).to()
    ).disabled(id != -1);
    final txWg = Q("textarea")
      .att("spellcheck", false)
      .att("cols", 60)
      .att("rows", 3)
      .disabled(id != -1)
    ;

    return [
      Q("tr")
        .add(Q("td")
          .att("colspan", "2"))
        .add(th()
          .text(_("Day")))
        .add(th()
          .att("colspan", "2")
          .text(_("Hour - Minute")))
        .add(th()
          .text(_("Command")))
        .add(Q("td")),
      Q("tr").add(Q("td").klass("line").att("colspan", "8")),
      Q("tr")
        .add(Q("td"))
        .add(Q("td")
          .add(id != -1
            ? Ui.lightImg("add")
                .setStyle("vertical-align", "middle")
            : Ui.link(() -> update(
                -1, dayDs, hWg, mWg, txWg
              ))
              .add(Ui.img("add")
                .style("vertical-align:middle"))))
        .add(Q("td")
          .add(dayWg))
        .add(Q("td")
          .add(hWg))
        .add(Q("td")
          .add(mWg))
        .add(Q("td")
          .add(txWg))
        .add(Q("td")),
      Q("tr").add(Q("td").att("colspan", "8").add(Q("hr")))
    ];
  }

  function mkTr (ann: Ann): Domo {
    final isSel = ann.id == id;
    final isNew = id == -1;

    final dayWg = Q("div");
    final dayDs = new DaySelector(dayWg, ann.days());
    if (isSel) dayDs.editable = true;
    dayDs.show();
    final hWg = Ui.select(
        "h-new",
        It.range(24).map(i ->
          (ann.date().getHours() == i ? "+" : "") + Fns.formatN00(i)
        ).to()
      ).disabled(!isSel)
    ;
    final mWg = Ui.select(
        "m-new",
        It.range(60).map(i ->
          (ann.date().getMinutes() == i ? "+" : "") + Fns.formatN00(i)
        ).to()
      ).disabled(!isSel)
    ;
    final txWg = Q("textarea")
      .att("spellcheck", false)
      .att("cols", 60)
      .att("rows", 3)
      .disabled(!isSel)
      .value(ann.text)
    ;
    final runSpan = Q("span");

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
            ? Ui.link(() -> update(ann.id, dayDs, hWg, mWg, txWg))
              .add(Ui.img("enter"))
            : Ui.lightImg("delete")))
      .add(Q("td")
        .add(dayWg))
      .add(Q("td")
        .add(hWg))
      .add(Q("td")
        .add(mWg))
      .add(Q("td")
        .add(txWg))
      .add(Q("td")
        .add(runSpan
          .removeAll()
          .add(Ui.link(() -> run(runSpan, ann.id))
            .add(Ui.img("run")))))
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
    id: Int, dayDs: DaySelector,
    hWg: Domo, mWg: Domo, txWg: Domo
  ): Void {
    final days = dayDs.days;
    final tx = txWg.getValue().trim();

    var err = "";
    if (days.length == 0) err = _("Week days is missing");
    if (tx == "") err = _("Command value is missing");

    if (err != "") {
      Ui.alert(err);
      return;
    }

    final today = Date.now();
    final time =  new Date(
      today.getFullYear(), today.getMonth(), today.getDate(),
      Std.parseInt(hWg.getValue()), Std.parseInt(mWg.getValue()), 0
    );
    final time0 = Js.wf(Dt.timestamp(time));
    final time1 = Js.wa(days.map(e -> Js.wi(e)));

    Global.client.ssend([
      "prg" => Js.ws("Kron"),
      "source" => Js.ws("Periodic"),
      "rq" => Js.ws(id == -1 ? "new" : "modify"),
      "ann" => new Ann(
          id, Ann_PERIODIC, Js.wa([time0, time1]), tx
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
    Global.client.ssend([
      "prg" => Js.ws("Kron"),
      "source" => Js.ws("Periodic"),
      "rq" => Js.ws("delete"),
      "id" => Js.wi(id)
    ], rp -> {
      mk(wg);
    });
  }

  function run (runSpan: Domo, id: Int): Void {
    runSpan
      .removeAll()
      .add(Ui.img("wait.gif"))
    ;
    Global.client.ssend([
      "prg" => Js.ws("Kron"),
      "source" => Js.ws("Periodic"),
      "rq" => Js.ws("run"),
      "id" => Js.wi(id)
    ], rp -> {
      final error = rp["error"].rs();
      if (error != "") {
        Ui.alert(error);
        mk(wg);
      } else {
        runSpan
          .removeAll()
          .add(Ui.link(() -> run(runSpan, id))
            .add(Ui.img("run")))
        ;
      }
    });
  }

  // Static --------------------------------------------------------------------

  public static function mk (wg: Domo) {
    Global.client.send([
      "prg" => Js.ws("Kron"),
      "source" => Js.ws("Periodic"),
      "rq" => Js.ws("idata")
    ], rp -> {
      final anns = rp["anns"].ra().map(Ann.fromJs);
      new Periodic(wg, anns).show();
    });
  }
}

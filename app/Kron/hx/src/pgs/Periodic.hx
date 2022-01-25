// Copyright 21-Dic-2021 ºDeme
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
  static final typeHelp = "Msg : " + _("Message") +
    "\nCm: " + _("Command")
  ;
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
    final typeWg = Ui.select("type-new", ["Msg", "Cm"]).disabled(id != -1);
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
      .att("cols", 60)
      .att("rows", 3)
      .disabled(id != -1)
    ;

    return [
      Q("tr")
        .add(Q("td")
          .att("colspan", "2"))
        .add(th()
          .add(Q("span")
            .text(_("Type")))
          .add(Q("span")
            .att("title", typeHelp)
            .style("cursor:pointer;color:#800000")
            .html("<sup>*</sup>"))
            .on(CLICK, () -> Ui.alert(typeHelp)))
        .add(th()
          .text(_("Day")))
        .add(th()
          .att("colspan", "2")
          .text(_("Hour - Minute")))
        .add(th()
          .text(_("Command / Message")))
        .add(Q("td")),
      Q("tr").add(Q("td").klass("line").att("colspan", "8")),
      Q("tr")
        .add(Q("td"))
        .add(Q("td")
          .add(id != -1
            ? Ui.lightImg("add")
                .setStyle("vertical-align", "middle")
            : Ui.link(() -> update(
                -1, typeWg, dayDs, hWg, mWg, txWg
              ))
              .add(Ui.img("add")
                .style("vertical-align:middle"))))
        .add(Q("td")
          .add(typeWg))
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

    final typeWg = Ui.select("type-new", [
        ann.textType == Ann_MESSAGE ? "+Msg" : "Msg",
        ann.textType == Ann_COMMAND ? "+Cm" : "Cm",
      ]).disabled(!isSel)
    ;
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
      .att("cols", 60)
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
            ? Ui.link(() -> update(ann.id, typeWg, dayDs, hWg, mWg, txWg))
              .add(Ui.img("enter"))
            : Ui.lightImg("delete")))
      .add(Q("td")
        .add(typeWg))
      .add(Q("td")
        .add(dayWg))
      .add(Q("td")
        .add(hWg))
      .add(Q("td")
        .add(mWg))
      .add(Q("td")
        .add(txWg))
      .add(Q("td")
        .add(Ui.link(() -> run(ann))
          .add(Ui.img("run"))))
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
    id: Int, typeWg: Domo, dayDs: DaySelector,
    hWg: Domo, mWg: Domo, txWg: Domo
  ): Void {
    final type = switch(typeWg.getValue()) {
      case "Cm": Ann_COMMAND;
      default: Ann_MESSAGE;
    }
    final days = dayDs.days;
    final tx = txWg.getValue().trim();

    var err = "";
    if (days.length == 0) err = _("Week days is missing");
    if (tx == "") err = _("'Command / Message' value is missing");

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

    Cts.client.ssend([
      "source" => Js.ws("Periodic"),
      "rq" => Js.ws(id == -1 ? "new" : "modify"),
      "ann" => new Ann(
          id, Ann_PERIODIC, Js.wa([time0, time1]), type, tx
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
      "source" => Js.ws("Periodic"),
      "rq" => Js.ws("delete"),
      "id" => Js.wi(id)
    ], rp -> {
      mk(wg);
    });
  }

  function run (ann: Ann): Void {
    Cts.client.ssend([
      "source" => Js.ws("Periodic"),
      "rq" => Js.ws("run"),
      "ann" => ann.toJs()
    ], rp -> {
      if (Ui.confirm("Annotation executed.\nSee log?")) {
        js.Browser.location.assign("?home");
      }
    });
  }

  // Static --------------------------------------------------------------------

  public static function mk (wg: Domo) {
    Cts.client.send([
      "source" => Js.ws("Periodic"),
      "rq" => Js.ws("idata")
    ], rp -> {
      final anns = rp["anns"].ra().map(Ann.fromJs);
      new Periodic(wg, anns).show();
    });
  }
}

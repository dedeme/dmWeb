// Copyright 21-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.It;
import dm.Js;
import data.Ann;
import I18n._;

/// Periodic commands page.
class Init {
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
        .text(_("Initialization")))
      .add(Q("div")
        .klass("separator"))
      .add(tb)
    ;
  }

  function trsNew () {
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
          .text(_("Command")))
        .add(Q("td")),
      Q("tr").add(Q("td").klass("line").att("colspan", "8")),
      Q("tr")
        .add(Q("td"))
        .add(Q("td")
          .add(id != -1
            ? Ui.lightImg("add")
                .setStyle("vertical-align", "middle")
            : Ui.link(() -> update(-1, txWg))
              .add(Ui.img("add")
                .style("vertical-align:middle"))))
        .add(Q("td")
          .add(txWg))
        .add(Q("td")),
      Q("tr").add(Q("td").att("colspan", "4").add(Q("hr")))
    ];
  }

  function mkTr (ann: Ann): Domo {
    final isSel = ann.id == id;
    final isNew = id == -1;

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
            ? Ui.link(() -> update(ann.id, txWg))
              .add(Ui.img("enter"))
            : Ui.lightImg("delete")))
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

  function update (id: Int, txWg: Domo): Void {
    final tx = txWg.getValue().trim();

    var err = "";
    if (tx == "") err = _("'Command' value is missing");

    if (err != "") {
      Ui.alert(err);
      return;
    }

    Cts.client.ssend([
      "source" => Js.ws("Init"),
      "rq" => Js.ws(id == -1 ? "new" : "modify"),
      "ann" => new Ann(id, Ann_INIT, Js.wn(), Ann_COMMAND, tx).toJs()
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
      "source" => Js.ws("Init"),
      "rq" => Js.ws("delete"),
      "id" => Js.wi(id)
    ], rp -> {
      mk(wg);
    });
  }

  function run (ann: Ann): Void {
    Cts.client.ssend([
      "source" => Js.ws("Init"),
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
      "source" => Js.ws("Init"),
      "rq" => Js.ws("idata")
    ], rp -> {
      final anns = rp["anns"].ra().map(Ann.fromJs);
      new Init(wg, anns).show();
    });
  }
}


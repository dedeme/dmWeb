// Copyright 09-Oct-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

using StringTools;
import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.It;
import dm.Tp;
import dm.Opt;
import wgs.DirRow;
import data.TestRs;
import I18n._;

class Directories {
  var wg: Domo;
  var dirs: Map<String, TestRs>;
  final newIn = Q("input")
    .style("width:200px")
    .disabled(true)
  ;
  function new (wg: Domo, dirs: Map<String, TestRs>) {
    this.wg = wg;
    this.dirs = dirs;

    view();
  }

  // View ----------------------------------------------------------------------

  function view () {
    wg
      .removeAll()
      .add(mkNewWg())
      .add(mkTableWg())
    ;
  }

  function mkNewWg (): Domo {
    final editWg = Q("div");
    final doWg = Q("div");


    function editf () {
      function cancelf () {
        editWg
          .removeAll()
          .add(Ui.link(e -> editf())
            .add(Ui.img("edit"))
        );

        doWg
          .removeAll()
          .add(Ui.lightImg("editOk"))
        ;
        newIn
          .value("")
          .disabled(true)
        ;
      }

      editWg
        .removeAll()
        .add(Ui.link(e -> cancelf())
          .add(Ui.img("editCancel")))
      ;
      doWg
        .removeAll()
        .add(Ui.link(e -> newDirectory ())
          .add(Ui.img("editOk")))
      ;
      newIn.disabled(false);
    }

    editWg.add(Ui.link(e -> editf())
      .add(Ui.img("edit"))
    );
    doWg.add(Ui.lightImg("editOk"));

    return Q("div")
      .add(Q("div")
        .klass("head")
        .text(_("New")))
      .add(Q("table")
        .klass("frame")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td")
            .add(editWg))
          .add(Q("td")
            .add(doWg))
          .add(Q("td")
            .add(newIn))
        ))
    ;
  }

  function mkTableWg():  Domo {
    return Q("div")
      .add(Q("div")
        .klass("head")
        .text(_("Directories")))
      .add(Q("table")
        .klass("white")
        .att("align", "center")
        .adds(It.fromMap(dirs)
          .sort((tp1, tp2) -> tp1.e1 > tp2.e1 ? 1 : -1)
          .map(tp ->
            DirRow.mk(tp.e1, tp.e2, () -> mk(wg))
          ).to()))
    ;
  }

  // Control -------------------------------------------------------------------

  function newDirectory () {
    final id = cast(newIn.getValue(), String).trim();
    final err = Cts.validateDirId(id);
    if (err != "") {
      Ui.alert(err);
      return;
    }
    if (dirs.exists(id)) {
      Ui.alert(_("Directory is duplicated"));
      return;
    }
    Cts.client.ssend([
      "page" => Js.ws("directories"),
      "rq" => Js.ws("new"),
      "id" => Js.ws(id)
    ], rp -> {
      mk(wg);
    });
  }

  // Static --------------------------------------------------------------------

  public static function mk (wg: Domo): Void {
    Cts.client.send([
      "page" => Js.ws("directories"),
      "rq" => Js.ws("idata")
    ], rp -> {
      final data = Opt.eget(It.fromMap(rp["data"].ro()).map(tp ->
        new Tp(tp.e1, TestRs.FromJs(tp.e2))
      ).toMap());
      new Directories(wg, data);
    });
  }
}

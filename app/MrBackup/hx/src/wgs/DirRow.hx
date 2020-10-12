// Copyright 06-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package wgs;

using StringTools;
import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import data.TestRs;
import I18n._;
import I18n._args;

class DirRow {
  final geditDiv = Q("div");
  final execDiv = Q("div");
  final dirEditDiv = Q("div");
  final dirDelDiv = Q("div");
  final dirInDiv = Q("div").style("width: 220px");
  final pathEditDiv = Q("div");
  final pathOkDiv = Q("div");
  final pathInDiv = Q("div").style("width: 320px");

  final dirIn = Q("input").style("width: 200px");
  final pathIn = Q("input").style("width: 300px");

  var tr: Domo;
  var dirName: String;
  var test: TestRs;
  var reload: Void -> Void;

  var isGedit = false;
  var isDirEdit = false;
  var isPathEdit = false;

  function new (
    tr: Domo, dirName: String, test: TestRs, reload: Void -> Void
  ): Void {
    this.tr = tr;
    this.dirName = dirName;
    this.test = test;
    this.reload = reload;
    view();
  }

  // View ----------------------------------------------------------------------

  function view (): Void {
    mkGedit();
    mkDirEdit();
    mkDirDel();
    mkDirIn();
    mkPathEdit();
    mkPathOk();
    mkPathIn();

    tr
      .removeAll()
      .add(Q("td").add(geditDiv))
      .add(execDiv
        .removeAll()
        .add(exec()))
      .add(sep())
      .add(Q("td").add(dirEditDiv))
      .add(Q("td").add(dirDelDiv))
      .add(Q("td").add(dirInDiv))
      .add(sep())
      .add(Q("td").add(pathEditDiv))
      .add(Q("td").add(pathOkDiv))
      .add(Q("td").add(pathInDiv))
      .add(sep())
      .add(info())
      .add(sep())
      .add(dirsView())
    ;
  }

  function sep (): Domo {
    return Q("td")
      .style(
        "border-right: 1px solid rgb(110,130,150);" +
        "border-left: 1px solid rgb(110,130,150);"
      )
    ;
  }

  function exec (): Domo {
    var tx = _("Update");
    var fn = e -> execUpdate();

    if (test.notInBase) {
      tx = _("Copy directory to base");
      fn = e -> copyToBase();
    } else if (test.isMissing || !test.synchronized) {
      tx = _("Copy directory base in the others");
      fn = e -> copyFromBase();
    } else if (!test.withPathTxt) {
      tx = _("Create 'path.txt'");
      fn = e -> createPathTxt();
    } else if (test.path == "") {
      tx = _("Put a directory in 'path.txt' manually");
      fn = e -> Ui.alert(tx);
    } else if (!test.pathOk) {
      tx = _("Put a valid directory in 'path.txt' manually");
      fn = e -> Ui.alert(tx);
    }

    return Q("td")
      .add(Ui.link(fn)
        .att("title", tx)
        .add(Ui.img("run")))
    ;
  }

  function info (): Domo {
    var tx = _("Ok");
    var img = "warning";

    if (test.notInBase) {
      tx = _("Directory is not in base");
    } else if (test.isMissing) {
      tx = _("Directory is missing in some pool");
    } else if (!test.synchronized) {
      tx = _("Directories are no synchronized");
    } else if (!test.withPathTxt) {
      tx = _("'path.txt' is missing");
    } else if (test.path == "") {
      tx = _("'path.txt' is empty");
    } else if (!test.pathOk) {
      tx = _args(_("'%0' not found"), [test.path]);
    }

    if (tx == _("Ok")) img = "well";

    return Q("td")
      .add(Ui.img(img)
        .att("title", tx))
    ;
  }

  function dirsView (): Domo {
    return Q("td")
      .add(Ui.link(e -> showDirs())
        .add(Ui.img("view")))
    ;
  }

  function mkGedit (): Void {
    geditDiv.removeAll();
    if (isGedit) {
      geditDiv.add(Ui.link(e -> geditOff())
        .add(Ui.img("editCancel")))
      ;
    } else {
      if (test.notInBase || test.isMissing || !test.synchronized) {
        geditDiv.add(Ui.lightImg("edit"));
      } else {
        geditDiv.add(Ui.link(e -> geditOn())
          .add(Ui.img("edit")))
        ;
      }
    }
  }

  function mkDirEdit (): Void {
    dirEditDiv.removeAll();
    if (isGedit) {
      if (isDirEdit) {
        dirEditDiv.add(Ui.link(e -> dirEditOff())
          .add(Ui.img("editCancel")))
        ;
      } else {
        dirEditDiv.add(Ui.link(e -> dirEditOn())
          .add(Ui.img("edit")))
        ;
      }
    } else {
      dirEditDiv.add(Ui.lightImg("edit"));
    }
  }

  function mkDirDel (): Void {
    dirDelDiv.removeAll();
    if (isGedit) {
      if (isDirEdit) {
        dirDelDiv.add(Ui.link(e -> changeDir())
          .add(Ui.img("editOk")))
        ;
      } else {
        dirDelDiv.add(Ui.link(e -> delDir())
          .add(Ui.img("delete")))
        ;
      }
    } else {
      dirDelDiv.add(Ui.lightImg("delete"));
    }
  }

  function mkDirIn (): Void {
    dirInDiv.removeAll();
    if (isDirEdit) {
      dirIn.value(dirName);
      dirInDiv.add(dirIn);
    } else {
      dirInDiv.text(cut(dirName, 195));
    }
  }

  function mkPathEdit (): Void {
    pathEditDiv.removeAll();
    if (isGedit && test.withPathTxt) {
      if (isPathEdit) {
        pathEditDiv.add(Ui.link(e -> pathEditOff())
          .add(Ui.img("editCancel")))
        ;
      } else {
        pathEditDiv.add(Ui.link(e -> pathEditOn())
          .add(Ui.img("edit")))
        ;
      }
    } else {
      pathEditDiv.add(Ui.lightImg("edit"));
    }
  }

  function mkPathOk (): Void {
    pathOkDiv.removeAll();
    if (isGedit && test.withPathTxt) {
      if (isPathEdit) {
        pathOkDiv.add(Ui.link(e -> changePath())
          .add(Ui.img("editOk")))
        ;
      } else {
        pathOkDiv.add(Ui.lightImg("editOk"));
      }
    } else {
      pathOkDiv.add(Ui.lightImg("editOk"));
    }
  }

  function mkPathIn (): Void {
    pathInDiv.removeAll();
    if (isPathEdit && test.withPathTxt) {
      pathIn.value(test.path);
      pathInDiv.add(pathIn);
    } else {
      pathInDiv.text(cut(test.path, 295));
    }
  }

  // Control -------------------------------------------------------------------

  function geditOn (): Void {
    isGedit = true;
    view();
  }

  function geditOff (): Void {
    isGedit = false;
    isDirEdit = false;
    isPathEdit = false;
    view();
  }

  function dirEditOff (): Void {
    isDirEdit = false;
    view();
  }

  function dirEditOn (): Void {
    isDirEdit = true;
    view();
  }

  function changeDir (): Void {
    final newId = cast(dirIn.getValue(), String).trim();
    final rs = Cts.validateDirId(newId);
    if (rs != "") {
      Ui.alert(rs);
      return;
    }

    Cts.client.ssend([
      "page" => Js.ws("dirRow"),
      "rq" => Js.ws("changeDir"),
      "id" => Js.ws(dirName),
      "newId" => Js.ws(newId)
    ], rp -> {
      reload();
    });
  }

  function delDir (): Void {
    if (!Ui.confirm(_args(_("Delete '%0'?"), [dirName]))) {
      return;
    }
    Cts.client.ssend([
      "page" => Js.ws("dirRow"),
      "rq" => Js.ws("delete"),
      "id" => Js.ws(dirName),
    ], rp -> {
      reload();
    });
  }

  function pathEditOff (): Void {
    isPathEdit = false;
    view();
  }

  function pathEditOn (): Void {
    isPathEdit = true;
    view();
  }

  function changePath (): Void {
    final newPath = cast(pathIn.getValue(), String).trim();
    if (test.withBackups) {
      if (!Ui.confirm(
        _("There are backups inside the directory.\nChange the path anyway?")
      )) {
        return;
      }
    }

    Cts.client.ssend([
      "page" => Js.ws("dirRow"),
      "rq" => Js.ws("changePath"),
      "id" => Js.ws(dirName),
      "newPath" => Js.ws(newPath)
    ], rp -> {
      reload();
    });
  }

  function showDirs (): Void {
    Cts.client.send([
      "page" => Js.ws("dirRow"),
      "rq" => Js.ws("showDirs"),
      "id" => Js.ws(dirName)
    ], rp -> {
    });
  }

  // Exec functions

  function copyToBase (): Void {
    Cts.client.ssend([
      "page" => Js.ws("dirRow"),
      "rq" => Js.ws("copyToBase"),
      "id" => Js.ws(dirName)
    ], rp -> {
      reload();
    });
  }

  function copyFromBase (): Void {
    Cts.client.ssend([
      "page" => Js.ws("dirRow"),
      "rq" => Js.ws("copyFromBase"),
      "id" => Js.ws(dirName)
    ], rp -> {
      reload();
    });
  }

  function createPathTxt (): Void {
    Cts.client.ssend([
      "page" => Js.ws("dirRow"),
      "rq" => Js.ws("createPathTxt"),
      "id" => Js.ws(dirName)
    ], rp -> {
      reload();
    });
  }

  function execUpdate (): Void {
    execDiv
      .removeAll()
      .add(Ui.img("wait.gif"))
    ;
    Cts.client.ssend([
      "page" => Js.ws("dirRow"),
      "rq" => Js.ws("update"),
      "id" => Js.ws(dirName)
    ], rp -> {
      execDiv
        .removeAll()
        .add(exec())
      ;
    });
  }

  // Static --------------------------------------------------------------------

  /// Constructor.
  /// Returns a 'tr'.
  ///   dirName: Directory name.
  ///   test   : State of directory.
  ///   reload : Function for reload directory page.
  public static function mk (
    dirName: String, test: TestRs, reload: Void -> Void
  ): Domo {
    final r = Q("tr");
    new DirRow(r, dirName, test, reload);
    return r;
  }

  static function cut (tx: String, length: Int): String {
    while (Cts.measure(tx) > length) {
      tx = "..." + tx.substring(4);
    }
    return tx;
  }
}

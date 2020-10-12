// Copyright 05-Oct-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import data.LogRow;
import wgs.Log;
import I18n._;

class Summary {
  final dirsDiv = Q("div");
  final filesDiv = Q("div");
  final bkDiv = Q("div");
  final logDiv = Q("div");

  var wgx: Domo;

  public function new (wg: Domo) {
    this.wgx = wg;
    view();
  }

  // View ----------------------------------------------------------------------

  function view () {
    function load (fn: Array<LogRow> -> Void) {
      Cts.client.send([
        "page" => Js.ws("summary"),
        "rq" => Js.ws("log")
      ], rp -> {
        final log = rp["log"].ra();
        fn(log.map(js -> LogRow.fromJs(js)));
      });
    }

    function reset (fn: () -> Void) {
      Cts.client.send([
        "page" => Js.ws("summary"),
        "rq" => Js.ws("resetLog")
      ], rp -> {
        fn();
      });
    }

    wgx
      .removeAll()
      .add(dirsDiv)
      .add(filesDiv)
      .add(bkDiv)
      .add(logDiv)
    ;
    Log.mk(logDiv, load, reset);
    updateDirs();
    updateFiles(false, false);
    update(false);
  }

  function updateDirs (): Void {
    dirsDiv
      .removeAll()
      .add(Q("div")
        .klass("head")
        .text(_("Directories")))
    ;

    Cts.client.send([
      "page" => Js.ws("summary"),
      "rq" => Js.ws("dirs")
    ], rp -> {
      final isBusy = rp["isBusy"].rb();
      final pools = rp["pools"].ri();
      final badPools = rp["badPools"].ri();
      final dirs = rp["dirs"].ri();
      final badDirs = rp["badDirs"].ri();

      if (isBusy) {
        dirsDiv
          .add(Q("table")
            .att("align", "center")
            .add(Q("tr")
              .add(Q("td")
                .klass("frame")
                .text(_("Server is busy.")))))
        ;
        return;
      }

      dirsDiv
        .add(Q("table")
          .att("align", "center")
          .klass("white")
          .add(mkTr(_("Pools"), -1, pools))
          .add(mkTr(_("Pools in bad condition"), badPools, -1))
          .add(mkTr(_("Directories"), -1, dirs))
          .add(mkTr(_("Directories in bad condition"), badDirs, -1)))
      ;
      Log.onReload();
    });
  }

  function updateFiles (read: Bool, up: Bool): Void {
    filesDiv
      .removeAll()
      .add(Q("div")
        .klass("head")
        .text(_("Files")))
    ;

    if (!read) {
      filesDiv
        .add(Q("div")
          .style("text-align: center")
          .add(Q("button")
            .text(_("Test"))
            .on(CLICK, e -> {
              updateDirs();
              updateFiles(true, false);
              update(false);
            })))
      ;
      return;
    }

    final body = Q("div")
      .style("text-align: center")
      .add(Ui.img("wait.gif"))
    ;
    filesDiv.add(body);

    Cts.client.send([
      "page" => Js.ws("summary"),
      "rq" => Js.ws("files")
    ], rp -> {
      final isBusy = rp["isBusy"].rb();
      final files = rp["files"].ri();
      final outdatedDirs = rp["outdatedDirs"].ri();
      final outdatedFiles = rp["outdatedFiles"].ri();

      body.removeAll();

      if (isBusy) {
        body
          .add(Q("table")
            .att("align", "center")
            .add(Q("tr")
              .add(Q("td")
                .klass("frame")
                .text(_("Server is busy.")))))
        ;
        return;
      }

      body
        .style("text-align: center")
        .add(Q("table")
          .att("align", "center")
          .klass("white")
          .add(mkTr(_("Files"), -1, files))
          .add(mkTr(_("Directories out of data"), outdatedDirs, -1))
          .add(mkTr(_("Files out of data"), outdatedFiles, -1)))
        .add(Q("div")
          .style("height:10px"))
        .add(Q("button")
          .text(_("Update"))
          .on(CLICK, ev -> {
            Q(null, ev.target).disabled(true);
            update(true);
          }))
      ;
      Log.onReload();
    });
  }

  function update (read: Bool): Void {
    bkDiv.removeAll();

    if (!read) {
      return;
    }

    bkDiv
      .style("text-align: center; padding-top: 10px")
      .add(Ui.img("wait.gif"));
    Cts.client.ssend([
      "page" => Js.ws("summary"),
      "rq" => Js.ws("update")
    ], rp -> {
      final tm = new haxe.Timer(1000);
      tm.run = function () {
        Cts.client.send([
          "page" => Js.ws("summary"),
          "rq" => Js.ws("isBusy")
        ], rp -> {
          if (!rp["isBusy"].rb()) {
            update(false);
            updateDirs();
            updateFiles(true, false);
            Log.onReload();
            tm.stop();
          }
        });
      }
    });
  }

  // Static --------------------------------------------------------------------

  static function mkTr(title: String, n1: Int, n2: Int): Domo {
    final tab = n1 < 0 ? "" : "&nbsp;&nbsp;&nbsp;&nbsp;";
    return Q("tr")
      .add(Q("td")
        .klass("entry")
        .html(tab + title))
      .add(Q("td")
        .klass("number2")
        .text(n1 >= 0 ? Std.string(n1): ""))
      .add(Q("td")
        .klass("number2")
        .text(n2 >= 0 ? Std.string(n2): ""))
    ;
  }


}

// Copyright 25-Aug-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.settings.servers;

using StringTools;
import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.It;
import dm.Js;
import dm.ModalBox;
import dm.ListSorter;
import I18n._;
import I18n._args;
import data.Server;
import data.Cts;

/// Modification of server configuration page.
class Configuration {
  final table = Q("table")
    .att("align", "center")
    .style("border-top: 1px solid rgb(110,130,150);" +
           "border-bottom: 1px solid rgb(110,130,150);" +
           "border-collapse: collapse;");

  final activateBt = Q("button");
  final resetBt = Q("button")
    .text(_("Reset"))
  ;
  final modifyBt = Q("button")
    .text(_("Modify"))
  ;

  final selWg = Q("input").att("type", "radio").att("name", "sel");
  final active = Q("input").att("type", "radio").att("name", "sel");
  final stopped = Q("input").att("type", "radio").att("name", "sel");

  final testDiv = Q("div");

  final cmdWget = Q("input").att("type", "radio").att("name", "cmd");
  final cmdPuppeteer = Q("input").att("type", "radio").att("name", "cmd");
  final url = Ui.field("dateSep").att("id", "url").style("width:600px");
  final regex = Q("textarea").att("rows", "3").att("cols", 80);

  final isoDate = Q("input").att("type", "checkbox");
  final dateSep = Ui.field("tableStart")
    .att("id", "dateSep")
    .style("width:10px");

  final isoNum = Q("input").att("type", "checkbox");

  final fieldsDiv = Q("div");

  final tableStart = fieldC("tableStart", "tableEnd");
  final tableEnd = fieldC("tableEnd", "rowStart");

  final rowStart = fieldC("rowStart", "rowEnd");
  final rowEnd = fieldC("rowEnd", "cSt0");

  final cells: Array<Array<Domo>> = [];

  final msgWait = Q("div");


  var wg: Domo;
  var serversPg: Servers;
  var isHistoric: Bool;
  var server: Server;

  var isActivated: Bool;
  var fieldsList: Array<String>;
  /// Constructor.
  ///   wg        : Container.
  ///   serversPg : Main page.
  ///   isHistoric: 'true' if historic configuration is edited.
  ///   server    : Server to edit.
  public function new (
    wg: Domo, serversPg: Servers, isHistoric: Bool, server: Server
  ) {
    this.wg = wg;
    this.serversPg = serversPg;
    this.isHistoric = isHistoric;
    this.server = server;

    isActivated = isHistoric
      ? server.historicConf != None
      : server.dailyConf != None
    ;
    fieldsList = [];

    final len = isHistoric ? 6 : 2;
    for (i in 0...len) {
      cells.push([
        Q("span"), fieldC("cSt" + i, "cEnd" + i),
        Q("span"), i == len - 1
          ? fieldC("cEnd" + i, "url")
          : fieldC("cEnd" + i, "cSt" + (i + 1))
      ]);
    }

    activateBt.on(CLICK, e -> activate());
    resetBt.on(CLICK, e -> reset());
    modifyBt.on(CLICK, e -> modify());


    view ();
  }

  // View ----------------------------------------------------------------------

  function view () {
    final sv = server;

    table
      .add(Q("tr")
        .add(Q("td")
          .style("width: 100px"))
        .add(Q("td")
          .style("width: 250px"))
        .add(Q("td")
          .style("width: 100px"))
        .add(Q("td")
          .style("width: 250px")))
      .add(Q("tr")
        .add(Q("td")
          .style("padding-top:4px;padding-bottom: 4px;" +
                 "border-bottom: 1px solid rgb(110,130,150);" +
                 "text-align:left")
          .add(activateBt))
        .add(Q("td")
          .style("padding-top:4px;padding-bottom: 4px;" +
                 "border-bottom: 1px solid rgb(110,130,150)"))
        .add(Q("td")
          .att("colspan", 2)
          .style("padding-top:4px;padding-bottom: 4px;" +
                 "border-bottom: 1px solid rgb(110,130,150);" +
                 "text-align:right")
          .add(resetBt)
          .add(Q("span")
            .text(" "))
          .add(modifyBt)))
    ;

    if (isActivated) {
      activateBt.text(_("Remove"));

      table
        .add(Q("tr")
          .add(Q("td")
            .att("colspan", 2)
            .style("text-align:left")
            .add(stopped)
            .add(Q("span")
              .html("&nbsp;"))
            .add(Q("span")
              .html(_("Stopped")))
            .add(Q("span")
              .html("&nbsp;&nbsp;&nbsp;"))
            .add(active)
            .add(Q("span")
              .html("&nbsp;"))
            .add(Q("span")
              .html(_("Active")))
            .add(Q("span")
              .html("&nbsp;&nbsp;&nbsp;"))
            .add(selWg)
            .add(Q("span")
              .html("&nbsp;"))
            .add(Q("span")
              .html(_("Selected"))))
          .add(Q("td")
            .style("text-align:right")
            .add(Ui.link(e -> test())
              .klass("link")
              .text(_("Test") + ": ")))
          .add(Q("td")
            .style("text-align:left")
            .add(testDiv)))
        .add(Q("tr")
          .add(Q("td")
            .att("colspan", 4)
            .style("text-align:left")
            .add(cmdWget)
            .add(Q("span")
              .html("&nbsp;"))
            .add(Q("span")
              .html(Cts.wget))
            .add(Q("span")
              .html("&nbsp;&nbsp;&nbsp;"))
            .add(cmdPuppeteer)
            .add(Q("span")
              .html("&nbsp;"))
            .add(Q("span")
              .html(Cts.puppeteer))))
        .add(Q("tr")
          .add(Q("td")
            .style("text-align:right")
            .text(_("URL") + ": "))
          .add(Q("td")
            .att("colspan", 3)
            .style("text-align:left")
            .add(url)))
        .add(Q("tr")
          .add(Q("td")
            .style("text-align:right")
            .text(_("RegEx") + ": "))
          .add(Q("td").att("colspan", 3)
            .style("text-align:left")
            .add(regex)))
      ;

      if (isHistoric) {
        table
          .add(Q("tr")
            .add(Q("td")
              .style("text-align:right")
              .add(isoDate))
            .add(Q("td")
              .style("text-align:left")
              .text(_("Is ISO date?")))
            .add(Q("td")
              .style("text-align:right")
              .add(dateSep))
            .add(Q("td")
              .style("text-align:left")
              .text(_("Date separator"))))
        ;
      }

      table
        .add(Q("tr")
          .add(Q("td")
            .style("text-align:right")
            .add(isoNum))
          .add(Q("td")
            .style("text-align:left")
            .text(_("Is ISO number?")))
          .add(Q("td"))
          .add(Q("td")))
        .add(Q("tr")
          .add(Q("td")
            .style("text-align:right")
            .text(_("Fields order") + ": "))
          .add(Q("td")
            .style("text-align:left")
            .add(fieldsDiv))
          .add(Q("td"))
          .add(Q("td")))
        .add(Q("tr")
          .add(Q("td")
            .style("text-align:right")
            .text(_("Table start") + ": "))
          .add(Q("td")
            .style("text-align:left")
            .add(tableStart))
          .add(Q("td")
            .style("text-align:right")
            .text(_("Table end") + ": "))
          .add(Q("td")
            .style("text-align:left")
            .add(tableEnd)))
        .add(Q("tr")
          .add(Q("td")
            .style("text-align:right")
            .text(_("Row start") + ": "))
          .add(Q("td")
            .style("text-align:left")
            .add(rowStart))
          .add(Q("td")
            .style("text-align:right")
            .text(_("Row end") + ": "))
          .add(Q("td")
            .style("text-align:left")
            .add(rowEnd)))
        .adds(cells.map(c ->
          Q("tr")
            .add(Q("td").style("text-align:right").add(c[0]))
            .add(Q("td").style("text-align:left").add(c[1]))
            .add(Q("td").style("text-align:right").add(c[2]))
            .add(Q("td").style("text-align:left").add(c[3]))
        ))
      ;

      final conf = isHistoric
        ? switch (server.historicConf) {
            case None: throw ("Configuration is None");
            case Some(cf): cf;
          }
        : switch (server.dailyConf) {
            case None: throw ("Configuration is None");
            case Some(cf): cf;
          }
      ;


      if (conf.sel == Cts.serverStopped) {
        stopped.checked(true);
      } else if (conf.sel == Cts.serverActive) {
        active.checked(true);
      } else {
        selWg.checked(true);
      }
      testDiv.removeAll().add(Ui.img("unknown"));

      if (conf.cmd == Cts.wget) cmdWget.checked(true);
      else cmdPuppeteer.checked(true);
      url.value(conf.url);
      regex.value(conf.regex);

      isoDate.checked(conf.isIsoDate);
      dateSep.value(conf.dateSeparator);

      isoNum.checked(conf.isIsoNumber);

      fieldsList = [];
      for (i in 0...conf.fieldsType.length) {
        fieldsList.push(conf.fieldsType.charAt(i));
      }
      setFieldsOrder();

      tableStart.value(conf.tableStart);
      tableEnd.value(conf.tableEnd);

      rowStart.value(conf.rowStart);
      rowEnd.value(conf.rowEnd);

      for (i in 0...conf.cellsStart.length) {
        cells[i][1].value(conf.cellsStart[i]);
        cells[i][3].value(conf.cellsEnd[i]);
      }

    } else {
      activateBt.text(_("Activate"));
      resetBt.disabled(true);
      modifyBt.disabled(true);
    }

    wg
      .removeAll()
      .style("text-align:center")
      .add(msgWait)
      .add(Q("div")
        .klass("head")
        .style("padding-bottom: 10px")
        .text(sv.name))
      .add(table)
    ;
  }

  function setWait (nickName: String) {
    msgWait.removeAll();

    if (nickName != "") {
      final box = new ModalBox(
        Q("div")
          .add(Q("div")
            .style("text-align:center")
            .add(Ui.img("wait2.gif")
              .klass("frame")))
          .add(Q("div")
            .style("text-align:center")
            .html(nickName)),
        false
      );
      msgWait.add(box.wg);
      box.show(true);
    }
  }

  // Control -------------------------------------------------------------------

  function setFieldsOrder () {
    final ls = new ListSorter<String>(
      () -> Ui.img("blank"),
      () -> Ui.img("go-previous"),
      () -> Ui.img("go-next"),
      fieldsList,
      ls -> {
        fieldsList = ls;
        setFieldsOrder();
      }
    );

    fieldsDiv
      .removeAll()
      .add(Q("table")
        .klass("white")
        .add(Q("tr")
          .adds(ls.ups.map(e -> Q("td").add(e))))
        .add(Q("tr")
          .adds(ls.downs.map(e -> Q("td").add(e))))
        .add(Q("tr")
          .adds(fieldsList.map(e -> Q("td")
            .style("border-top: 1px solid rgb(110,130,150)")
            .html(e)))))
    ;

    It.from(fieldsList).eachIx((e, i) -> {
      final n = fieldToLabel(isHistoric, e);
      cells[i][0].removeAll().html(n + " " + _("start") + ": ");
      cells[i][2].removeAll().html(n + " " + _("end") + ": ");
    });
  }

  function activate () {
    final sv = server;

    if (isActivated) {
      if (!Ui.confirm(_args(
        _("Remove %0 configuration?"),
        [isHistoric ? _("historic") : _("daily")]
      ))) {
        return;
      }
      if (isHistoric) {
        sv.historicConf = None;
      } else {
        sv.dailyConf = None;
      }
    } else if (isHistoric) {
      sv.historicConf = Some(new Sconf(
        Cts.wget, "", "", Cts.serverActive,
        true, "/", true,
        "DOCXNV", // Date, Open, Close, maX, miN, Volume
        "", "",
        "", "",
        ["", "", "", "", "", ""],
        ["", "", "", "", "", ""]
      ));
    } else {
      sv.dailyConf = Some(new Sconf(
        Cts.wget, "", "", Cts.serverActive,
        true, "/", true,
        "CQ", // Code, Quote
        "", "",
        "", "",
        ["", ""],
        ["", ""]
      ));
    }

    serversPg.modify(server);
  }

  function reset () {
    //eslint-disable-next-line
    new Configuration(
      wg, serversPg, isHistoric, server
    );
  }

  function modify () {
    final cmd = cmdWget.getChecked() ? Cts.wget : Cts.puppeteer;
    final url = cast(url.getValue(), String).trim();
    if (url == "") {
      Ui.alert(_("URL is missing"));
      return;
    }
    final regex = cast(regex.getValue(), String).trim();
    final sel = stopped.getChecked()
      ? Cts.serverStopped
      : active.getChecked() ? Cts.serverActive : Cts.serverSelected
    ;
    final isIsoDate = isoDate.getChecked();
    final dateSeparator = cast(dateSep.getValue(), String).trim();
    if (dateSeparator == "") {
      Ui.alert(_("Date separator is missing"));
      return;
    }
    final isIsoNumber = isoNum.getChecked();
    final fieldsType = fieldsList.join("");

    final tableStart = cast(tableStart.getValue(), String).trim();
    if (markError("Table start", tableStart)) return;
    final tableEnd = cast(tableEnd.getValue(), String).trim();
    if (markError("Table end", tableEnd)) return;

    final rowStart = cast(rowStart.getValue(), String).trim();
    if (markError("Row start", rowStart)) return;
    final rowEnd = cast(rowEnd.getValue(), String).trim();
    if (markError("Row end", rowEnd)) return;
    final cellsStart = [];
    final cellsEnd = [];
    for (i in 0...cells.length) {
      final c = cells[i];
      var label = fieldToLabel(isHistoric, fieldsList[i]) +
        " " + _("start");
      final st = cast(c[1].getValue(), String).trim();
      if (markError(label, st)) return;
      cellsStart.push(st);

      label = fieldToLabel(isHistoric, fieldsList[i]) +
        " " + _("end");
      final end = cast(c[3].getValue(), String).trim();
      if (markError(label, end)) return;
      cellsEnd.push(end);
    }

    final ocf = isHistoric
      ? server.historicConf
      : server.dailyConf
    ;

    switch (ocf) {
      case None: throw("Configuratio is None");
      case Some(cf):
        cf.cmd = cmd;
        cf.url = url;
        cf.regex = regex;
        cf.sel = sel;
        cf.isIsoDate = isIsoDate;
        cf.dateSeparator = dateSeparator;
        cf.isIsoNumber = isIsoNumber;
        cf.fieldsType = fieldsType;
        cf.tableStart = tableStart;
        cf.tableEnd = tableEnd;
        cf.rowStart = rowStart;
        cf.rowEnd = rowEnd;
        cf.cellsStart = cellsStart;
        cf.cellsEnd = cellsEnd;

        serversPg.modify(server);
    }
  }

  function test () {
    if (isHistoric) {
      var ok = true;
      It.from(server.codes).eachSync(
        (nkCode, fn) -> {
          switch (nkCode.code) {
            case None: setWait("???");
            case Some(code): setWait(code);
          }
          Cts.client.ssend([
            "module" => Js.ws("settings"),
            "source" => Js.ws("servers/configuration"),
            "rq" => Js.ws("historicTest"),
            "serverId" => Js.wi(server.id),
            "nickId" => Js.wi(nkCode.nickId)
          ], rp -> {
            fn(rp);
          });
        },
        rp -> {
          if (!rp["ok"].rb()) ok = false;
        },
        () -> {
          setWait("");
          testDiv
            .removeAll()
            .add(Ui.img(ok ? "well" : "error"))
          ;
        }
      );
    } else {
      testDiv
        .removeAll()
        .add(Ui.img("wait.gif"))
      ;
      Cts.client.ssend([
        "module" => Js.ws("settings"),
        "source" => Js.ws("servers/configuration"),
        "rq" => Js.ws("dailyTest"),
        "serverId" => Js.wi(server.id)
      ], rp -> {
        final ok = rp["ok"].rb();
        testDiv
          .removeAll()
          .add(Ui.img(ok ? "well" : "error"))
        ;
      });
    }
  }

  // Static --------------------------------------------------------------------

  static function fieldC (id: String, nextId: String): Domo {
    return Ui.field(nextId).att("id", id).style("width:240px");
  }

  function fieldToLabel (isHistoric: Bool, id: String): String {
    return id == "C" && isHistoric ? _("CloseN")
      : id == "C" ? _("Code")
        : id == "Q" ? _("CloseN")
          : id == "D" ? _("Date")
            : id == "O" ? _("Open")
              : id == "X" ? _("Max")
                : id == "N" ? _("Min")
                  : _("Volume")
    ;
  }

  // Returns 'true' if error.
  function markError (label: String, mark: String): Bool {
    if (mark == "") {
      Ui.alert(_args(_("%0 is missing"), [label]));
      return true;
    }
    final marks = mark.split("|");
    for (i in 0...marks.length) {
      final m = marks[i].trim();
      if (m == "") {
        Ui.alert(_args(_("Values of '%0' have blanks"), [label]));
        return true;
      }
    }
    return false;
  }

}

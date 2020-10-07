// Copyright 24-Aug-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.settings.servers;

using StringTools;
import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.It;
import data.Server;
import data.Nick;
import data.Cts;
import I18n._;

/// Modification of server codes page.
class Codes {
  var wg: Domo;
  var serversPg: Servers;
  var server: Server;
  var nicks: Array<Nick>;

  var codeFields: Array<Domo>;
  var testSpans: Array<Domo>;

  /// Constructor.
  ///   wg: Container.
  ///   serversPg: Main page.
  ///   sever: Server to modify.
  ///   nicks: Nicks list.
  public function new (
    wg: Domo, serversPg: Servers, server: Server, nicks: Array<Nick>
  ) {
    this.wg = wg;
    this.serversPg = serversPg;
    this.server = server;
    this.nicks = nicks;

    codeFields = [];
    testSpans = [];

    view();
  }

  function getCode (id: Int): String {
    return switch (It.from(server.codes).find(e -> e.nickId == id)) {
      case None: throw "Server code " + id + " is missing";
      case Some(e): switch (e.code) {
        case None: "";
        case Some(code): code;
      }
    }
  }

  // View ----------------------------------------------------------------------

  function view () {
    final sv = server;
    final nicks = nicks;

    final table = Q("table")
      .att("align", "center")
      .style("border-top: 1px solid rgb(110,130,150);" +
             "border-bottom: 1px solid rgb(110,130,150);" +
             "border-collapse: collapse;")
    ;

    final tds = [];
    final len = nicks.length;
    var i = 0;
    while (i < len) {
      final ix = i;
      final nk = nicks[i++];
      final nextNk = i < len ? nicks[i] : nicks[0];

      final field = Ui.field(Std.string(nextNk.id))
        .att("id", Std.string(nk.id))
        .setStyle("width", "125px")
        .value(this.getCode(nk.id));
      codeFields.push(field);

      final span = Q("span").add(Ui.img("unknown"));
      testSpans.push(span);

      tds.push(Q("td")
        .style("text-align:right;white-space: nowrap;")
        .add(Ui.link(e -> this.test(nk.id, ix))
          .klass("link")
          .text(nk.name))
        .add(Q("span")
          .text(": "))
        .add(field)
        .add(span)
      );
    }

    final cols = 4;
    final rowsLen = Math.ceil(len / cols);
    final rows = [];
    if (rowsLen == 0) {
      rows.push(Q("tr").add(Q("td").text(_("Without Nicks"))));
    } else {
      final tdWgs = [];
      for (i in 0...rowsLen) {
        final tdWg = [];
        for (j in 0...cols) {
          tdWg.push(i * cols + j < len ? Q("td") : null);
        }
        tdWgs.push(tdWg);
      }

      var ix = 0;
      for (cix in 0...cols) {
        for (rix in 0...rowsLen) {
          tdWgs[rix][cix] = tdWgs[rix][cix] == null
            ? Q("td").setStyle("border-left", "1px solid rgb(110,130,150)")
            : cix == 0
              ? tds[ix++]
              : tds[ix++].setStyle("border-left", "1px solid rgb(110,130,150)")
          ;
        }
      }

      for (i in 0...rowsLen) {
        final rowTds = [];
        for (j in 0...cols) {
          rowTds.push(tdWgs[i][j]);
        }
        rows.push(Q("tr").adds(rowTds));
      }
    }

    table
      .add(Q("tr")
        .add(Q("td").att("colspan", cols)
          .style("padding-top:4px;padding-bottom: 4px;" +
                 "border-bottom: 1px solid rgb(110,130,150)")
          .add(Q("button")
            .text(_("Reset"))
            .on(CLICK, e -> reset()))
          .add(Q("span")
            .text(" "))
          .add(Q("button")
            .text(_("Modify"))
            .on(CLICK, e -> modify()))))
      .adds(rows)
    ;

    wg
      .removeAll()
      .style("text-align:center")
      .add(Q("div")
        .klass("head")
        .style("padding-bottom: 10px")
        .text(sv.name))
      .add(table)
    ;
  }

  // Control -------------------------------------------------------------------

  function reset () {
    new Codes(wg, serversPg, server, nicks);
  }

  function modify () {
    for (c in codeFields) {
      for (sc in server.codes) {
        if (sc.nickId == Std.parseInt(c.getAtt("id"))) {
          sc.code = Some(cast(c.getValue(), String).trim());
          break;
        }
      }
    };

    serversPg.modify(server);
  }

  /**
    @private
    @param {number} nickId
    @param {number} index
    @return !Promise<void>
  **/
  function test (nickId, index) {
    testSpans[index]
      .removeAll()
      .add(Ui.img("wait.gif"));

    Cts.client.ssend([
      "module" => Js.ws("settings"),
      "source" => Js.ws("servers/code"),
      "rq" => Js.ws("historicTest"),
      "serverId" => Js.wi(server.id),
      "nickId" => Js.wi(nickId)
    ], rp -> {
      final ok = rp["ok"].rb();
      testSpans[index]
        .removeAll()
        .add(Ui.img(ok ? "well" : "error"))
      ;
    });
  }
}

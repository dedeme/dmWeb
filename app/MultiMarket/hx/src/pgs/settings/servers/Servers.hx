// Copyright 24-Aug-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.settings.servers;

using StringTools;
import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.It;
import dm.Menu;
import wgs.Msg;
import data.Cts;
import data.Server;
import data.Nick;
import I18n._;
import I18n._args;
import pgs.settings.servers.wgs.Wserver;

/// Main servers page.
class Servers {
  var wg: Domo;
  var servers: Array<Server>;
  var nicks: Array<Nick>;

  var withVolume: Bool;
  var selectedServer: Int;
  var selectedTab: String;

  final newServerWg = Ui.field("newBt").style("width:100px");

  function new (wg: Domo, servers: Array<Server>, nicks:Array<Nick>) {
    this.wg = wg;
    this.servers = servers;
    this.nicks = nicks;

    withVolume = true;
    selectedServer = -1;
    selectedTab = "names";

    view();
  }

  // View ----------------------------------------------------------------------

  function view () {
    final serverListWg = Q("div");
    final body = Q("div");
    final sel = selectedServer;

    if (servers.length == 0) {
      serverListWg
        .removeAll()
        .add(Q("div")
          .klass("frame")
          .style("text-align:center")
          .html(_("Without<br>Servers")))
      ;

      body
        .removeAll()
        .add(Q("table")
          .klass("frame")
          .att("align", "center")
          .add(Q("tr")
            .add(Q("td")
              .text(_("Without Data")))))
      ;
    } else {
      var i = 0;
      serverListWg
        .removeAll()
        .add(Q("table")
          .klass("submenu")
          .adds(servers.map(sv -> {
            final td = Q("td");
            new Wserver(
              td,
              this,
              sv,
              i == selectedServer
            );
            i++;
            return Q("tr").add(td);
          })))
      ;

      if (sel < 0 || sel > servers.length) {
        body
          .removeAll()
          .add(Q("table")
            .klass("frame")
            .att("align", "center")
            .add(Q("tr")
              .add(Q("td")
                .text(_("No server selected")))))
        ;
      } else {
        final server = servers[sel];
        final body2 = Q("div");

        final lopts = [
          Menu.toption("names", _("Names"), () -> setTab("names")),
          Menu.separator(),
          Menu.toption("daily", _("Daily"), () -> setTab("daily")),
          Menu.separator(),
          Menu.toption(
            "historic", _("Historic"), () -> setTab("historic")
          ),
          Menu.separator(),
          Menu.toption("codes", _("Codes"), () -> setTab("codes"))
        ];
        final ropts = [
          new MenuEntry(None, Q("span").text(server.name))
        ];
        final menu = new Menu(lopts, ropts, selectedTab);

        switch (selectedTab) {
        case "daily":
          new Configuration(body2, this, false, server);
        case "historic":
          new Configuration(body2, this, true, server);
        case "codes":
          new Codes(body2, this, server, nicks);
        default:
          new Names(body2, this, server);
        }

        body
          .removeAll()
          .add(Q("table")
            .klass("main")
            .add(Q("tr")
              .add(Q("td")
                .add(menu.wg)))
            .add(Q("tr")
              .add(Q("td")
                .add(body2))))
        ;
      }
    }

    wg
      .removeAll()
      .add(Q("table")
        .klass("main")
        .add(Q("tr")
          .add(Q("td")
            .style("vertical-align:top;width:5px")
            .add(Q("table")
              .klass("home")
              .add(Q("tr")
                .add(Q("td")
                  .add(newServerWg)))
              .add(Q("tr")
                .add(Q("td")
                  .style("text-align:center")
                  .add(Q("button")
                    .att("id", "newBt")
                    .text(_("New"))
                    .on(CLICK, e -> newServer()))))
              .add(Q("tr")
                .add(Q("td")
                  .add(Q("hr"))))
              .add(Q("tr")
                .add(Q("td")
                  .add(serverListWg)))))
          .add(Q("td")
            .style("vertical-align:top")
            .add(body))))
    ;
  }

  // Control -------------------------------------------------------------------

  function newServer () {
    final server = cast(newServerWg.getValue(), String).trim();
    if (server == "") {
      Msg.error(_("Server name is empty"));
      return;
    }
    if (It.from(servers).some(e -> e.shortName == server)) {
      Msg.error(_("Server name is duplicated"));
      return;
    }
   Cts.client.ssend([
      "module" => Js.ws("settings"),
      "source" => Js.ws("servers"),
      "rq" => Js.ws("new"),
      "server" => Js.ws(server)
    ], rp -> {
      if (!rp["ok"].rb()) {
        Msg.error(Cts.failMsg);
      }
      Servers.mk(wg);
    });
  }

  public function del (server: Server) {
    if (!Ui.confirm(_args(_("Delete '%0'?"), [server.shortName]))) {
      return;
    }
    Cts.client.ssend([
      "module" => Js.ws("settings"),
      "source" => Js.ws("servers"),
      "rq" => Js.ws("del"),
      "id" => Js.wi(server.id)
    ], rp -> {
      Servers.mk(wg);
    });
  }

  public function sel (server: Server) {
    final svs = servers;
    final id = server.id;
    var ix = -1;
    for (i in 0...svs.length) {
      if (svs[i].id == id) {
        ix = i;
        break;
      }
    }
    selectedServer = ix;
    view();
  }

  public function modify (server: Server) {
    Cts.client.ssend([
      "module" => Js.ws("settings"),
      "source" => Js.ws("servers"),
      "rq" => Js.ws("modify"),
      "server" => server.toJs()
    ], rp -> {
      if (rp["ok"].rb()) {
        view();
        Msg.ok(Cts.okMsg);
      } else {
        Msg.error(Cts.failMsg);
        Servers.mk(wg);
      }
    });
  }

  function setTab (tabId: String) {
    selectedTab = tabId;
    view();
  }


  // Static --------------------------------------------------------------------

  /// Constructor.
  ///   wg: Container.
  public static function mk (wg: Domo) {
    Cts.client.ssend([
      "module" => Js.ws("settings"),
      "source" => Js.ws("servers"),
      "rq" => Js.ws("idata")
    ], rp -> {
      final servers = rp["servers"].ra().map(e -> Server.fromJs(e));
      servers.sort((e1, e2) -> e1.shortName > e2.shortName ? 1 : -1);
      final nicks = rp["nicks"].ra().map(e -> Nick.fromJs(e));
      nicks.sort((e1, e2) -> e1.name > e2.name ? 1 : -1);
      new Servers(wg, servers, nicks);
    });
  }
}


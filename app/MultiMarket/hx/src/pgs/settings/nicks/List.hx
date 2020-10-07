// Copyright 20-Aug-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.settings.nicks;

using StringTools;
import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.It;
import dm.Js;
import dm.Menu;
import dm.ModalBox;
import data.Cts;
import data.Nick;
import wgs.Msg;
import pgs.settings.nicks.wgs.Wnick;
import I18n._;

/// Nicks list.
class List {
  var inputDiv: Domo;
  var menuDiv: Domo;
  var bodyDiv: Domo;
  var nicksPg: Nicks;
  var model: Int;
  var nicks: Array<Nick>;
  var volumes: Map<String, Float>;
  var withVolume: Bool;

  final msgWait = Q("div");
  final newNick = Ui.field("newBt")
    .style("width:100px")
  ;

  public function new (
    inputDiv: Domo,
    menuDiv: Domo,
    bodyDiv: Domo,
    nicksPg: Nicks,
    model: Int,
    nicks: Array<Nick>,
    volumes: Map<String, Float>,
    withVolume: Bool
  ) {
    this.inputDiv = inputDiv;
    this.menuDiv = menuDiv;
    this.bodyDiv = bodyDiv;
    this.nicksPg = nicksPg;
    this.model = model;
    if (withVolume) {
      nicks.sort((e1, e2) -> volumes[e1.name] < volumes[e2.name] ? 1 : -1);
    } else {
      nicks.sort((e1, e2) -> e1.name > e2.name ? 1 : -1);
    }
    this.nicks = nicks;
    this.volumes = volumes;
    this.withVolume = withVolume;

    view();
  }

  // View ----------------------------------------------------------------------

  function view () {
    inputDiv
      .removeAll()
      .add(Q("table")
        .klass("home")
        .add(Q("tr")
          .add(Q("td")
            .add(newNick)))
        .add(Q("tr")
          .add(Q("td")
            .style("text-align:center")
            .add(Q("button")
              .att("id", "newBt")
              .text(_("New"))
              .on(CLICK, e -> {
                nicksPg.addNick(cast(newNick.getValue(), String).trim());
              })))))
    ;

    final lopts = [];
    final ropts = [
      Menu.toption("download", _("Download"), () -> download()),
      Menu.separator(),
      Menu.toption("test", _("Test"), () -> test())
    ];
    final menu = new Menu(lopts, ropts, "");
    menuDiv
      .removeAll()
      .add(menu.wg)
    ;

    if (nicks.length == 0) {
      bodyDiv
        .removeAll()
        .add(Q("table")
          .att("align", "center")
          .klass("frame")
          .add(Q("tr")
            .add(Q("td")
              .text(_("Without Nicks")))))
      ;
    } else {
      final cols = 6;
      final rows = Math.ceil(nicks.length / cols);
      final list = [];

      for (c in 0...cols) {
        for (r in 0...rows) {
          list[c + r * cols] = nicks[r + c * rows];
        }
      }

      final tb = Q("table")
        .att("align", "center")
        .klass("frame")
        .style("border-collapse : collapse;")
      ;
      var n = 0;
      var tr = Q("tr");
      for (nk in list) {
        if (n > 0 && n % cols == 0) {
          tb.add(tr);
          tr = Q("tr");
        }
        final wg = Q("div");
        if (nk != null) {
          Wnick.mk(
            wg,
            nicksPg,
            nk,
            nk.id == model,
            volumes[nk.name]
          );
        }
        tr.add(Q("td")
          .style("width:100px;text-align:center;border-right: solid 1px;")
          .add(wg)
        );
        ++n;
      }
      tb.add(tr);

      bodyDiv
        .removeAll()
        .add(msgWait)
        .add(tb)
      ;
    }
  }

  /** @private */
  function setWait (nickName: String) {
    msgWait.removeAll();

    if (nickName != "") {
      final box = new ModalBox(
        Q("div")
          .add(Q("div")
            .style("text-align:center")
            .add(Ui.img("wait2.gif").klass("frame")))
          .add(Q("div").style("text-align:center").html(nickName)),
        false
      );
      msgWait.add(box.wg);
      box.show(true);
    }
  }

  // Control -------------------------------------------------------------------

  function download () {
    var error = false;
    var warning = false;
    for (i in 0...nicks.length) {
      if (nicks[i].id == model) {
        final tmp = nicks[0];
        nicks[0] = nicks[i];
        nicks[i] = tmp;
        break;
      }
    }

    It.from(nicks).eachSync(
      (nk, fn) -> {
        setWait(nk.name);
        Cts.client.ssend([
          "module" => Js.ws("settings"),
          "source" => Js.ws("nicks/list"),
          "rq" => Js.ws("download"),
          "nickId" => Js.wi(nk.id)
        ], rp -> {
          fn(rp);
        });
      },
      rp -> {
        final rps = rp["result"].rs();
        if (rps == "error") {
          error = true;
        } else if (rps == "warning") {
          warning = true;
        }
      },
      () -> {
        setWait("");
        if (error) {
          if (warning) {
            Msg.error(_("Errors and warnings found."));
          } else {
            Msg.error(_("Errors found."));
          }
        } else if (warning) {
          Msg.info(_("Warning found."));
        } else {
          Msg.ok(_("Download ok."));
        }
      }
    );
  }

  function test () {
    var error = false;
    var warning = false;
    It.from(nicks).eachSync(
      (nk, fn) -> {
        setWait(nk.name);
        Cts.client.ssend([
          "module" => Js.ws("settings"),
          "source" => Js.ws("nicks/list"),
          "rq" => Js.ws("test"),
          "nickName" => Js.ws(nk.name)
        ], rp -> {
          fn(rp);
        });
      },
      rp -> {
        final rps = rp["result"].rs();
        if (rps == "error") {
          error = true;
        } else if (rps == "warning") {
          warning = true;
        }
      },
      () -> {
        setWait("");
        if (error) {
          if (warning) {
            Msg.error(_("Errors and warnings found."));
          } else {
            Msg.error(_("Errors found."));
          }
        } else if (warning) {
          Msg.info(_("Warning found."));
        } else {
          Msg.ok(_("Test ok."));
        }
      }
    );
  }

}

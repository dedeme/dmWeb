// Copyright 20-Aug-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.settings.nicks;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.It;
import dm.Menu;
import dm.Opt;
import data.Cts;
import data.Nick;
import wgs.Msg;
import I18n._;
import I18n._args;

/// Nicks page.
class Nicks {
  var wg: Domo;
  var model: Int;
  var nicks: Array<Nick>;
  var volumes: Map<String, Float>;
  var withVolume = true;
  var selectedNick = None;

  function new (
    wg: Domo, model: Int, nicks: Array<Nick>, volumes: Map<String, Float>
  ) {
    this.wg = wg;
    this.model = model;
    this.nicks = nicks;
    this.volumes = volumes;

    view();
  }

  // View ----------------------------------------------------------------------

  function view () {
    final lopts = [
      new MenuEntry(
        None, Q("span").text(mkStatistics(nicks))
      )
    ];
    final ropts = [
      Menu.toption("volume", _("Volume"), () -> list(true)),
      Menu.separator(),
      Menu.toption("list", _("List"), () -> list(false))
    ];
    final menuSelected = switch(selectedNick) {
      case None: withVolume ? "volume" : "list";
      default: "";
    }
    final menu = new Menu(lopts, ropts, menuSelected);

    final inputDiv = Q("div");
    final menuDiv = Q("div");
    final bodyDiv = Q("div");

    switch (selectedNick) {
      case Some(nick):
        var nickModel = nicks[0];
        for (e in nicks) if (e.id == model) { nickModel = e; break; }
        Editor.mk(
          inputDiv, menuDiv, bodyDiv, nicks, nick, nickModel
        );
      case None:
        new List(
          inputDiv,
          menuDiv,
          bodyDiv,
          this,
          model,
          nicks,
          volumes,
          withVolume
        );
    }

    wg
      .removeAll()
      .add(Q("table")
        .klass("main")
        .add(Q("tr")
          .add(Q("td")
            .style("width:5px")
            .att("rowspan", 2)
            .add(inputDiv))
          .add(menu.wg))
        .add(Q("tr")
          .add(Q("td")
            .add(menuDiv)))
        .add(Q("tr")
          .add(Q("td")
            .att("colspan", 2)
            .add(bodyDiv))))
    ;

  }

  // Control -------------------------------------------------------------------

  /// Add a new nick.
  public function addNick (nickName: String) {
    if (nickName == "") {
      Msg.error(_("Nick name is missing"), () -> {});
      return;
    }
    if (It.from(nicks).some(e -> e.name == nickName)) {
      Msg.error(_args(_("Nick '%0' is duplicated"), [nickName]), () -> {});
      return;
    }

    Cts.client.ssend([
      "module" => Js.ws("settings"),
      "source" => Js.ws("nicks"),
      "rq" => Js.ws("add"),
      "nickName" => Js.ws(nickName)
    ], rp -> {
      if (!rp["ok"].rb()) {
        Msg.error(Cts.failMsg, () -> {});
      }
      Nicks.mk(wg);
    });
  }

  public function setModel (nick: Nick) {
    Cts.client.ssend([
      "module" => Js.ws("settings"),
      "source" => Js.ws("nicks"),
      "rq" => Js.ws("setModel"),
      "id" => Js.wi(nick.id)
    ], rp -> {
      Nicks.mk(wg);
    });
  }

  public function setIsSel (nick: Nick) {
    Cts.client.ssend([
      "module" => Js.ws("settings"),
      "source" => Js.ws("nicks"),
      "rq" => Js.ws("setIsSel"),
      "id" => Js.wi(nick.id),
      "value" => Js.wb(!nick.isSel)
    ], rp -> {
      Nicks.mk(wg);
    });
}

  public function del (nick: Nick) {
    if (!Ui.confirm(_args(_("Delete '%0'?"), [nick.name]))) {
      return;
    }
    Cts.client.ssend([
      "module" => Js.ws("settings"),
      "source" => Js.ws("nicks"),
      "rq" => Js.ws("del"),
      "id" => Js.wi(nick.id)
    ], rp -> {
      Nicks.mk(wg);
    });
  }

  public function selectNick (nick: Nick) {
    selectedNick = Some(nick);
    view();
  }

  function list (withVolume: Bool) {
    this.withVolume = withVolume;
    selectedNick = None;
    view();
  }


  // Static --------------------------------------------------------------------

  static function mkStatistics (list: Array<Nick>): String {
    final total = list.length;
    var sel = 0;
    for (nk in list) if (nk.isSel) ++sel;
    return _args(
      _("Total: %0. Selected: %1."),
      [Std.string(total), Std.string(sel)
    ]);
  }

  public static function mk (wg: Domo) {
    Cts.client.ssend([
      "module" => Js.ws("settings"),
      "source" => Js.ws("nicks"),
      "rq" => Js.ws("idata")
    ], rp -> {
      final model = rp["model"].ri();
      final nicks = rp["nicks"].ra().map(e -> Nick.fromJs(e));
      final volumes = rp["volumes"].rMap(e -> e.rf());

      new Nicks(wg, model, nicks, volumes);
    });
  }

}

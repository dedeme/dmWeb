// Copyright 16-Aug-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package wgs;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.Menu;
import I18n._;
import I18n._args;
import data.Cts;

/// Double menu.
class Dmenu {
  var wg: Domo;
  var selected: String;
  var upDiv: Domo;
  var upMenu: Menu;
  var downDiv: Domo;
  var hidden: Bool;

  /// Constructor:
  ///   wg: Container.
  ///   selected: Selected option.
  public function new (wg: Domo, selected: String) {
    this.wg = wg;
    this.selected = selected;
    upDiv = Q("div").style("padding:0px");
    upMenu = mkUpMenu();
    downDiv = Q("div");
    hidden = false;
    view();
  }

  // View ----------------------------------------------------------------------

  function view () {
    wg
      .removeAll()
      .add(upDiv
        .removeAll()
        .add(upMenu.wg))
      .add(downDiv)
    ;
  }

  function mkUpMenu (): Menu {
    final lopts = [
      Menu.tlink("home", _("Home")),
      Menu.separator2(),
      Menu.tlink("fleas", _("Fleas")),
      Menu.separator(),
      Menu.tlink("performance", _("Performance")),
      Menu.separator(),
      Menu.tlink("ranking", _("Ranking")),
      Menu.separator(),
      Menu.tlink("ranges+", _("Ranges +")),
      Menu.separator2(),
      Menu.tlink("acc", _("Accounting")),
      Menu.separator2(),
      Menu.tlink("daily", _("Daily Quotes"))
    ];

    final ropts = [
      Menu.tlink("settings", _("Annotations & Settings")),
      Menu.separator(),
      Menu.close(() -> bye())
    ];

    return new Menu(lopts, ropts, selected);
  }

  /// Button to hidden-show up menu.
  public function hiddingButton (): MenuEntry {
    return new MenuEntry(
      None,
      Ui.link(e -> change())
        .add(Ui.img("menu").style("vertical-align:middle"))
    );
  }

  // Control -------------------------------------------------------------------

  function bye () {
    if (!Ui.confirm(_("Application exit?"))) {
      return;
    }
    Cts.client.send([
      "module" => Js.ws("wgs"),
      "source" => Js.ws("dmenu"),
      "rq" => Js.ws("bye"),
      "sessionId" => Js.ws(Cts.client.sessionId())
    ], rp -> {
      final wg = Q("div");
      MsgPage.mk(
        wg, Cts.appName, _args(_("Logout-message"), [Cts.appName])
      );
      Q("@body")
        .removeAll()
        .add(wg)
      ;
    });
  }

  function change () {
    hidden = !hidden;
    upDiv.removeAll();
    if (!hidden) upDiv.add(upMenu.wg);
  }

  /// Sets down menu.
  public function setDownMenu (menu: Menu) {
    downDiv
      .removeAll()
      .add(menu.wg)
    ;
    hidden = true;
    upDiv
      .removeAll()
    ;
  }

}

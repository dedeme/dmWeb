// Copyright 21-Oct-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.settings.servers.wgs;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import data.Cts;
import data.Server;
import I18n._;

/// Wserver Widget
class Wserver {
  var wg: Domo;
  var servers: Servers;
  var server: Server;
  var isSelected: Bool;

  /// Constructor.
  ///   wg: Container.
  ///   servers: Main page.
  ///   server: Server to show.
  ///   isSelected: "true" if server is selected for showing.
  public function new (
    wg: Domo, servers: Servers, server: Server, isSelected: Bool
  ) {
    this.wg = wg;
    this.servers = servers;
    this.server = server;
    this.isSelected = isSelected;

    view();
  }

  // View ----------------------------------------------------------------------

  function view () {
    final dailyInfo = switch (server.dailyConf) {
      case None: emptyBt(_("Daily configuration deactivated"));
      case Some(cf): cf.sel == Cts.serverSelected
        ? img("star", _("Daily configuration selected"))
        : cf.sel == Cts.serverActive
          ? img("flag1", _("Daily configuration activated"))
          : img("stopped", _("Daily configuration stopped"))
        ;
    }

    final historicInfo = switch (server.historicConf) {
      case None: emptyBt(_("Historic configuration deactivated"));
      case Some(cf): cf.sel == Cts.serverSelected
        ? img("star", _("Historic configuration selected"))
        : cf.sel == Cts.serverActive
          ? img("flag1", _("Historic configuration activated"))
          : img("stopped", _("Historic configuration stopped"))
        ;
    }

    wg
      .removeAll()
      .add(Q("table")
        .add(Q("tr")
          .add(Q("td")
            .add(Ui.link(e -> servers.del(server))
              .add(img("delete", _("Delete")))))
          .add(Q("td").add(dailyInfo))
          .add(Q("td").add(historicInfo))
          .add(Q("td").add(isSelected
            ? Q("span")
              .html("<b>" + server.shortName + "</b>")
            : Ui.link(e -> servers.sel(server))
              .klass("link")
              .text(server.shortName)))))
    ;
  }

  // Static --------------------------------------------------------------------

  static function img (id: String, title: String): Domo {
    return Ui.img(id).att("title", title);
  }

  static function emptyBt (title: String): Domo {
    return Q("div")
      .style("padding:5px;" +
             "border: 1px solid #002040;border-radius: 6px;" +
             "background: #d0ddde;")
      .att("title", title)
    ;
  }
}

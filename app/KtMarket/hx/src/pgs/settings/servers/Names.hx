// Copyright 21-Oct-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.settings.servers;

using StringTools;
import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import data.Server;
import I18n._;

/// Page to set server names.
class Names {
  var wg: Domo;
  var serversPg: Servers;
  var server: Server;

  var shortNameField: Domo;
  var nameField: Domo;

  /// Constructor.
  ///   wg: Container.
  ///   serversPg: Main page.
  ///   sever: Server to modify.
  public function new (wg: Domo, serversPg: Servers, server: Server) {
    this.wg = wg;
    this.serversPg = serversPg;
    this.server = server;

    shortNameField = Ui.field("nameField")
      .value(server.shortName)
    ;
    nameField = Ui.field("btAccept")
      .att("id", "nameField")
      .value(server.name)
    ;

    view();
  }

  // View ----------------------------------------------------------------------

  function view () {
    final sv = server;

    wg
      .removeAll()
      .style("text-align:center")
      .add(Q("div")
        .klass("head")
        .style("padding-bottom: 10px")
        .text(sv.name))
      .add(Q("table")
        .att("align", "center")
        .style("border-top: 1px solid rgb(110,130,150);" +
               "border-bottom: 1px solid rgb(110,130,150)")
        .add(Q("tr")
          .add(Q("td")
            .style("text-align:right")
            .text(_("Short Name") + ":"))
          .add(Q("td")
            .add(shortNameField)))
        .add(Q("tr")
          .add(Q("td")
            .style("text-align:right")
            .text(_("Name") + ":"))
          .add(Q("td")
            .add(nameField)))
        .add(Q("tr")
          .add(Q("td")
            .att("colspan", 2)
            .style("padding-top:4px")
            .add(Q("button").text(_("Reset"))
              .on(CLICK, e -> this.reset()))
            .add(Q("span")
              .text(" "))
            .add(Q("button")
              .text(_("Modify"))
              .on(CLICK, e -> this.modify())))))
    ;
  }

  // Control -------------------------------------------------------------------

  function reset () {
    new Names(wg, serversPg, server); //eslint-disable-line
  }

  function modify () {
    final shortName = cast(shortNameField.getValue(), String).trim();
    final name = cast(nameField.getValue(), String).trim();

    if (shortName == "") {
      Ui.alert(_("Short Name is missing"));
      return;
    }

    if (name == "") {
      Ui.alert(_("Name is missing"));
      return;
    }

    server.shortName = shortName;
    server.name = name;
    serversPg.modify(server);
  }

}

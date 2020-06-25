// Copyright 20-Jun-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import dm.Domo;
import dm.Ui.Q;
import dm.Ui;
import dm.It;
import dm.Js;
import I18n._;
import data.Settings;

/// Index page.
class Index {

  /// Constructor.
  ///   wg  : Container.
  ///   sett: Settings data.
  ///   lib : Library name.
  public static function mk (wg: Domo, sett: Settings, lib: String): Void {

    // entries is [id (with tabulation), link, doc (can be "")]
    // If entry is a directory link and doc are "".
    final view = (entries: Array<Array<String>>) -> {
      Q("@title").text('HxDoc-${lib}');
      if (entries.length == 0) {
        wg
          .removeAll()
          .add(Q("table")
            .att("align", "center")
            .add(Q("tr")
              .add(Q("td")
                .klass("frame")
                .text(_("Without data")))))
        ;
        return;
      } else {
        wg
          .removeAll()
          .add(Q("table")
            .klass("frame")
            .att("width", "100%")
            .adds(
              entries.map(e ->
                Q("tr")
                  .add(Q("td")
                    .style("width:5px;white-space:nowrap")
                    .add(e[1] == ""
                      ? Q("span")
                        .html('<b>${e[0]}</b>')
                      : Q("span")
                        .klass("link")
                        .html('<a href="?${lib}@${e[1]}">${e[0]}</a>')))
                  .add(Q("td")
                    .style("width: 5px"))
                  .add(Q("td")
                    .html(e[2])))))
        ;
      }
    }

    switch (It.from(sett.paths).find(p -> p.lib == lib)) {
    case Some(p):
      Cts.client.send([
        "source" => Js.ws("Index"),
        "path" => Js.ws(p.path)
      ], rp -> {
        view(rp["entries"].rArray(e -> e.rArray(f -> f.rs())));
      });
    case None:
      view([]);
    }

  }

}

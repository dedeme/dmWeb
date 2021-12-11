// Copyright 20-Jun-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs;

import dm.Domo;
import dm.Ui.Q;
import dm.Ui;
import dm.It;
import dm.Js;
import I18n._;
import cm.data.Paths;

/// Index page.
class Index {
  final wg: Domo;
  final lib: String;
  final entries: Array<Array<String>>;

  // entries is [id (with tabulation), link, doc (can be "")]
  // If entry is a directory link and doc are "".
  function new (wg: Domo, lib: String, entries: Array<Array<String>>) {
    this.wg = wg;
    this.lib = lib;
    this.entries = entries;

    view();
  }

  // View ----------------------------------------------------------------------

  function view (): Void {
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

  // Static --------------------------------------------------------------------

  /// Constructor.
  ///   wg  : Container.
  ///   paths: Paths data.
  ///   lib : Library name.
  public static function mk (wg: Domo, paths: Paths, lib: String): Void {
    switch (It.from(paths.list).find(p -> p.lib == lib)) {
    case Some(p):
      Cts.client.send([
        "source" => Js.ws("Index"),
        "path" => Js.ws(p.path)
      ], rp -> {
        final entries = rp["entries"].rArray(e -> e.rArray(f -> f.rs()));
        new Index(wg, lib, entries);
      });
    case None:
      new Index(wg, lib, []);
    }
  }
}

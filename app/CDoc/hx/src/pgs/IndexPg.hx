// Copyright 10-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import data.IndexTree;
import I18n._;
import I18n._args;

class IndexPg {
  final wg: Domo;
  final module: String;
  final tree: IndexTree;
  final linkPrefix: String;

  function new (wg: Domo, module: String, tree: IndexTree) {
    this.wg = wg;
    this.module = module;
    this.tree = tree;

    linkPrefix = "?" + module + "@";
  }

  // View ----------------------------------------------------------------------

  function show(): Void {
    Q("@title").text(Cts.appName + " - " + module);

    final trs: Array<Domo> = [];
    add(trs, tree.trees, "", 0);

    wg
      .removeAll()
      .add(Q("div")
        .klass("frame")
        .add(Q("table")
          .klass("main")
          .adds(trs)))
    ;
  }

  function add (
    trs: Array<Domo>, trees: Array<IndexTree>, path: String, space: Int
  ) {
    if (path != "") path = path + "/";

    trees.sort((t1, t2) ->
      switch (t1.doc) {
        case Some(d1): switch (t2.doc) {
          case Some(d2): t1.id.toUpperCase() > t2.id.toUpperCase() ? 1 : -1;
          case None: -1;
        }
        case None: switch (t2.doc) {
          case Some(d2): 1;
          case None: t1.id.toUpperCase() > t2.id.toUpperCase() ? 1 : -1;
        }
      }
    );

    for (e in trees) {
      switch (e.doc) {
        case Some(d):
          trs.push(Q("tr")
            .add(Q("td")
              .style('width:10px;padding-left:${space}px')
              .html('<a href="${linkPrefix}${path}${e.id}">${e.id}</a>'))
            .add(Q("td")
              .style("padding-left:10px")
              .text(d))
          );
        case None:
          trs.push(Q("tr")
            .add(Q("td")
              .style('padding-left:${space}px')
              .html('<b>${e.id}</b>'))
            .add(Q("td"))
          );
          add(trs, e.trees, path + e.id, space + 20);
      }
    }
  }

  // Static --------------------------------------------------------------------

  public static function mk (wg: Domo, module: String): Void {
    Cts.client.send([
      "source" => Js.ws("IndexPg"),
      "rq" => Js.ws("index"),
      "module" => Js.ws(module)
    ], rp -> {
      if (rp["index"].isNull()) {
        new MsgPg(wg, _("Library path not found o not valid.")).show();
        return;
      }
      final tree = IndexTree.fromJs(rp["index"]);
      new IndexPg(wg, module, tree).show();
    });
  }

}

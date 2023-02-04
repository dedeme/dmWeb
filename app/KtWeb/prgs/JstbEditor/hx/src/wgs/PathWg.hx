// Copyright 24-Jan-2023 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Lateral path in table.

package wgs;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import data.Tpath;
import data.Field;
import I18n._;

class PathWg {
  final wg: Domo;
  final tpath: Tpath;
  final fn: Tpath -> Void;

  public function new (wg: Domo, tpath: Tpath, fn: Tpath -> Void) {
    this.wg = wg;
    this.tpath = tpath;
    this.fn = fn;
  }

  // View ----------------------------------------------------------------------

  public function show () {
    final trs = [
      Q("tr")
        .add(Q("td")
          .style("text-align:left")
          .add(Ui.link(() -> go(new Field([])))
            .klass("link")
            .text(_("Root")))),
      Q("tr").add(Q("td").add(Q("hr")))
    ];

    final fieldPath = tpath.field.fieldPath;
    for (i in 0...fieldPath.length) {
      final e = fieldPath[i];
      trs.push(
        Q("tr")
          .add(Q("td")
            .style("text-align:left")
            .add(Ui.link(() -> go(new Field(fieldPath.slice(0, i+1))))
              .klass("link")
              .text(e.isMap ? e.key : ""+e.ix)))
      );
    }

    wg
      .removeAll()
      .klass("frame")
      .add(Q("table")
        .adds(trs))
    ;
  }

  // Control -------------------------------------------------------------------

  function go (fieldPath: Field): Void {
    fn(new Tpath(tpath.table, fieldPath, None));
  }
}

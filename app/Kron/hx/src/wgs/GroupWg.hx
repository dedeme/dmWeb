// Copyright 21-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package wgs;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.It;
import I18n._;

/// Widget to input a notes group
class GroupWg {
  final wg: Domo;
  final groups: Array<String>;
  public final entry: Domo;
  public var editable: Bool;

  public function new (wg: Domo, groups: Array<String>, sel: String) {
    groups.sort((e1, e2) -> e1.toUpperCase() > e2.toUpperCase() ? 1 : -1);
    this.wg = wg;
    this.groups = groups;
    entry = Q("input").att("type", "text").style("width:100px");
    if (sel != "" && groups.contains(sel)) entry.value(sel);
    editable = false;
  }

  // View ----------------------------------------------------------------------

  public function show () {
    final val = entry.getValue().trim();
    final sel = Ui.select(
      "gr",
      It.unary("").cat(It.from(groups)).map(e -> e == val ? "+" + e : e).to()
    );
    sel.on(CHANGE, () -> selection(sel.getValue()));

    if (editable) {
      entry.disabled(false);
      sel.disabled(false);
    } else {
      entry.disabled(true);
      sel.disabled(true);
    }

    wg
      .removeAll()
      .add(Q("table")
        .klass("main")
        .add(Q("tr")
          .add(Q("td")
            .style("text-align:center")
            .add(entry)))
        .add(Q("tr")
          .add(Q("td")
            .style("text-align:center")
            .add(sel))))
    ;
  }

  // Control -------------------------------------------------------------------

  function selection (val: String) {
    entry.value(val);
    show();
  }

}

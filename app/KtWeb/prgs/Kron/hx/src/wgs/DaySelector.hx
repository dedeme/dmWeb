// Copyright 21-Nov-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package wgs;

import dm.Domo;
import dm.Ui.Q;
import dm.It;
import I18n._;

/// Widget to select week days
class DaySelector {
  final wg: Domo;
  /// Days selected (0 == Monday)
  public final days: Array<Int>;
  public var editable = false;

  public function new (wg: Domo, days: Array<Int>) {
    this.wg = wg;
    this.days = days;
  }

  // View ----------------------------------------------------------------------

  public function show () {
    final ds = _("MTWRFSU");
    final tds = editable
      ? It.range(7).map(i ->
          Q("td")
            .klass(days.contains(i) ? "frame" : "GrFrame")
            .style("font-family:monospace;cursor:pointer;" +
              (days.contains(i) ? "font-weight:bold;" : "color:#a9a9a9;"))
            .on(CLICK, () -> click(i))
            .text(ds.charAt(i))
        ).to()
      : It.range(7).map(i ->
          Q("td")
            .klass("GrFrame")
            .style("font-family:monospace;"+
              (days.contains(i) ? "font-weight:bold;" : "color:#a9a9a9;"))
            .text(ds.charAt(i))
        ).to()
    ;

    wg
      .removeAll()
      .add(Q("table")
        .klass("main")
        .add(Q("tr")
          .add(Q("td")
            .add(Q("table")
              .add(Q("tr")
                .adds(It.from(tds).take(4).to())))))
        .add(Q("tr")
          .add(Q("td")
            .add(Q("table")
              .att("align", "center")
              .add(Q("tr")
                .adds(It.from(tds).drop(4).to()))))))
    ;
  }

  // Control -------------------------------------------------------------------

  public function click (d: Int) {
    if (days.contains(d)) {
      days.remove(d);
    } else {
      days.push(d);
    }
    show();
  }

}

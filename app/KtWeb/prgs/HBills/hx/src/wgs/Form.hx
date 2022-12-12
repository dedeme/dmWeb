// Copyright 18-Nov-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Form entry.

package wgs;

import dm.Domo;
import dm.Ui.Q;
import dm.Opt;
import I18n._;
import data.MonthAnn;

class Form {
  public static function mk (
    ann: MonthAnn,
    fclose: () -> Void,
    faccept: (MonthAnn) -> Void
  ): Domo {
    final place0 = switch (ann.place) {case Some(p): p ; case None: "";};
    final entry = Q("input")
      .att("type", "text")
      .att("id", "formEntry")
      .style("width:100px")
      .value(place0)
    ;
    entry.on(KEYDOWN, (ev) -> {
      if (
        ev.code.toLowerCase() == "numpadenter" ||
        ev.code.toLowerCase() == "enter"
      ) {
        final pVal = entry.getValue();
        if (pVal == place0) {
          fclose();
        } else {
          final place = pVal == "" ? None : Some(pVal);
          final a2 = new MonthAnn(
            ann.month,
            place,
            ann.amount
          );
          faccept(a2);
        }
      } else if (ev.code.toLowerCase() == "escape") {
        fclose();
      }
    });
    return Q("table")
      .add(Q("tr")
        .add(Q("td")
          .klass("head")
          .text(_("Set Place"))))
      .add(Q("tr")
        .add(Q("td")
          .text(" ")))
      .add(Q("tr")
        .add(Q("td")
          .add(Q("span")
            .html(
                ann.month.substring(4) + "/" + ann.month.substring(0, 4) +
                ":&nbsp;")
              )
          .add(entry)))

    ;
  }
}

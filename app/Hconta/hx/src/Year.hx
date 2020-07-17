// Copyright 05-Jul-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import data.All;
import I18n._;
import I18n._args;

/// Year page.
class Year {
  /// Constructor.
  ///   wg: Widget.
  ///   data: Accounting data.
  public static function mk (wg: Domo, data: All): Void {
    final year = data.conf.currentYear;

    // Control -----------------------------------------------------------------

    function changeYear (y: String): Void {
      if (data.conf.years.contains(y)) {
        Cts.client.send([
          "source" => Js.ws("Main"),
          "rq" => Js.ws("year"),
          "timeStamp" => Js.ws(data.timeStamp),
          "year" => Js.ws(y),
        ], rp -> {
          if (rp["timeStamp"].rs() == "") {
            Ui.alert(_("Fail trying to change current year."));
          } else {
            js.Browser.location.assign('?$y');
          }
        });
      } else {
        Ui.alert(_args(_("Year %0 not found"), [y]));
      }
    }

    function closeYear (): Void {
      final years = data.conf.years;
      final newYear = Std.parseInt(years[years.length - 1]) + 1;
      data.acc.close(newYear);
      data.conf.currentYear = Std.string(newYear);
      data.send(() -> js.Browser.location.assign('?${data.conf.currentYear}'));
    }

    // View --------------------------------------------------------------------

    function years() {
      function td(y) {
        return Q("td").add(
          y == year
            ? Q("span")
              .klass("frame")
              .html("·" + y + "·")
            : Ui.link(ev -> changeYear(y))
                .klass("link")
                .html("·" + y + "·")
        );
      }
      return Q("tr")
        .add(Q("td")
          .att("colspan", 2)
          .style("text-align:center;")
          .add(Q("table")
            .att("align", "center")
            .add(Q("tr")
            .adds(data.conf.years.map(y -> td(y))))))
      ;
    }

    function close() {
      return Q("tr")
        .add(Q("td")
          .att("colspan", 2)
          .add(Q("button")
            .html(_("Close year"))
            .on(CLICK, e -> {
              if (Ui.confirm(
                _("This operation only can be manually undone.\nContinue?")
              )) {
                closeYear();
              }
            })));
    }

    wg
      .removeAll()
      .add(Q("table")
        .style("width:100%;text-align:center")
        .add(Q("tr")
          .add(Q("td")
            .att("colspan", 2)
            .html("<b>" + _("Year") + "<b>")))
        .add(Q("tr")
          .add(Q("td")
            .style("width:5px;white-space: nowrap;text-align:right")
            .html(_("Change")))
          .add(Q("td")
            .add(Q("hr"))))
        .add(years())
        .add(Q("tr")
          .add(Q("td")
            .style("width:5px;white-space: nowrap;text-align:right")
            .html(_("Close")))
          .add(Q("td")
          .add(Q("hr"))))
        .add(close()))
    ;
  }
}

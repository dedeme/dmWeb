// Copyright 17-Oct-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.settings.calendarPg.wgs;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.It;
import data.Cts;

/// Widget to show an hour.
class Whour {
  var h: Domo;
  var m: Domo;

  public var hour(get, never): Int;
  function get_hour () {
    var sel: js.html.SelectElement = cast(h.e);
    return sel.selectedIndex;
  }

  public var minute(get, never): Int;
  function get_minute () {
    var sel: js.html.SelectElement = cast(m.e);
    return sel.selectedIndex * 5;
  }

  public var wg(get, never): Domo;
  function get_wg () {
    return Q("table")
      .add(Q("tr")
        .add(Q("td")
          .add(h))
        .add(Q("td")
          .add(Q("span")
            .html("<big> : </big>")))
        .add(Q("td")
          .add(m)))
    ;
  }

  /// Constructor.
  ///   hour  : 24 hours day hour.
  ///   minute: Minute.
  ///   action: Action to do when hour or minute changes.
  public function new (hour: Int, minute: Int, action: () -> Void) {
    final hlist = It.range(24).map(
      n -> (n == hour ? "+" : "") + Fns.format00(n)
    ).to();

    final mlist = It.range(12).map(
      n -> (n * 5 == minute ? "+" : "") + Fns.format00(n * 5)
    ).to();

    h = Ui.select("wh", hlist).on(CHANGE, e -> action());
    m = Ui.select("wh", mlist).on(CHANGE, e -> action());
  }
}

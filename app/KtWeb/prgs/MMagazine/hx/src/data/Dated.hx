// Copyright 26-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Dt;

interface Dated {
  public final date: String;
}

class DatedArray {
  static function regularize(a: Array<Dated>): Void {
    function eqYear(d1: String, d2: String): Bool {
      return d1.substring(0, 4) == d2.substring(0, 4);
    }
    function eqMonth(d1: String, d2: String): Bool {
      return d1.substring(0, 6) == d2.substring(0, 6);
    }


    final today = Date.now();
    final lastSunday = Dt.add(today, -Dt.weekDay(today));
    final topMonthly = Dt.add(lastSunday, -56);
    final tMonthly = Dt.to(topMonthly);
    final tYearly = Dt.to(
      Dt.mk(1, Dt.month(topMonthly), Dt.year(topMonthly) - 1)
    );

    final rs = [];
    var last = "";
    for (d in a) {
      if (d.date > tMonthly) rs.push(d);
      else if (d.date < tYearly) {
        if (!eqYear(d.date, last)) rs.push(d);
      }
      else {
        if (!eqMonth(d.date, last)) rs.push(d);
      }

      last = d.date;
    }

    a.resize(0);
    for (d in rs) a.push(d);
  }

  /// Adds a new element in place to and regularize 'a'.
  ///   'a': Sorted array by date, from after to before.
  public static function add (a: Array<Dated>, e: Dated): Array<Dated> {
    if (a[0].date == e.date) a[0] = e;
    else {
      a.unshift(e);
      regularize(a);
    }
    return a;
  }
}



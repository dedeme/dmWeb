// Copyright 03-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package update;

import dm.Dt;
import ex.net.IbexNet;
import db.IbexTb;

/// Ibex quotes update.
class IbexUpdate {
  public static function run(): Void {
    final lastSunday = cm.Fns.lastSunday();
    final today = Date.now();
    if (lastSunday != Dt.to(today)) {
      return;
    }

    var i = -2;
    while (i > -7) {
      var day = Dt.add(today, i);
      switch (IbexNet.read(day)) {
        case Some (v):
          final ibex = IbexTb.read();
          ibex.add(today, v);
          IbexTb.write(ibex);
          return;
        case None:
          --i;
      }
    }
  }
}

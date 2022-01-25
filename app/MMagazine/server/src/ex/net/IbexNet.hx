// Copyright 13-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package ex.net;

using StringTools;

import dm.Opt;
import dm.Dt;
import dm.It;
import dm.Dec;
import dm.Tp;

/// Reader for ibex.
class IbexNet {
  static function getText (): String {
    final url = "https://www.infobolsa.es/indice/historico-ibex_35";
    final pr = new sys.io.Process("wget -q --no-cache -O - " + url);
    final tx = pr.stdout.readAll().toString();
    pr.close();
    return tx;
  }

  static function find (tx: String, date: Date, from: Int): Option<Float> {
    final d = Dt.toIso(date);
    final ix1 = tx.indexOf(">" + d + "<");
    if (ix1 != -1) {
      final ix2 = tx.indexOf("\"price\">", ix1);
      if (ix2 != -1) {
        ix2 += 8;
        final ix3 = tx.indexOf("<", ix2);
        if (ix3 != -1) {
          return Dec.fromIso(tx.substring(ix2, ix3).trim());
        }
      }
    }
    return None;
  }

  public static function read (date: Date): Option<Float> {
    final tx = getText();

    final start = tx.indexOf("historicalPricesTableExport");
    if (start == -1) {
      return None;
    }

    return find(tx, date, start);
  }
}


// Copyright 20-Dec-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/*
Read source file with quotes in format INFOBOLSA and write in
format MultiMarket.

USE:

- Copy the script in the directorio with data file from INFOBOLSA.
- Modify variables 'source' and 'target'.
- Run from a console with:
    haxe -main QuotesConverter --interp
*/

using StringTools;
import sys.io.File;

class Tmp {
  static public function main():Void {
    final source = "berkley.txt";
    final target = "BKY.tb"

    var db: Array<String> = [];
    final tx = File.getContent(source);
    final lines = tx.split("\n");
    for (l in lines) {
      var r: Array<String> = [];
      l = l.trim();
      if (l == "") continue;
      final parts = l.split(" ");
      var first = true;
      for (part in parts) {
        part = part.trim();
        if (part == "") continue;
        if (first) {
          final ps = part.split("/");
          r.push(ps[2] + ps[1] + ps[0]);
          first = false;
          continue;
        }
        r.push(part.replace(".", "").replace(",", "."));
      }
      if (r.length != 6) {
        throw new haxe.Exception("Fail in " + l);
      }
      final newTx = r[0] + ":" + r[2] + ":" + r[1] + ":" +
        r[3] + ":" + r[4] + ":" + r[5] + ":false";
      db.push(newTx);
    }
    File.saveContent(target, db.join("\n"));
    trace("Hecho");
  }
}

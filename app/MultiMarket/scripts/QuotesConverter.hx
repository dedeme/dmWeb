// Copyright 20-Dec-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/*
Reads source file with quotes from INFOBOLSA and writes in
format MultiMarket.

USE:

- Modify source file from INFOBOLSA leaving it in format:
    02/07/2021:5,850:5,950:5,950:5,850:6.434
    01/07/2021:5,990:5,800:6,190:5,750:8.379
    ...
- Put script and source in the same directory.
- Modify variables 'txt' and 'tb'.
- Run from a console with:
    haxe -p /dm/dmHaxe/lib/cmhxlib/src -p /dm/dmHaxe/lib/svhxlib/src --run %f
      (%f is this script)
*/

import dm.File;
using StringTools;

class QuotesConverter {
  static final txt = File.ropen("EAT.txt");
  static final tb = File.wopen("EAT.tb");

  public static function main () {
    File.readLines(txt, l -> {
      final ps = l.split(":");
      final dt = ps[0].split("/");
      final openS = ps[2].replace(".", "").replace(",", ".");
      final open = Std.parseFloat(openS);
      final closeS = ps[1].replace(".", "").replace(",", ".");
      final close = Std.parseFloat(closeS);
      var maxS = ps[3].replace(".", "").replace(",", ".");
      if (open > Std.parseFloat(maxS)) maxS = openS;
      if (close > Std.parseFloat(maxS)) maxS = closeS;
      var minS = ps[4].replace(".", "").replace(",", ".");
      if (open < Std.parseFloat(minS)) minS = openS;
      if (close < Std.parseFloat(minS)) minS = closeS;
      final nl =dt[2] + dt[1] + dt[0] + ":" +
        openS +":" +
        closeS +":" +
        maxS +":" +
        minS +":" +
        ps[5].replace(".", "").replace(",", ".") +":" +
        "false\n";
      File.writeText(tb, nl);
    });

    txt.close();
    tb.close();
  }
}

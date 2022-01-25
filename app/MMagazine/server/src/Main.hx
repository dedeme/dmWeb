// Copyright 31-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import dm.Cgi;
import dm.Path;
import data.acc.Op;

/// Application entry.
class Main {
  static final help = "Usage: MMagazine [update | test | help | <request>]";

  public static function main (): Void {
    final args = Sys.args();

    if (args.length != 1) {
      Sys.println(help);
      return;
    }

    final env = Sys.environment();
    final user = env["USER"];
    if (user == null) {
      Cgi.init(Path.cat([env["CONTEXT_DOCUMENT_ROOT"], Cts.webHome]), 900);
      Db.init();
      Pgs.process(args[0]);
    } else if (args[0] == "update") {
      Db.init();
      Update.process();
    } else if (args[0] == "test") {
      Db.init();
      trace("Done");
    } else if (args[0] == "help") {
      Sys.println(help);
    } else {
      Sys.println("Argument '" + args[0] + "' is not valid.\n" + help);
    }
  }
}

// Copyright 20-Nov-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs;

import dm.Cgi;
import dm.Js;

/// Main page.
class Main {
  public static function process(mrq: Map<String, Js>): String {
    final rq = Cgi.rqString(mrq, "rq");
    return switch (rq) {
      case "close":
        final sessionId = Cgi.rqString(mrq, "sessionId");
        Cgi.delSession(sessionId);
        Cgi.rpEmpty();
      default: throw new haxe.Exception(
          'Value of rq (${rq}) is not valid'
        );
    }
  }
}

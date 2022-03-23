// Copyright 03-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import dm.Cgi;
import dm.Cryp;
import dm.Js;
import pgs.ChangePass;

/// Web entry.
class Pgs {

  public static function process (rq: String): Void {
    final ix = rq.indexOf(":");

    // CONNECTION --------------------------------------------------------------

    if (ix == -1) {
      Sys.print(Cgi.connect(rq));
      return;
    }

    // AUTHENTICATION ----------------------------------------------------------

    if (ix == 0) {
      final key = Cryp.key(cm.Cts.appName, Cgi.klen);
      final data = Cryp.decryp(key, rq.substring(1));
      final ps = data.split(":");
      Sys.print(Cgi.authentication(key, ps[0], ps[1], ps[2] == "1"));
      return;
    }

    // NORMAL DATA -------------------------------------------------------------

    final sessionId = rq.substring(0, ix);
    var conKey = "";
    var rest = rq.substring(ix + 1);
    final ix2 = rest.indexOf(":");
    if (ix2 != -1) {
      conKey = rest.substring(0, ix2);
      rest = rest.substring(ix2 + 1);
    }
    switch (Cgi.getComKey(sessionId, conKey)) {
      case None: Sys.print(Cgi.rpExpired());
      case Some(comKey):
        final js = Cryp.decryp(comKey, rest);
        final data = Js.from(js).ro();
        Sys.print(processHub(data));
    }
  }

  static function processHub(mrq: Map<String, Js>): String {
    final source = Cgi.rqString(mrq, "source");
    return switch (source) {
      case "Main": pgs.Main.process(mrq);
      case "Home": pgs.Home.process(mrq);
      case "RankingsPg": pgs.RankingsPg.process(mrq);
      case "ProfitsPg": pgs.ProfitsPg.process(mrq);
      case "PercentsPg": pgs.PercentsPg.process(mrq);
      case "ModelsPg": pgs.ModelsPg.process(mrq);
      case "HotMapsPg": pgs.HotMapsPg.process(mrq);
      case "ChangePass": pgs.ChangePass.process(mrq);
      default: throw new haxe.Exception(
          'Value of source (${source}) is not valid'
        );
    }
  }
}

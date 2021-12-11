// Copyright 19-Nov-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import dm.Cgi;
import dm.Cryp;
import dm.Js;
import db.Db;
import db.ConfTb;
import db.PathsTb;
import pgs.PathsPg;
import pgs.ChangePass;
import pgs.Index;
import pgs.Module;
import pgs.Code;

/// Initial page
class Main {

  public static function main (): Void {
    final args = Sys.args();

    if (args.length != 1) {
      throw (new haxe.Exception("HxDoc need one and only one argument."));
    }

    final rq = args[0];

    Cgi.init(Cts.home, 900);
    Db.init();

    final ix = rq.indexOf(":");

    // CONNECTION --------------------------------------------------------------

    if (ix == -1) {
      Sys.print(Cgi.connect(rq));
      return;
    }

    // AUTHENTICATION ----------------------------------------------------------

    if (ix == 0) {
      final key = Cryp.key(cm.Cts.app, Cgi.klen);
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
      case "Main": process(mrq);
      case "Paths": PathsPg.process(mrq);
      case "ChangePass": ChangePass.process(mrq);
      case "Index": Index.process(mrq);
      case "Module": Module.process(mrq);
      case "Code": Code.process(mrq);
      default: throw new haxe.Exception(
          'Value of source (${source}) is not valid'
        );
    }
  }

  static function process(mrq: Map<String, Js>): String {
    final rq = Cgi.rqString(mrq, "rq");
    return switch (rq) {
      case "read":
        final paths = PathsTb.read();
        for (p in paths.list) {
          p.isValid = dm.File.isDirectory(p.path);
          if (!p.isValid) {
            p.isShown = false;
          }
        }
        Cgi.rp([
          "conf" => ConfTb.readJs(),
          "paths" => paths.toJs()
        ]);
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

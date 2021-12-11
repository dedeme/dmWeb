// Copyright 19-Nov-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs;

using StringTools;

import dm.Cgi;
import dm.Js;
import dm.Path;
import dm.File;
import db.ConfTb;
import db.PathsTb;

/// Index page.
class Index {

  static function readFile (path: String): {id: String, doc: String} {
    final fname = Path.name(path);
    var doc = "";

    final f = File.ropen(path);
    while (true) {
      try {
        var l = File.readLine(f).trim();
        if (l.startsWith("///")) {
          l = l.substring(3).trim();
          final ix = l.indexOf(".");
          if (ix == -1) {
            doc = l;
          } else {
            doc = l.substring(0, ix + 1);
          }
          break;
        }
      } catch (e: haxe.io.Eof) {
        break;
      }
    }
    f.close();

    return { id: fname.substring(0, fname.length - 3), doc: doc };
  }

  static function readDir (
    level: Int, prefix: String, path: String
  ): Array<Array<String>> {
    final tabs = new StringBuf();
    for (i in 0...level) tabs.add("&nbsp;&nbsp;&nbsp;&nbsp;");

    final r = new Array<Array<String>>();
    final files = new Array<{id: String, doc: String}>();
    final dirs = new Array<String>();

    for (fname in File.dir(path)) {
      final newPath = Path.cat([path, fname]);
      if (File.isDirectory(newPath)) {
        dirs.push(newPath);
      } else if (fname.endsWith(".hx")) {
        files.push(readFile(newPath));
      }
    }

    files.sort((f1, f2) -> f1.id.toUpperCase() > f2.id.toUpperCase() ? 1 : -1);
    for (f in files) {
      final p = prefix != "" ? Path.cat([prefix, f.id]) : f.id;
      r.push([tabs.toString() + f.id, p, f.doc]);
    }

    dirs.sort((d1, d2) -> d1.toUpperCase() > d2.toUpperCase() ? 1 : -1);
    for (d in dirs) {
      final id = Path.name(d);
      final p = prefix != "" ? Path.cat([prefix, id]) : id;
      r.push([tabs.toString() + id, "", ""]);
      for (subd in readDir(level + 1, p, d)) r.push(subd);
    }

    return r;
  }

  public static function process(mrq: Map<String, Js>): String {
    final path = Cgi.rqString(mrq, "path");
    final list = Js.wa(readDir(0, "", path).map(e ->
        Js.wa(e.map(f -> Js.ws(f)))
      ));

    return Cgi.rp(["entries" => list]);
  }
}

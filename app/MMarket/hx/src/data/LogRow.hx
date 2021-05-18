// Copyright 13-May-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Log entry.

package data;

using StringTools;
import dm.Dt;
import dm.Js;
import dm.Opt;

/// Log entry.
class LogRow {
  public var isError(default, null): Bool;
  public var date(get, never): Date;
  function get_date() {
    final ix = time.indexOf("(");
    return Opt.eget(Dt.fromIso(time.substring(0, ix).trim()));
  }
  var time: String;
  var msg: String;

  // Constructor
  //   isError: If 'msg' is an error message.
  //   time   : Time of message.
  //   msg    : Message
  function new (isError: Bool, time: String, msg: String) {
    this.isError = isError;
    this.time = time;
    this.msg = msg;
  }

  /// Returns a formatted entry.
  ///   lineWidth: Width of line.
  public function format (lineWidth: Int): String {
    final indent = time.length + 3;
    final len = lineWidth - indent;
    final sep = isError ? " = " : " - ";
    return time + sep + format2(msg, indent, len);
  }

  public static function fromJs (js: Js): LogRow {
    final a = js.ra();
    return new LogRow(a[0].rb(), a[1].rs(), a[2].rs());
  }

  static function format2 (m: String, indent: Int, len: Int): String {
    if (m.trim() == "") return m;

    final r = [];
    Lambda.iter(m.split("\n"), l -> {
      final subr = [];

      while (l.length > len) {
        var line = l.substring(0, len);
        l = l.substring(len);
        final ix = line.lastIndexOf(" ");
        if (ix != -1 && line.substring(0, ix).trim() != "") {
          l = line.substring(ix + 1) + l;
          line = line.substring(0, ix);
        }
        subr.push(line);
      }

      if (l.trim() != "") subr.push(l);
      Lambda.iter(subr, subl -> r.push(subl));
    });

    var ind = "";
    for (i in 0...indent) ind += " ";
    return r.join("\n" + ind);
  }

}

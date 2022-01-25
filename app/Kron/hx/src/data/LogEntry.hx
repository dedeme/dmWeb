// Copyright 14-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Log entry.
package data;

using StringTools;
import dm.Dt;
import dm.Js;
import dm.Opt;
import I18n._;

/// Log entry.
class LogEntry {
  public final type: String;
  public var date(get, never): Date;
  function get_date() {
    final ix = time.indexOf("(");
    return Opt.eget(Dt.fromIso(time.substring(0, ix).trim()));
  }
  var time: String;
  var msg: String;

  // Constructor
  //   type: F(ail), I(nformation), M(essage).
  //   time: Time of message.
  //   msg : Message
  function new (type: String, time: String, msg: String) {
    this.type = type;
    this.time = time;
    this.msg = msg;
  }

  /// Returns a formatted entry.
  ///   lineWidth: Width of line.
  public function format (lineWidth: Int): String {
    final indent = time.length + 3;
    final len = lineWidth - indent;
    final sep = type == "F" ? " = " : " - ";
    final m = (type == "M") ? _("MESSAGE") + "\n" + msg : msg;
    return time + sep + format2(m, indent, len);
  }

  public static function fromJs (js: Js): LogEntry {
    final a = js.ra();
    return new LogEntry(a[0].rs(), a[1].rs(), a[2].rs());
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

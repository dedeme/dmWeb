// Copyright 22-Jan-2023 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Response of Editor page.

package data;

import dm.Js;
import dm.Opt;

class EditorRp {
  public static final NO_TABLE = "NoTable";
  public static final NO_JSON = "NoJSON";
  public static final MODIFIED = "Modified";

  public final error: String;
  public final jdata: Option<JsData>;

  function new (error: String, jdata: Option<JsData>) {
    this.error = error;
    this.jdata = jdata;
  }

  public static function fromJs (js: Js): EditorRp {
    final a = js.ra();
    final e1 = a[1].ra();
    return new EditorRp(
      a[0].rs(),
      e1.length == 0 ? None : Some(JsData.fromJs(e1[0]))
    );
  }

}

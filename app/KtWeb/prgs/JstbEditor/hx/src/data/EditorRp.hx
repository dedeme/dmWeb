// Copyright 22-Jan-2023 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Response of Editor page.

package data;

import dm.Js;
import dm.Opt;

class EditorRp {
  public static final NO_TABLE = "NoTable";
  public static final NO_JSON = "NoJSON";
  public static final NO_TPATH = "NoTpath";
  public static final NO_CONF = "NoConf";
  public static final MODIFIED = "Modified";

  public final error: String;
  public final tpath: Option<Tpath>;
  public final jdata: Option<JsData>;

  function new (error: String, tpath: Option<Tpath>, jdata: Option<JsData>) {
    this.error = error;
    this.tpath = tpath;
    this.jdata = jdata;
  }

  public static function fromJs (js: Js): EditorRp {
    final a = js.ra();
    return new EditorRp(
      a[0].rs(),
      a[1].isNull() ? None : Some(Tpath.fromJs(a[1])),
      a[2].isNull() ? None : Some(JsData.fromJs(a[2]))
    );
  }

}

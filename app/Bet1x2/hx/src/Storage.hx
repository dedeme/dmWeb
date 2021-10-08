// Copyright 14-Sep-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import dm.Store;
import dm.Opt;
import dm.Js;

/// Client data base
class Storage {
  static final lang = Cts.app + "_lang";
  public static function getLang (): String {
    return switch (Store.get(lang)) {
      case Some("en"): "en";
      default: "es";
    };
  }
  public static function setLang (l: String): Void {
    Store.put(lang, l);
  }
}

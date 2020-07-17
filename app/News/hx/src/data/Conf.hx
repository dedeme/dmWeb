// Copyright 27-Jun-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Js;
import dm.Dt;

/// Application configuration.
class Conf {
  /// Can be "en" or "es".
  public var lang: String;

  function new (lang: String) {
    this.lang = lang;
  }

  public function toJs (): Js {
    return Js.wo(["lang" => Js.ws(lang)]);
  }

  public static function fromJs (js: Js): Conf {
    final o = js.ro();
    return new Conf(o.get("lang").rs());
  }
}

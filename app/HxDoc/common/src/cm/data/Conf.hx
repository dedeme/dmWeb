// Copyright 19-Nov-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package cm.data;

import dm.Js;

/// Settings data.
class Conf {
  var conf: Map<String, Js>;

  public var lang(get, set): String;
  function get_lang () return conf["lang"].rs();
  function set_lang (v) { conf["lang"] = Js.ws(v); return v; }

  public var source(get, set): String;
  function get_source () return conf["source"].rs();
  function set_source (v) { conf["source"] = Js.ws(v); return v; }

  public var show(get, set): Bool;
  function get_show () return conf["show"].rb();
  function set_show (v) { conf["show"] = Js.wb(v); return v; }

  function new (conf: Map<String, Js>) {
    this.conf = conf;
  }

  public function toJs (): Js {
    return Js.wo(conf);
  }

  public static function fromJs (js: Js): Conf {
    return new Conf(js.ro());
  }

  /// Returns a first time initialized "Conf"
  public static function mkInitial (): Conf {
    final cf = new Conf([]);
    cf.lang = "es";
    cf.source = "@";
    cf.show = true;
    return cf;
  }
}

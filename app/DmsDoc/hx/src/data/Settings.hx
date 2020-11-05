// Copyright 19-Jun-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Js;

class Settings {
  var conf: Map<String, Js>;
  /// Source paths.
  public var paths(default, null): Array<Lpath>;

  public var lang(get, set): String;
  function get_lang () return conf["lang"].rs();
  function set_lang (v) { conf["lang"] = Js.ws(v); return v; }

  public var source(get, set): String;
  function get_source () return conf["source"].rs();
  function set_source (v) { conf["source"] = Js.ws(v); return v; }

  public var show(get, set): Bool;
  function get_show () return conf["show"].rb();
  function set_show (v) { conf["show"] = Js.wb(v); return v; }

  function new (conf: Map<String, Js>, paths: Array<Lpath>) {
    this.conf = conf;
    paths.sort((p1, p2) ->
      p1.lib.toUpperCase() > p2.lib.toUpperCase() ? 1 : -1
    );
    this.paths = paths;
  }

  public function toJs (): Js {
    return Js.wo([
      "conf" => Js.wo(conf),
      "paths" => Js.wArray(paths, e -> e.toJs())
    ]);
  }

  public static function fromJs (js: Js): Settings {
    var ob = js.ro();
    return new Settings(
      ob["conf"].ro(),
      ob["paths"].rArray(Lpath.fromJs)
    );
  }

}

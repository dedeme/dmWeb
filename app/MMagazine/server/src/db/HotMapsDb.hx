// Copyright 25-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package db;

import dm.Path;
import dm.File;
import dm.Js;
import ex.mmarket.MModelEvalsDb;
import cm.data.HotMap;
import cm.data.ParamsEval;

/// Data base of model hot maps.
class HotMapsDb {
  static var dir: String = null;

  static function fpath (model: String): String {
    return Path.cat([dir, model + ".tb"]);
  }

  public static function init (parent: String) {
    dir = Path.cat([parent, "hotMaps"]);
    if (!File.exists(dir)) File.mkdir(dir);
    for (m in MModelEvalsDb.list()) {
      final path = fpath(m);
      if (!File.exists(path)) {
        write(m, [new HotMap(
          cm.Fns.lastSunday(),
          MModelEvalsDb.paramsEvals(m)
        )]);
      }
    }
  }

  /// 'hotMaps' is an unsorted array.
  public static function write (model: String, hotMaps: Array<HotMap>): Void {
    File.write(fpath(model), Js.wa(hotMaps.map(e -> e.toJs())).to());
  }

  public static function writeMap (
    model: String, data: Map<String, Array<ParamsEval>>
  ): Void {
    final r = [];
    for (k => v in data) r.push(new HotMap(k, v));
    write(model, r);
  }

  public static function readJs (model: String): Js {
    return Js.from(File.read(fpath(model)));
  }

  /// Returns an unsorted array.
  public static function read (model: String): Array<HotMap> {
    return readJs(model).ra().map(e -> HotMap.fromJs(e));
  }

  public static function readMap (
    model: String
  ): Map<String, Array<ParamsEval>> {
    final r = new Map<String, Array<ParamsEval>>();
    for (e in read(model)) r[e.date] = e.entries;
    return r;
  }
}

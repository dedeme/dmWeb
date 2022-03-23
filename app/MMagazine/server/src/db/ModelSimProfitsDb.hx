// Copyright 25-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package db;

import dm.Path;
import dm.File;
import dm.Js;
import cm.data.ModelFloats;
import ex.mmarket.MModelSimProfitsDb;

/// Evaluation models table of several dates.
class ModelSimProfitsDb {
  static var dir: String = null;
  static var ftotals: String = null;
  static var fcashes: String = null;
  static var frefs: String = null;


  public static function init (parent: String): Void {
    dir = Path.cat([parent, "simulation"]);
    if (!File.exists(dir)) {
      File.mkdir(dir);
    }

    ftotals = Path.cat([dir, "totals.tb"]);
    if (!File.exists(ftotals)) {
      writeTotals([new ModelFloats(
        cm.Fns.lastSunday(),
        MModelSimProfitsDb.totalsRanking()
      )]);
    }
    fcashes = Path.cat([dir, "cashes.tb"]);
    if (!File.exists(fcashes)) {
      writeCashes([new ModelFloats(
        cm.Fns.lastSunday(),
        MModelSimProfitsDb.cashesRanking()
      )]);
    }
    frefs = Path.cat([dir, "refs.tb"]);
    if (!File.exists(frefs)) {
      writeRefs([new ModelFloats(
        cm.Fns.lastSunday(),
        MModelSimProfitsDb.refsRanking()
      )]);
    }
  }

  /// 'data' is an unsorted array.
  public static function writeTotals (data: Array<ModelFloats>): Void {
    File.write(ftotals, Js.wa(data.map(e -> e.toJs())).to());
  }

  /// 'data' is an unsorted array.
  public static function writeCashes (data: Array<ModelFloats>): Void {
    File.write(fcashes, Js.wa(data.map(e -> e.toJs())).to());
  }

  /// 'data' is an unsorted array.
  public static function writeRefs (data: Array<ModelFloats>): Void {
    File.write(frefs, Js.wa(data.map(e -> e.toJs())).to());
  }

  public static function readTotalsJs (): Js {
    return Js.from(File.read(ftotals));
  }

  public static function readTotals (): Array<ModelFloats> {
    return readTotalsJs().ra().map(e -> ModelFloats.fromJs(e));
  }

  public static function readCashesJs (): Js {
    return Js.from(File.read(fcashes));
  }

  public static function readCashes (): Array<ModelFloats> {
    return readCashesJs().ra().map(e -> ModelFloats.fromJs(e));
  }

  public static function readRefsJs (): Js {
    return Js.from(File.read(frefs));
  }

  public static function readRefs (): Array<ModelFloats> {
    return readRefsJs().ra().map(e -> ModelFloats.fromJs(e));
  }

}

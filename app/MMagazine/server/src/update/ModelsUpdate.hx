// Copyright 26-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package update;

import cm.data.Dated;
import cm.data.ModelFloats;
import cm.data.ModelEvals;
import cm.data.HotMap;
import db.ModelSimProfitsDb;
import db.ModelEvalsTb;
import db.HotMapsDb;
import ex.mmarket.MModelEvalsDb;
import ex.mmarket.MModelSimProfitsDb;

/// Model evalutations from MMakert update.
class ModelsUpdate {
  public static function run (): Void {
    final lastSunday = cm.Fns.lastSunday();

    final mdEvs = ModelEvalsTb.read();
    final newMdEvs = DatedArray.add(
      cast(mdEvs),
      new ModelEvals(lastSunday, MModelEvalsDb.ranking())
    ).map(e -> cast(e, ModelEvals));
    ModelEvalsTb.write(newMdEvs);

    for (m in MModelEvalsDb.list()) {
      final maps = HotMapsDb.read(m);
      final newMaps = DatedArray.add(
        cast(maps),
        new HotMap(lastSunday, MModelEvalsDb.paramsEvals(m))
      ).map(e -> cast(e, HotMap));
      HotMapsDb.write(m, newMaps);
    }

    final mdTotals = ModelSimProfitsDb.readTotals();
    final newMdTotals = DatedArray.add(
      cast(mdTotals),
      new ModelFloats(lastSunday, MModelSimProfitsDb.totalsRanking())
    ).map(e -> cast(e, ModelFloats));
    ModelSimProfitsDb.writeTotals(newMdTotals);

    final mdCashes = ModelSimProfitsDb.readCashes();
    final newMdCashes = DatedArray.add(
      cast(mdCashes),
      new ModelFloats(lastSunday, MModelSimProfitsDb.cashesRanking())
    ).map(e -> cast(e, ModelFloats));
    ModelSimProfitsDb.writeCashes(newMdCashes);

    final mdRefs = ModelSimProfitsDb.readRefs();
    final newMdRefs = DatedArray.add(
      cast(mdRefs),
      new ModelFloats(lastSunday, MModelSimProfitsDb.refsRanking())
    ).map(e -> cast(e, ModelFloats));
    ModelSimProfitsDb.writeRefs(newMdRefs);

  }
}

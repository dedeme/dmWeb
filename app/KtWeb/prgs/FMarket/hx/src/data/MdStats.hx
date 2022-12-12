// Copyright 08-Dic-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Js;

class MdStats {
  public final models: Array<Model>;
  public final groupsRanking: Array<Group>;

  function new (models: Array<Model>, groupsRanking: Array<Group>) {
    this.models = models;
    this.groupsRanking = groupsRanking;
  }

  public static function fromJs (js: Js): MdStats {
    final a = js.ra();
    return new MdStats(
      a[0].ra().map(Model.fromJs),
      a[1].ra().map(Group.fromJs)
    );
  }
}

/// Model statistics
class Model {
  public final modelId: String;
  public final bests: Array<OrderFlea>;
  public final worsts: Array<OrderFlea>;
  public final nfleas: Int;
  public final position: Int;
  public final assets: Float;

  function new (
    modelId: String, bests: Array<OrderFlea>, worsts: Array<OrderFlea>,
    nfleas: Int, position: Int, assets: Float
  ) {
    this.modelId = modelId;
    this.bests = bests;
    this.worsts = worsts;
    this.nfleas = nfleas;
    this.position = position;
    this.assets = assets;
  }

  public static function fromJs (js: Js): Model {
    final a = js.ra();
    return new Model(
      a[0].rs(),
      a[1].ra().map(OrderFlea.fromJs),
      a[2].ra().map(OrderFlea.fromJs),
      a[3].ri(),
      a[4].ri(),
      a[5].rf()
    );
  }
}

/// Tuple with (position in the list of all the fleas, flea).
class OrderFlea {
  public final order: Int;
  public final flea: Flea;

  function new (order: Int, flea: Flea) {
    this.order = order;
    this.flea = flea;
  }

  public static function fromJs (js: Js): OrderFlea {
    final a = js.ra();
    return new OrderFlea(
      a[0].ri(),
      Flea.fromJs(a[1])
    );
  }
}

class Group {
  public final modelIds: Array<String>;
  public final duplicates: Int;

  function new (modelIds: Array<String>, duplicates: Int) {
    this.modelIds = modelIds;
    this.duplicates = duplicates;
  }

  public function formatIds (): String {
    return modelIds
      .map(id -> {
        while (id.length < 5) id += " ";
        return id;
      })
      .join(" | ")
    ;
  }

  public static function fromJs (js: Js): Group {
    final a = js.ra();
    return new Group(
      a[0].ra().map(e -> e.rs()),
      a[1].ri()
    );
  }

}


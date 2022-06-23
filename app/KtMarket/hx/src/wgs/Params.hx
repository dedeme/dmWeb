// Copyright 03-Nov-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package wgs;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;

/// Parameters widget
class Params {
  public final wg = Q("div");
  var ps: Array<Param>;
  public var value(get, never): Array<Float>;
  function get_value() return ps.map(p -> p.value);

  /// Constructor:
  ///   names : Parameter names.
  ///   mins  : Minimum values.
  ///   maxs  : Maximum values.
  ///   id    : TextFields root id. Final id will be made with id + index.
  ///   nextId: Id of next widget to pass focus.
  ///   values: Defaul values.
  public function new (
    names: Array<String>, mins: Array<Float>, maxs: Array<Float>,
    id: String, nextId: String, ?values: Array<Float>
  ) {
    ps = [];
    for (i in 0...mins.length) {
      final nx = (i < mins.length - 1) ? id + Std.string(i + 1) : nextId;
      ps.push(values == null
        ? new Param(names[i], mins[i], maxs[i], id + Std.string(i), nx)
        : new Param(
            names[i], mins[i], maxs[i], id + Std.string(i), nx, values[i]
          )
      );
    }

    view();
  }

  // View ----------------------------------------------------------------------

  function view () {
    wg
      .removeAll()
      .add(Q("table").klass("frame")
        .add(Q("tr")
          .adds(ps.map(p -> Q("td").add(p.wg)))))
    ;
  }
}


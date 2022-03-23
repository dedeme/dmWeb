// Copyright 08-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Js;
import dm.Opt;

/// Entry of index.
/// It can be a file path or a directory path.
class IndexTree {
  /// Index entry identifier.
  public final id: String;
  /// Documentation, if the entry is a file path. Otherwise its value is None.
  public final doc: Option<String>;
  /// If 'doc' is 'None', entries of directory.
  public final trees: Array<IndexTree>;

  function new (
    id: String, doc: Option<String>, trees: Array<IndexTree>
  ) {
    this.id = id;
    this.doc = doc;
    this.trees = trees;
  }

  public function toJs (): Js {
    return Js.wa([
      Js.ws(id),
      switch (doc) { case None: Js.wn(); case Some(v): Js.ws(v); },
      Js.wa(trees.map(e -> e.toJs()))
    ]);
  }

  public static function fromJs (js: Js): IndexTree {
    final a = js.ra();
    return new IndexTree(
      a[0].rs(),
      a[1].isNull() ? None : Some(a[1].rs()),
      a[2].ra().map(e -> fromJs(e))
    );
  }
}

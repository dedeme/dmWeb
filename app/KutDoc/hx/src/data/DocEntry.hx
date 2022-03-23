// Copyright 08-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Js;

/// Documentation entry.
class DocEntry {
  /// Entry name.
  public final name: String;
  /// Entry documentation.
  public final doc: String;
  /// Entry code.
  public final code: String;
  /// Entry code page link.
  public final link: String;

  function new (
    name: String, doc: String, code: String, link: String
  ) {
    this.name = name;
    this.doc = doc;
    this.code = code;
    this.link = link;
  }

  public function toJs (): Js {
    return Js.wa([
      Js.ws(name),
      Js.ws(doc),
      Js.ws(code),
      Js.ws(link)
    ]);
  }

  public static function fromJs (js: Js): DocEntry {
    final a = js.ra();
    return new DocEntry(
      a[0].rs(),
      a[1].rs(),
      a[2].rs(),
      a[3].rs()
    );
  }

}

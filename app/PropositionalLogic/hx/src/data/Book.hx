// Copyright 02-Aug-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

using StringTools;
import haxe.Exception as Exc;
import dm.Js;
import dm.Rs;
import dm.Opt;
import dm.It;
import dm.Tp;
import dm.Dec;
import data.Prop;

class Book {
  /// All the theorems
  public final entries: Array<BookEntry>;

  function new (es: Array<BookEntry>) {
    entries = es;
  }

  /// 'id' has the form: 'name'-'number' (e.g.: "NC-21")
  public function get(id: String): Option<Demo> {
    return Opt.bind(
      It.from(entries).find(e -> e.id.toString() == id),
      e -> Some(e.demo)
    );
  }

  /// Adds a new Demo and returns its identifer. If it already exists,
  /// returns Error=DuplicatedId.
  public function add (demo: Demo): Result<String> {
    final newDemo = demo.regularize();
    final name = newDemo.name();
    final conclusion = newDemo.conclusion;
    var maxN = -1;
    for (e in entries) {
      if (e.id.name == name) {
        if (Prop.eq(conclusion, e.demo.conclusion))
          return Error(e.id.toString());
        if (e.id.n > maxN) maxN = e.id.n;
      }
    }
    final id = new BookId(name, maxN + 1);
    entries.push(new BookEntry(id, newDemo));
    return Ok(id.toString());
  }

  /// Replace an old entry with identifier 'id' or do nothing if such identifier
  /// does not exist.
  public function replace (id: String, demo: Demo): Void {
    final newDemo = demo.regularize();
    for (i in 0...entries.length) {
      final e = entries[i];
      if (e.id.toString() == id) {
        entries[i] = new BookEntry(e.id, newDemo);
        break;
      }
    }
  }

  /// Interchanges identifiers of two demonstrations.
  public function interchange (id1: BookId, id2: BookId): Void {
    final id1Str = id1.toString();
    final id2Str = id2.toString();

    for (i in 0...entries.length) {
      final e = entries[i];
      final id = e.id;
      final demo = e.demo;

      if (id.eq(id1)) entries[i] = new BookEntry(id2, demo);
      else if (id.eq(id2)) entries[i] = new BookEntry(id1, demo);

      for (j in 0...demo.reasons.length) {
        switch (demo.reasons[j]) {
          case Theorem(idStr, n, rpls) if (idStr == id1Str):
            demo.reasons[j] = Reason.mkTheorem(id2Str, n, rpls);
          case Theorem(idStr, n, rpls) if (idStr == id2Str):
            demo.reasons[j] = Reason.mkTheorem(id1Str, n, rpls);
          default:
        }
      }
    }
  }

  /// Returns identifiers of theorems with depend on the theorem 'id'.
  public function used (id: String):  Array<String> {
    final r: Array<String> = [];
    for (e in entries) {
      for (rs in e.demo.reasons) {
        switch (rs) {
          case Theorem(thId, n, rpls) if (thId == id): r.push(e.id.toString());
          default:
        }
      }
    }
    return r;
  }

  /// Remove the theorem 'id'.
  public function remove (id: String): Void {
    if (used(id).length > 0)
      throw(new haxe.Exception("There are theorems depending on " + id));
    final ix = It.from(entries).indexf(e -> e.id.toString() == id);
    if (ix != 1) entries.splice(ix, 1);
  }

  /// Returns identifiers with start for 'name' + one symbol more, and
  /// an array of Tp<identifier, conclusion> with name 'name'.
  public function search (
    name: String
  ): Tp<Array<String>, Array<Tp<String, BookEntry>>> {
    final names: Array<String> = [];
    final ps: Array<Tp<String, BookEntry>> = [];

    for (e in entries) {
      if (e.id.name == name) {
        ps.push(new Tp(e.id.toString(), e));
      } else if (e.id.name.startsWith(name)) {
        final name2 = e.id.name.substring(0, name.length + 1);
        if (!names.contains(name2)) names.push(name2);
      }
    }

    return new Tp(names, ps);
  }

  public function toJs (): Js {
    return Js.wa(entries.map(e -> e.toJs()));
  }

  public static function fromJs (js: Js): Book {
    return new Book(js.ra().map(BookEntry.fromJs));
  }
}

class BookEntry {
  /// Theorem identifier.
  public final id: BookId;
  /// Theorem demonstration.
  public final demo: Demo;

  /// Creates a new BookEntry
  public function new (id: BookId, demo: Demo) {
    this.id = id;
    this.demo = demo;
  }

  public function toJs (): Js {
    return Js.wa([
      id.toJs(),
      demo.toJs()
    ]);
  }

  public static function fromJs (js: Js): BookEntry {
    final a = js.ra();
    return new BookEntry(
      BookId.fromJs(a[0]),
      Demo.fromJs(a[1])
    );
  }
}

class BookId {
  public final name: String;
  public final n: Int;

  public function new (name: String, n: Int) {
    this.name = name;
    this.n = n;
  }

  public function eq (id: BookId): Bool {
    return id.name == name && id.n == n;
  }

  public function toString (): String {
    return name + "-" + n;
  }

  public function toJs (): Js {
    return Js.wa([
      Js.ws(name),
      Js.wi(n)
    ]);
  }

  public static function fromJs (js: Js): BookId {
    final a = js.ra();
    return new BookId (
      a[0].rs(),
      a[1].ri()
    );
  }
}

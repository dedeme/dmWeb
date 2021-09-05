// Copyright 27-Aug-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Js;
import dm.It;
import I18n._;
import I18n._args;

/// Plan table
class Plan {
  public var entries(default, null): Array<PlanEntry>;

  function new (entries: Array<PlanEntry>) {
    this.entries = entries;
  }

  /// Return "" or a message of error if 'add' fails.
  public function add (entry: PlanEntry): String {
    final id = entry.id;

    if (id == "") return _("Id is missing");
    if (entry.desc == "") return _("Description is missing");

    if (contains(id)) {
      return _args(_("The account '%0' already exists"), [id]);
    }
    entries.push(entry);
    return "";
  }

  /// Return "" or a message of error if 'del' fails.
  public function del (idEntry: String, diaryAccs: Array<String>): String {
    if (diaryAccs.contains(idEntry)) {
      return _args(
        _("The account '%0' has annotations and can not be deleted"),
        [idEntry]
      );
    }
    final ix = It.from(entries).indexf(e -> e.id == idEntry);
    if (ix != -1) {
      entries.splice(ix, 1);
    }
    return "";
  }

  /// Modifies 'diary' if idEntry != entry.id.
  /// Return "" or a message of error if 'modify' fails.
  public function modify (
    idEntry: String, entry: PlanEntry, diary: Diary
  ): String {
    final newId = entry.id;

    if (newId == "") return _("Id is missing");
    if (entry.desc == "") return _("Description is missing");

    if (idEntry == newId) {
      for (i in 0...entries.length) {
        if (entries[i].id == idEntry) {
          entries[i] = entry;
          break;
        }
      }
      return "";
    }

    if (It.from(entries).indexf(e -> e.id == newId) != -1) {
      return _args(_("The account '%0' already exists"), [newId]);
    }

    for (i in 0...entries.length) {
      if (entries[i].id == idEntry) {
        entries[i] = entry;
        break;
      }
    }

    diary.changeAcc(idEntry, newId);

    return "";
  }

  public function contains (id: String): Bool {
    return It.from(entries).indexf(e -> e.id == id) != -1;
  }

  public function isIncome (id: String): Bool {
    switch (It.from(entries).find(e -> e.id == id)) {
    case None:
      throw new haxe.Exception("Count " + id + " not found");
    case Some(e):
      return e.isIncome;
    }
  }

  public function desc (id: String): String {
    switch (It.from(entries).find(e -> e.id == id)) {
    case None:
      throw new haxe.Exception("Count " + id + " not found");
    case Some(e):
      return e.desc;
    }
  }

  public function toJs (): Js {
    return Js.wa(entries.map(e -> e.toJs()));
  }

  public static function fromJs (js: Js): Plan {
    return new Plan(js.ra().map(e -> PlanEntry.fromJs(e)));
  }

}

class PlanEntry {
  public var isIncome(default, null): Bool;
  public var id(default, null): String;
  public var desc(default, null): String;

  public function new (isIncome: Bool, id: String, desc: String) {
    this.isIncome = isIncome;
    this.id = id;
    this.desc = desc;
  }

  public function toJs (): Js {
    return Js.wa([
      Js.wb(isIncome),
      Js.ws(id),
      Js.ws(desc)
    ]);
  }

  public static function fromJs (js: Js): PlanEntry {
    final a = js.ra();
    return new PlanEntry(
      a[0].rb(),
      a[1].rs(),
      a[2].rs()
    );
  }
}


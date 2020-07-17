// Copyright 28-Jun-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

using StringTools;

import dm.Js;
import dm.It;
import dm.Tp;
import dm.Opt;
import dm.Dt;
import I18n._;
import I18n._args;

/// Accounting data.
class Acc {
  /// Ledger groups.
  public var groups(default, null) = [
    "1" => "Financiación básica",
    "2" => "Inmovilizado",
    "3" => "Existencias",
    "4" => "Acreedores y deudores",
    "5" => "Cuentas financieras",
    "6" => "Compras y gastos",
    "7" => "Ventas e ingresos",
    "8" => "Gastos del patrimonio neto",
    "9" => "Ingresos del patrimonio neto"
  ];

  /// Ledger subgroups
  public var subgroups(default, null) = [
    Cts.cash.substring(0, 2) => "Tesorería",
    Cts.capital.substring(0, 2) => "Capital",
    Cts.results.substring(0, 2) => "Resultados pendientes de aplicación"
  ];

  /// Ledger subgroups
  public var accounts(default, null): Map<String, AccValue> = [
    Cts.cash.substring(0, 3) =>
      new AccValue("Bancos, cuentas de ahorro, euros", "BABVI"),
    Cts.capital.substring(0, 3) =>
      new AccValue("Capital", "BPAI"),
    Cts.results.substring(0, 3) =>
      new AccValue("Resultados del ejercicio", "BPAVII")
  ];

  /// Ledger subgroups
  public var subaccounts(default, null): Map<String, String> = [
    Cts.cash => "Bankia. Cta.",
    Cts.capital => "Capital",
    Cts.results => "Resultados del ejercicio"
  ];

  /// Diary
  public var diary(default, null): Array<DiaryEntry> = [];

  function new () {}

  // Subgroups functions -------------------------

  /// Add a subgroup.
  ///   id         : Identifier with 2 digits. It must be valid.
  ///   description: Subgroup description.
  public function subgroupAdd (id: String, description: String): String {
    if (subgroups.exists(id))
      return _args(_("Subgroup '%0' is duplicated"), [id]);
    subgroups.set(id, description);
    return "";
  }

  /// Returns "" if 'id' is deletable (no annotations afected). Otherwise
  /// return a error message pointing the number of annotations afected.
  ///   id         : Identifier with 2 digits.
  public function subgroupDeletable (id: String): String {
    for (k in sub(id).keys()) {
      final error = accountDeletable(k);
      if (error != "") {
        return error;
      }
    }
    return "";
  }

  /// Delete a subgroup and all its accounts. It does not modify the Diary.
  public function subgroupDel (id: String) {
    for (k => _ in sub(id)) accountDel(k);
    subgroups.remove(id);
  }

  /// Modifies a subgroup.
  ///   oldId      : Old identifier with 2 digits.
  ///   newId      : New identifier with 2 digits. It must be valid.
  ///   description: Subgroup description.
  public function subgroupMod (
    oldId: String, newId: String, description: String
  ): String {
    if (oldId == newId) {
      subgroups.set(newId, description);
      return "";
    }

    if (subgroups.exists(newId))
      return _args(_("Subgroup '%0' is duplicated"), [newId]);

    final lg = newId.length;
    for (k => v in sub(oldId))
      accountMod(k, newId + k.substring(lg), v.description, v.summary);
    subgroups.remove(oldId);
    subgroups.set(newId, description);
    return "";
  }

  // Accounts functions -----------------------

  /// Add an account.
  ///   id         : Identifier with 3 digits. It must be valid.
  ///   description: Account description.
  ///   sumary     : Summary code (for balance and profits)
  public function accountAdd (
    id: String, description: String, summary: String
  ): String {
    if (accounts.exists(id))
      return _args(_("Account '%0' is duplicated"), [id]);
    accounts.set(id, new AccValue(description, summary));
    return "";
  }

  /// Returns "" if 'id' is deletable (no annotations afected). Otherwise
  /// return a error message pointing the number of annotations afected.
  ///   id         : Identifier with 3 digits. It must be valid.
  public function accountDeletable (id: String): String {
    for (k in sub(id).keys()) {
      final error = subaccountDeletable(k);
      if (error != "") {
        return error;
      }
    }
    return "";
  }

  /// Delete an account and all its subaccounts. It does not modify the Diary.
  public function accountDel (id: String) {
    for (k => _ in sub(id)) subaccountDel(k);
    accounts.remove(id);
  }

  /// Modifies a account.
  ///   oldId      : Old identifier with 3 digits.
  ///   newId      : New identifier with 3 digits. It must be valid.
  ///   description: Account description.
  ///   sumary     : Summary code (for balance and profits)
  public function accountMod (
    oldId: String, newId: String, description: String, summary: String
  ): String {
    if (oldId == newId) {
      accounts.set(newId, new AccValue(description, summary));
      return "";
    }

    if (accounts.exists(newId))
      return _args(_("Account '%0' is duplicated"), [newId]);

    final lg = newId.length;
    for (k => v in sub(oldId))
      subaccountMod(k, newId + k.substring(lg), v.description);
    accounts.remove(oldId);
    accounts.set(newId,  new AccValue(description, summary));
    return "";
  }

  // Subaccounts functions -----------------------

  /// Add a subaccount.
  ///   id         : Identifier with 5 digits. It must be valid.
  ///   description: Subaccount description.
  public function subaccountAdd (id: String, description: String): String {
    if (subaccounts.exists(id))
      return _args(_("Subaccount '%0' is duplicated"), [id]);
    subaccounts.set(id, description);
    return "";
  }

  /// Returns "" if 'id' is deletable (no annotations afected). Otherwise
  /// return a error message pointing the number of annotations afected.
  ///   id         : Identifier with 5 digits. It must be valid.
  public function subaccountDeletable (id: String): String {
    final n = usedSubaccounts(id).length;
    if (n != 0) {
      return _args(_("Subaccount '%0' can not be removed.\n"), [id]) +
        _args(_("It is used in %0 annotations."), [Std.string(n)]);
    }
    return "";
  }

  /// Delete a subaccount. It does not modify the Diary.
  public function subaccountDel (id: String) {
    subaccounts.remove(id);
  }

  /// Modifies a subaccount.
  ///   oldId      : Old identifier with 5 digits.
  ///   newId      : New identifier with 5 digits. It must be valid.
  ///   description: Subaccount description.
  public function subaccountMod (
    oldId: String, newId: String, description: String
  ): String {
    if (oldId == newId) {
      subaccounts.set(newId, description);
      return "";
    }

    if (subaccounts.exists(newId))
      return _args(_("Subaccount '%0' is duplicated"), [newId]);

    for (i in usedSubaccounts(oldId)) {
      final e = diary[i];
      if (e.debits.exists(oldId)) {
        final v = e.debits.get(oldId);
        e.debits.remove(oldId);
        e.debits.set(newId, v);
      }
      if (e.credits.exists(oldId)) {
        final v = e.debits.get(oldId);
        e.debits.remove(oldId);
        e.debits.set(newId, v);
      }
    }
    subaccounts.remove(oldId);
    subaccounts.set(newId, description);
    return "";
  }

  // Common --------------------------------------

  /// Returns the description of a group, subgroup, account or subaccount.
  /// If 'acc' is not found, returns "".
  ///   acc: Id of a group, subgroup, account or subaccount
  public function descriptionOf (acc: String): String {
    final lg = acc.length;
    if (lg == 3) {
      return accounts.exists(acc) ? accounts.get(acc).description : "";
    } else {
      final m = lg == 1 ? groups
        : lg == 2 ? subgroups
        : subaccounts
      ;
      return m.exists(acc) ? m.get(acc): "";
    }
  }

  function sub0 (acc: String): It<Tp<String, AccValue>> {
    function mapTo(m: Map<String, String>): It<Tp<String, AccValue>> {
      return It.fromMap(m).map(tp -> new Tp(tp.e1, new AccValue(tp.e2)));
    }
    final lg = acc.length;
    final itM = lg == 0 ? mapTo(groups)
      : lg == 1 ? mapTo(subgroups)
      : lg == 3 ? mapTo(subaccounts)
      : It.fromMap(accounts)
    ;
    return itM.filter(tp -> tp.e1.startsWith(acc));
  }

  /// Returns groups, subgroups, accounts or subaccounts which start with 'acc'.
  /// NOTE: AccValue.summary is "", but when the map is of accounts.
  ///   acc: "" or 'id' of a group, subgroup or account
  public function sub (acc: String): Map<String, AccValue> {
    return Opt.get(sub0(acc).toMap());
  }

  /// Returns groups, subgroups, accounts or subaccounts which start with
  /// 'acc' and have a subaccount created.
  /// NOTE: AccValue.summary is "", but when the map is of accounts.
  ///   acc: "" or 'id' of a group, subgroup or account
  public function subOf (acc: String): Map<String, AccValue>  {
    return Opt.get(
      sub0(acc).filter(a ->
        It.fromMap(subaccounts).some(sa -> sa.e1.startsWith(a.e1))
      ).toMap());
  }

  /// Returns the idenfitiers available for a group, subgroup or account.
  ///   acc  : Identifier of group, subgroup or account (1, 2 or 3 digits).
  ///   extra: Identifier to add to result. If its value is "", no identifier
  ///          will be added.
  public function available (acc: String, extra: String): Array<String> {
    final subs = sub(acc);
    final it = acc.length == 3
      ? It.range(26).map(i -> i < 10 ? "0" + Std.string(i) : Std.string(i))
      : It.range(10).map(i -> Std.string(i))
    ;
    final r = it.filter(id -> !subs.exists(acc + id)).to();
    if (extra != "") r.push(extra);
    r.sort((i1, i2) -> i1 > i2 ? 1 : -1);
    return r;
  }

  // Diary ---------------------------------------

  /// Returns the index in Diary of entries which use 'acc'
  ///   acc  : Identifier of group, subgroup, account or subaccount
  ///         (1, 2, 3 ir 5 digits).
  public function usedAccs (acc: String): Array<Int> {
    final len = acc.length;
    final r = [];
    It.from(diary).eachIx((e, i) ->
      if (
        It.fromMap(e.debits).some(tp -> tp.e1.substring(0, len) == acc) ||
        It.fromMap(e.credits).some(tp -> tp.e1.substring(0, len) == acc)
      ) {
        r.push(i);
      }
    );
    return r;
  }

  /// Returns the index in Diary of entries which use 'subacc'
  ///   subacc: Subaccount code.
  public function usedSubaccounts (subacc: String): Array<Int> {
    final r = [];
    It.from(diary).eachIx((e, i) ->
      if (e.debits.exists(subacc) || e.credits.exists(subacc)) r.push(i)
    );
    return r;
  }

  /// Returns the most used subaccounts (Tp[account, description]).
  ///   forCash: If it is 'true', only searh in accounts used with the main cash
  ///            (Bankia).
  public function mostUsedSubaccounts (forCash: Bool): It<Tp<String, String>> {
    final accs: Map<String, Int> = [];
    if (forCash) {
      for (i in usedSubaccounts(Cts.cash)) {
        final e = diary[i];
        for (k => v in e.debits) {
          if (k == Cts.cash) continue;
          if (accs.exists(k)) accs.set(k, accs.get(k) + 1);
          else accs.set(k, 1);
        }
        for (k => v in e.credits) {
          if (k == Cts.cash) continue;
          if (accs.exists(k)) accs.set(k, accs.get(k) + 1);
          else accs.set(k, 1);
        }
      }
    } else {
      for (e in diary) {
        for (k => v in e.debits) {
          if (accs.exists(k)) accs.set(k, accs.get(k) + 1);
          else accs.set(k, 1);
        }
        for (k => v in e.credits) {
          if (accs.exists(k)) accs.set(k, accs.get(k) + 1);
          else accs.set(k, 1);
        }
      }
    }

    return It.fromMap(accs)
      .sort((tp1, tp2) -> tp2.e2 - tp1.e2)
      .take(Cts.mostUsedLen)
      .map(tp -> new Tp(tp.e1, subaccounts.get(tp.e1)))
    ;
  }

  /// Add an entry to Diary.
  public function addDiary (entry: DiaryEntry): Int {
    final date = entry.date;
    final r: Array<DiaryEntry> = [];
    var ix = 0;
    var rIx = -1;
    for (e in diary) {
      if (rIx == -1 && Dt.df(e.date, date) > 0) {
        r.push(entry);
        rIx = ix;
      }
      r.push(e);
      ++ix;
    }
    if (rIx == -1) {
      r.push(entry);
      rIx = ix;
    }
    diary = r;
    return rIx;
  }

  /// Changes diary for open annotation.
  public function close (newYear: Int): Void {
    final accs = new Map<String, Cu>();
    function add(acc: String, amm: Cu) {
      if (accs.exists(acc)) accs[acc] = accs[acc].add(amm);
      else accs[acc] = amm;
    }
    var sum = 0.0;
    for (e in diary) {
      It.fromMap(e.debits)
        .filter(t -> accounts[t.e1.substring(0, 3)].summary.charAt(0) == "B")
        .each(t -> {
          final v = t.e2;
          sum += v.value;
          add(t.e1, v);
        });
      It.fromMap(e.credits)
        .filter(t -> accounts[t.e1.substring(0, 3)].summary.charAt(0) == "B")
        .each(t -> {
          final v = t.e2.negate();
          sum += v.value;
          add(t.e1, v);
        });
    }
    add("10200", new Cu(-sum));

    diary = [new DiaryEntry(
      Dt.mk(1, 1, newYear),
      "Asiento de apertura",
      Opt.get(It.fromMap(accs)
        .filter(t -> t.e2.value > 0)
        .toMap()),
      Opt.get(It.fromMap(accs)
        .filter(t -> t.e2.value < 0)
        .map(t -> new Tp(t.e1, t.e2.negate()))
        .toMap())
    )];
  }

  // JSON ----------------------------------------

  /// Serialize to JSON.
  public function toJs (): Js {
    return Js.wa([
      Js.wMap(subgroups, Js.ws),
      Js.wMap(accounts, e -> e.toJs()),
      Js.wMap(subaccounts, Js.ws),
      Js.wArray(diary, e -> e.toJs())
    ]);
  }

  /// Restore a JSON serialization.
  public static function fromJs (js: Js): Acc {
    final r = new Acc();
    final a = js.ra();
    if (a.length > 0) {
      r.subgroups = a[0].rMap(j -> j.rs());
      r.accounts = a[1].rMap(j -> AccValue.fromJs(j));
      r.subaccounts = a[2].rMap(j -> j.rs());
      r.diary = a[3].rArray(DiaryEntry.fromJs);
    }
    return r;
  }

}

// Copyright 24-Sep-2017 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

goog.provide("Db");

Db = class {
  /**
   * @param {!Array<!Array<string>>} subgroups
   * @param {!Array<!Array<string>>} accounts
   * @param {!Array<!Array<string>>} subaccounts
   * @param {!Array<!db_Dentry>} diary
   */
  constructor (subgroups, accounts, subaccounts, diary) {
    /** @private */
    this._subgroups = subgroups;
    /** @private */
    this._accounts = accounts;
    /** @private */
    this._subaccounts = subaccounts;
    /** @private */
    this._diary = diary;
  }

  /**
   * Fields:
   *   id: 2 digits
   *   description
   * @return {!Array<!Array<string>>}
   */
  static groups () {
    return [
      ["1", "Financiación básica"],
      ["2", "Inmovilizado"],
      ["3", "Existencias"],
      ["4", "Acreedores y deudores"],
      ["5", "Cuentas financieras"],
      ["6", "Compras y gastos"],
      ["7", "Ventas e ingresos"],
      ["8", "Gastos del patrimonio neto"],
      ["9", "Ingresos del patrimonio neto"]
    ];
  }

  /**
   * @param {string} id
   * @return {string}
   */
  static groupsGet (id) {
    const gs = Db.groups();
    for (let i = 0; i < gs.length; ++i) {
      const g = gs[i];
      if (g[0] === id) {
        return g[1];
      }
    }
    throw ("Group " + id + " is missing");
  }

  /**
   * Fields:
   *   id: 2 digits
   *   description
   * @return {!Array<!Array<string>>}
   */
  subgroups () {
    return this._subgroups;
  }

  /**
   * @param {string} id
   * @param {string} description
   * @return {void}
   */
  subgroupsAdd (id, description) {
    this._subgroups.push([id, description]);
  }

  /**
   * @param {string} id
   * @return {void}
   */
  subgroupsDel (id) {
    this.accountsDel(id);
    this._subgroups = It.from(this._subgroups).filter(e => e[0] !== id).to();
  }

  /**
   * @param {string} modifyId
   * @param {string} id
   * @param {string} description
   * @return {void}
   */
  subgroupsMod (modifyId, id, description) {
    if (modifyId !== id) {
      this.accountsMod(modifyId, id);
    }
    this._subgroups = It.from(this._subgroups)
      .map(e => e[0] === modifyId ? [id, description] : e).to();
  }

  /**
   * Fields:
   *   id: 3 digits
   *   description
   *   summary: Place in Balance or PyG
   * @return {!Array<!Array<string>>}
   */
  accounts () {
    return this._accounts;
  }

  /**
   * @param {string} id
   * @return {!Array<string>} 0 = description, 1 = summary
   */
  accountsGet (id) {
    const accs = this._accounts;
    for (let i = 0; i < accs.length; ++i) {
      const acc = accs[i];
      if (acc[0] === id) {
        return [acc[1], acc[2]];
      }
    }
    throw ("Account " + id + " is missing");
  }

  /**
   * @param {string} id
   * @param {string} description
   * @param {string} summary
   * @return {void}
   */
  accountsAdd (id, description, summary) {
    this._accounts.push([id, description, summary]);
  }

  /**
   * @param {string} id
   * @return {void}
   */
  accountsDel (id) {
    this.subaccountsDel(id);
    this._accounts = It.from(this._accounts)
      .filter(e => !e[0].startsWith(id)).to();
  }

  /**
   * @param {string} modifyId
   * @param {string} id
   * @param {string=} description
   * @param {string=} summary
   * @return {void}
   */
  accountsMod(modifyId, id, description, summary) {
    if (modifyId !== id) {
      this.subaccountsMod(modifyId, id);
    }
    this._accounts = It.from(this._accounts)
      .map(e => e[0].startsWith(modifyId)
        ? description === undefined
          ? [id + e[0].substring(id.length), e[1], e[2]]
          : [id + e[0].substring(id.length), description, summary]
        : e
      ).to();
  }

  /**
   * Fields:
   *   id: 5 digits
   *   description
   * @return {!Array<!Array<string>>}
   */
  subaccounts () {
    return this._subaccounts;
  }

  /**
   * @param {string} id
   * @return {string}
   */
  static subaccountsGet (id) {
    const subs = this._subaccounts;
    for (let i = 0; i < subs.length; ++i) {
      const sub = subs[i];
      if (sub[0] === id) {
        return sub[1];
      }
    }
    throw ("Subaccount " + id + " is missing");
  }

  /**
   * @param {string} id
   * @param {string} description
   * @return {void}
   */
  subaccountsAdd (id, description) {
    this._subaccounts.push([id, description]);
  }

  /**
   * @param {string} id
   * @return {void}
   */
  subaccountsDel (id) {
    this._subaccounts = It.from(this._subaccounts)
      .filter(e => !e[0].startsWith(id)).to();
  }

  /**
   * @param {string} modifyId
   * @param {string} id
   * @param {string=} description
   * @return {void}
   */
  subaccountsMod (modifyId, id, description) {
    this._subaccounts = It.from(this._subaccounts)
      .map(e => e[0].startsWith(modifyId)
        ? [
            id + e[0].substring(id.length),
            description === undefined ? e[1] : description
          ]
        : e
      ).to();
    if (modifyId !== id) {
      this.planChangeAcc(modifyId, id);
    }
  }

  /**
   * @param {string} acc Id of a group, subgroup, account or subaccount
   * @return {string}
   */
  description (acc) {
    const lg = acc.length;
    const it = lg === 1 ? It.from(Db.groups())
      : lg === 2 ? It.from(this._subgroups)
      : lg === 3 ? It.from(this._accounts)
      : It.from(this._subaccounts);
    const r = it.find(e => e[0] === acc);
    return r.length === 0 ? "" : r[0][1];
  }

  /**
   * Returns groups, subgroups, accounts or subaccounts which start with 'acc'
   * @param {string} acc "" or an 'id' of subgroup or account
   * @return {!It<!Array<string>>}
   */
  sub (acc) {
    const lg = acc.length;
    return (lg === 0 ? It.from(Db.groups())
      : lg === 1 ? It.from(this._subgroups)
      : lg === 2 ? It.from(this._accounts)
      : It.from(this._subaccounts)).filter(s => s[0].startsWith(acc));
  }

  /**
   * Returns groups, subgroups, accounts or subaccounts which start with
   * 'acc' and have a subaccount created.
   * @param {string} acc "" or an 'id' of subgroup or account
   * @return {!It<!Array<string>>}
   */
  subOf (acc) {
    const self = this;
    return self.sub(acc).filter(
      s1 => It.from(self._subaccounts).containsf(s2 => s2[0].startsWith(s1[0]))
    );
  }

  /** @return {!Array<!db_Dentry>} */
  diary () {
    return this._diary;
  }

  /**
   * @param {!Array<!db_Dentry>} d New diary data. Used when closing year.
   * @return {void}
   */
  setDiary (d) {
    this._diary = d;
  }

  /**
   * @param {!db_Dentry} entry
   * @return {number}
   */
  diaryAdd (entry) {
    this._diary = It.from(this._diary)
      .takeUntil(e => e.date().compare(entry.date()) > 0)
      .add(entry)
      .addIt(It.from(this._diary)
        .dropUntil(e => e.date().compare(entry.date()) > 0)
      ).to();
    return It.from(this._diary)
      .takeUntil(e => e.date().compare(entry.date()) > 0)
      .size();
  }

  /**
   * @param {number} ix Number of annotations (its order number is ix - 1)
   * @return {void}
   */
  diaryDel (ix) {
    this._diary = It.from(this._diary)
      .take(ix - 1)
      .addIt(It.from(this._diary).drop(ix))
      .to();
  }

  /**
   * @param {number} ix Number of annotations (its order number is ix - 1)
   * @param {!db_Dentry} entry
   * @return {void}
   */
  diaryModify (ix, entry) {
    this._diary[ix - 1] = entry;
  }

  /**
   * Returns annotations number with entry plan 'entryPlan'
   * @param {string} entryPlan
   * @return {number}
   */
  planAnnotations (entryPlan) {
    return It.from(this._diary).reduce(0, (s, e) =>
      It.from(e.debits()).containsf(d => d.e1().startsWith(entryPlan)) ||
      It.from(e.credits()).containsf(c => c.e1().startsWith(entryPlan))
        ? s + 1 : s
    );
  }

  /**
   * Changes an account/group id by other
   * @param {string} id
   * @param {string} newId
   * @return {void}
   */
  planChangeAcc (id, newId) {
    It.from(this._diary).each(e => {
      It.from(e.debits()).each(d => {
        if (d.e1().startsWith(id)) {
          d.setE1(newId + d.e1().substring(id.length));
        }
      });
      It.from(e.credits()).each(c => {
        if (c.e1().startsWith(id)) {
          c.setE1(newId + c.e1().substring(id.length));
        }
      });
    });
  }

  /** @return {!Array<string>}*/
  mostUsed () {
    /** @const {!Object<string, number>} */
    const accs = {};
    /** @const {function(string):void} */
    const add = s => {
      const acc = accs[s];
      accs[s] = acc ? acc + 1 : 1;
    };
    It.from(this._diary).each(e => {
      It.from(e.debits()).each(d => { add(d.e1()); });
      It.from(e.credits()).each(c => { add(c.e1()); });
    });

    return It.keys(accs).sortf((a1, a2) => accs[a1] < accs[a2] ? 1 : -1)
      .take(15).to();
  }

  /**
   * param {number} year
   * @return {db_Dentry} Open annotation for next year
   */
  close (year) {
    const self = this;
    const accs = {};
    function add(acc, amm) {
      const a = accs[acc] || 0;
      accs[acc] = a + amm;
    }
    let sum = 0;
    It.from(this._diary).each(e => {
      It.from(e.debits())
        .filter(d =>
          self.accountsGet(d.e1().substring(0, 3))[1].charAt(0) === "B")
        .each(d => {
          const v = d.e2().value();
          sum += v;
          add(d.e1(), v);
        });
      It.from(e.credits())
        .filter(c =>
          self.accountsGet(c.e1().substring(0, 3))[1].charAt(0) === "B")
        .each(c => {
          const v = c.e2().value();
          sum -= v;
          add(c.e1(), -v);
        });
    });
    add("10200", -sum);

    return new db_Dentry(
      new DateDm(1, 1, year + 1),
      "Asiento de apertura",
      It.keys(accs).filter(k => accs[k] > 0)
        .map(k => new Tp(k, new Dec(accs[k], 2))).to(),
      It.keys(accs).filter(k => accs[k] < 0)
        .map(k => new Tp(k, new Dec(-accs[k], 2))).to()
    );
  }

  /** @return {string} */
  serialize () {
    return JSON.stringify([
      this._subgroups,
      this._accounts,
      this._subaccounts,
      It.from(this._diary).map(e => e.serialize()).to()
    ]);
  }

  /**
   * @param {string} serial
   * @return {!Db}
   */
  static restore (serial) {
    if (serial === "") {
      const db = new Db([], [], [], []);
      db.subgroupsAdd("57", "Tesorería");
      db.accountsAdd("572", "Bancos, cuentas de ahorro, euros", "BABVI");
      db.subaccountsAdd("57200", "Bankia");
      db.subgroupsAdd("10", "Capital");
      db.accountsAdd("102", "Capital", "BPAI");
      db.subaccountsAdd("10200", "Capital");
      db.subgroupsAdd("12", "Resultados pendientes de aplicación");
      db.accountsAdd("120", "Resultados del ejercicio", "BPAVII");
      db.subaccountsAdd("12000", "Resultados del ejercicio");
      return db;
    }
    const pars = /** @type {!Array<?>} */(JSON.parse(serial));
    return new Db (
      pars[0],
      pars[1],
      pars[2],
      It.from(pars[3]).map(e => db_Dentry.restore(e)).to()
    );
  }
}

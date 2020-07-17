// Copyright 05-Jul-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

using StringTools;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Tp;
import dm.Tp3;
import dm.It;
import dm.Opt;
import dm.Menu;
import data.All;
import data.Cu;
import data.DiaryEntry;
import data.Profits;
import data.Balance;
import I18n._;

/// Summary page.
class Summary {
  /// Constructor.
  ///   wg: Widget.
  ///   data: Accounting data.
  ///   type: Summary type: S, P, B (Statements, Profit & losses, Balance).
  ///   deep: Summary deep: M, A, S (suMmary, Account, Subaccount).
  public static function mk (
    wg: Domo, data: All, type: String, deep: String
  ): Void {
    if (type != "S" && type != "P" && type != "B") type = "P";
    if (deep != "M" && deep != "A" && deep != "S") deep = "M";


    function blanks (tx, n) {
      var bs = "";
      for (i in 0...n) bs += "&nbsp;";
      return bs + tx + bs;
    }

    function normal (tx) {
      return tx;
    }

    function bold (tx) {
      return '<b>$tx</b>';
    }

    function italic (tx) {
      return '<i><b>$tx</b></i>';
    }

    function underline (tx) {
      return '<u><i><b>$tx</b></i></u>';
    }

    final ropts1 = [
      Menu.toption("S", _("Statements"), () -> mk(wg, data, "S", deep)),
      Menu.separator(),
      Menu.toption("P", blanks(_("P & L"), 6), () -> mk(wg, data, "P", deep)),
      Menu.separator(),
      Menu.toption("B", _("Balance"), () -> mk(wg, data, "B", deep)),
      Menu.separator2()
    ];
    final menu1 = new Menu([], ropts1, type);
    final lopts2 = [
      Menu.separator2(),
      Menu.toption("M", _("Summary"), () -> mk(wg, data, type, "M")),
      Menu.separator(),
      Menu.toption("A", _("Accounts"), () -> mk(wg, data, type, "A")),
      Menu.separator(),
      Menu.toption("S", _("Subaccounts"), () -> mk(wg, data, type, "S"))
    ];
    final menu2 = new Menu(lopts2, [], deep);

    // Model -------------------------------------------------------------------

    function calcSubStats (
      accs: Array<Tp<
        { id: String, desc: String, cu: Cu },
        Array<{ id: String, desc: String, cu: Cu }>
      >>,
      acc: String,
      sacc: String,
      cu: Cu
    ): Bool {
      var missing = true;
      for (a in accs) {
        if (a.e1.id == acc) {
          var missing3 = true;
          final saccs = a.e2;
          for (sa in saccs) {
            if (sa.id == sacc) {
              sa.cu = sa.cu.add(cu);
              missing3 = false;
              break;
            }
          }
          if (missing3) {
            saccs.push(
              {id: sacc, desc: data.acc.descriptionOf(sacc), cu: cu}
            );
          }
          a.e1.cu = a.e1.cu.add(cu);
          missing = false;
          break;
        }
      }
      return missing;
    }

    function calcStats (diary: Array<DiaryEntry>) {
      // [{2 {3 id, des, val}, [{2 {3 id, des, val} , [{3 id, des, val}]}]}]}
      final stats: Array<Tp<
        { id: String, desc: String, cu: Cu },
        Array<Tp<
          { id: String, desc: String, cu: Cu },
          Array<{ id: String, desc: String, cu: Cu }>
        >>
      >> = [];

      for (e in diary) {
        function process(sacc: String, cu: Cu): Void {
          final gr = sacc.charAt(0);
          final acc = sacc.substring(0, 3);
          var missing = true;
          for (g in stats) {
            if (g.e1.id == gr) {
              var missing2 = calcSubStats(g.e2, acc, sacc, cu);
              if (missing2) {
                final s3 = {id: sacc, desc: data.acc.descriptionOf(sacc), cu: cu};
                g.e2.push(new Tp(
                  {id: acc, desc: data.acc.descriptionOf(acc), cu: cu},
                  [s3]
                ));
              }
              g.e1.cu = g.e1.cu.add(cu);
              missing = false;
              break;
            }
          }
          if (missing) {
            final s3 = {id: sacc, desc: data.acc.descriptionOf(sacc), cu: cu};
            final s2 = new Tp(
              {id: acc, desc: data.acc.descriptionOf(acc), cu: cu},
              [s3]
            );
            stats.push(new Tp(
              {id: gr, desc: data.acc.descriptionOf(gr), cu: cu},
              [s2]
            ));
          }
        }

        It.fromMap(e.debits).each(t -> process(t.e1, t.e2));
        It.fromMap(e.credits).each(t -> process(t.e1, t.e2.negate()));
      }
      for (g in stats) {
        for (a in g.e2) {
          a.e2.sort((v1, v2) -> v1.id > v2.id ? 1 : -1);
        }
        g.e2.sort((v1, v2) -> v1.e1.id > v2.e1.id ? 1 : -1);
      }
      stats.sort((v1, v2) -> v1.e1.id > v2.e1.id ? 1 : -1);
      return stats;
    }

    function calcPl () {
      // {2 {3 id, des, val},
      //    [{2 {3 id, des, val}, [{2 {3 id, des, val} , [{3 id, des, val}]}]}]}
      // }
      final pls: Array<Tp<
        { id: String, desc: String, cu: Cu },
        Array<Tp<
          { id: String, desc: String, cu: Cu },
          Array<Tp<
            { id: String, desc: String, cu: Cu },
            Array<{ id: String, desc: String, cu: Cu }>
          >>
        >>
      >> = [];

      final accs = data.acc.accounts;
      function filter (nm, smm: String) {
        return smm.startsWith("P") && smm.substring(1) == nm;
      }
      function diary (nm) {
        return data.acc.diary.map(e ->
          new DiaryEntry(
            e.date, e.description,
            Opt.get(It.fromMap(e.debits)
              .filter(t -> filter(nm, accs.get(t.e1.substring(0, 3)).summary))
              .toMap()),
            Opt.get(It.fromMap(e.credits)
              .filter(t -> filter(nm, accs.get(t.e1.substring(0, 3)).summary))
              .toMap())
          )
        );
      }

      var sumT = 0.0;
      final letters = ["A", "B", "C"];
      for (l in letters) {
        final stats = It.fromMap(Profits.entries())
          .filter(t -> Profits.groupOf(t.e1) == l)
          .sort((t1, t2) -> t1.e1 > t2.e1 ? 1 : -1)
          .map(t -> {
            var sum = 0.0;
            final accs: Array<Tp<
                { id: String, desc: String, cu: Cu },
                Array<{ id: String, desc: String, cu: Cu }>
              >> = [];

            for (e in diary(t.e1)) {
              function process(sacc: String, cu: Cu) {
                final acc = sacc.substring(0, 3);
                final missing = calcSubStats(accs, acc, sacc, cu);
                if (missing) {
                  accs.push(new Tp(
                    {id: acc, desc: data.acc.descriptionOf(acc), cu: cu},
                    [{id: sacc, desc: data.acc.descriptionOf(sacc), cu: cu}]
                  ));
                }
                sum += cu.value;
              }

              It.fromMap(e.debits).each(t -> process(t.e1, t.e2));
              It.fromMap(e.credits).each(t -> process(t.e1, t.e2.negate()));
            }

            return new Tp(
              {id: t.e1, desc: t.e2, cu: new Cu(sum)},
              accs
            );
          }).to()
        ;

        for (g in stats) {
          for (a in g.e2) {
            a.e2.sort((v1, v2) -> v1.id > v2.id ? 1 : -1);
          }
          g.e2.sort((v1, v2) -> v1.e1.id > v2.e1.id ? 1 : -1);
        }
        final sum = It.from(stats).reduce(0.0, (r, t) -> r + t.e1.cu.value);
        pls.push(new Tp(
          {id: l, desc: Profits.groups().get(l), cu: new Cu(sum)},
          stats
        ));
        sumT += sum;
      }
      pls.push(new Tp(
        {id: "D", desc: Profits.groups().get("D"), cu: new Cu(sumT)},
        []
      ));

      return pls;
    }

    function calcBalance () {
      // {2 {3 id, des, val},
      //    [{2 {3 id, des, val}, [{2 {3 id, des, val} , [{3 id, des, val}]}]}]}
      // }
      final bal: Array<Tp<
        { id: String, desc: String, cu: Cu },
        Array<Tp<
          { id: String, desc: String, cu: Cu },
          Array<Tp<
            { id: String, desc: String, cu: Cu },
            Array<{ id: String, desc: String, cu: Cu }>
          >>
        >>
      >> = [];

      final accs = data.acc.accounts;
      function filter (nm, smm: String) {
        return smm.startsWith("B") && smm.substring(1) == nm;
      }
      function diary (nm) {
        return data.acc.diary.map(e ->
          new DiaryEntry(
            e.date, e.description,
            Opt.get(It.fromMap(e.debits)
              .filter(t -> filter(nm, accs.get(t.e1.substring(0, 3)).summary))
              .toMap()),
            Opt.get(It.fromMap(e.credits)
              .filter(t -> filter(nm, accs.get(t.e1.substring(0, 3)).summary))
              .toMap())
          )
        );
      }

      var sumT = 0.0;
      final letters = ["AA", "AB", "PA", "PB", "PC"];
      for (l in letters) {
        final stats = It.fromMap(Balance.entries())
          .filter(t -> Balance.groupOf(t.e1) == l)
          .sort((t1, t2) -> t1.e1 > t2.e1 ? 1 : -1)
          .map(t -> {
            var sum = 0.0;
            final accs: Array<Tp<
                { id: String, desc: String, cu: Cu },
                Array<{ id: String, desc: String, cu: Cu }>
              >> = [];

            for (e in diary(t.e1)) {
              function process(sacc: String, cu: Cu) {
                final acc = sacc.substring(0, 3);
                final missing = calcSubStats(accs, acc, sacc, cu);
                if (missing) {
                  accs.push(new Tp(
                    {id: acc, desc: data.acc.descriptionOf(acc), cu: cu},
                    [{id: sacc, desc: data.acc.descriptionOf(sacc), cu: cu}]
                  ));
                }
                sum += cu.value;
              }

              It.fromMap(e.debits).each(t -> process(t.e1, t.e2));
              It.fromMap(e.credits).each(t -> process(t.e1, t.e2.negate()));
            }

            return new Tp(
              {id: t.e1, desc: t.e2, cu: new Cu(sum)},
              accs
            );
          }).to()
        ;

        for (g in stats) {
          for (a in g.e2) {
            a.e2.sort((v1, v2) -> v1.id > v2.id ? 1 : -1);
          }
          g.e2.sort((v1, v2) -> v1.e1.id > v2.e1.id ? 1 : -1);
        }
        final sum = It.from(stats).reduce(0.0, (r, t) -> r + t.e1.cu.value);
        bal.push(new Tp(
          {id: l, desc: Balance.groups().get(l), cu: new Cu(sum)},
          stats
        ));
        sumT += sum;
      }

      return bal;
    }

    // View --------------------------------------------------------------------

    function fS (level: Int): Domo {
      final stats = calcStats(data.acc.diary);
      final table = Q("table")
        .klass("summary")
        .att("align", "center")
      ;
      for (g in stats) {
        final v = g.e1;
        final desc = v.desc;
        final cu = v.cu;
        final fmt = level == 0 ? normal : level == 1 ? bold : italic;
        table.add(Q("tr")
          .add(Q("td").klass("summary0cp")
            .att("colspan", 3)
            .add(Ui.link(_ ->
              js.Browser.location.assign('?accs&${v.id}')
            )
              .klass("link")
              .html(fmt(v.id + ". " + desc))))
          .add(Q("td").klass("summary0d")
            .html(cu.value > 0 ? fmt(cu.toString()) : ""))
          .add(Q("td").klass("summary0c")
            .html(cu.value < 0 ? fmt(cu.abs().toString()) : "")));
        if (level == 0) continue;

        for (a in g.e2) {
          final v = a.e1;
          final desc = v.desc;
          final cu = v.cu;
          final fmt = level == 1 ? normal : bold;
          table.add(Q("tr")
            .add(Q("td").style("width:40px"))
            .add(Q("td").klass("summary0cp")
              .att("colspan", 2)
              .add(Ui.link(_ ->
                js.Browser.location.assign('?accs&${v.id}')
              )
                .klass("link")
                .html(fmt(v.id + ". " + desc))))
            .add(Q("td").klass("summary0d")
              .html(cu.value > 0 ? fmt(cu.toString()) : ""))
            .add(Q("td").klass("summary0c")
              .html(cu.value < 0 ? fmt(cu.abs().toString()) : "")));

          if (level == 1) continue;
          for (v in a.e2) {
            final desc = v.desc;
            final cu = v.cu;
            final fmt = level == 1 ? normal : bold;
            table.add(Q("tr")
              .add(Q("td").style("width:40px"))
              .add(Q("td").style("width:40px"))
              .add(Q("td").klass("summary0cp")
                .add(Ui.link(_ ->
                  js.Browser.location.assign('?accs&${v.id}')
                )
                  .klass("link")
                  .html(v.id + ". " + desc)))
              .add(Q("td").klass("summary0d")
                .html(cu.value > 0 ? cu.toString() : ""))
              .add(Q("td").klass("summary0c")
                .html(cu.value < 0 ? cu.abs().toString() : "")));
          }
        }
      }
      return Q("div")
        .add(Q("div")
          .klass("head")
          .html(_("Statements") + "<br><i>" + _("Summary") + "</i>"))
        .add(table)
      ;
    }

    function fSM (): Domo {
      return fS(0);
    }

    function fSA (): Domo {
      return fS(1);
    }

    function fSS (): Domo {
      return fS(2);
    }

    function fPB (isP: Bool, level): Domo {
      final entries = isP ? calcPl() : calcBalance();
      final table = Q("table")
        .klass("summary")
        .att("align", "center")
      ;
      for (l in entries) {
        final v = l.e1;
        final desc = v.desc;
        final cu = v.cu;
        final fmt = level == 0 ? bold : level == 1 ? italic : underline;
        table.add(Q("tr")
          .add(Q("td").klass("summary0cp")
            .att("colspan", 4)
            .html(fmt(v.id + ". " + desc)))
          .add(Q("td").klass("summary0d")
            .html(cu.value > 0 ? fmt(cu.toString()) : ""))
          .add(Q("td").klass("summary0c")
            .html(cu.value < 0 ? fmt(cu.abs().toString()) : "")));

        for (g in l.e2) {
          final v = g.e1;
          final desc = v.desc;
          final cu = v.cu;
          final fmt = level == 0 ? normal : level == 1 ? bold : italic;
          table.add(Q("tr")
            .add(Q("td").style("width:40px"))
            .add(Q("td").klass("summary0cp")
              .att("colspan", 3)
              .html(fmt(v.id + ". " + desc)))
            .add(Q("td").klass("summary0d")
              .html(cu.value > 0 ? fmt(cu.toString()) : ""))
            .add(Q("td").klass("summary0c")
              .html(cu.value < 0 ? fmt(cu.abs().toString()) : "")));
          if (level == 0) continue;

          for (a in g.e2) {
            final v = a.e1;
            final desc = v.desc;
            final cu = v.cu;
            final fmt = level == 1 ? normal : bold;
            table.add(Q("tr")
              .add(Q("td").style("width:40px"))
              .add(Q("td").style("width:40px"))
              .add(Q("td").klass("summary0cp")
                .att("colspan", 2)
                .add(Ui.link(_ ->
                  js.Browser.location.assign('?accs&${v.id}')
                )
                  .klass("link")
                  .html(fmt(v.id + ". " + desc))))
              .add(Q("td").klass("summary0d")
                .html(cu.value > 0 ? fmt(cu.toString()) : ""))
              .add(Q("td").klass("summary0c")
                .html(cu.value < 0 ? fmt(cu.abs().toString()) : "")));

            if (level == 1) continue;
            for (v in a.e2) {
              final desc = v.desc;
              final cu = v.cu;
              final fmt = level == 1 ? normal : bold;
              table.add(Q("tr")
                .add(Q("td").style("width:40px"))
                .add(Q("td").style("width:40px"))
                .add(Q("td").style("width:40px"))
                .add(Q("td").klass("summary0cp")
                  .add(Ui.link(_ ->
                    js.Browser.location.assign('?accs&${v.id}')
                  )
                    .klass("link")
                    .html(v.id + ". " + desc)))
                .add(Q("td").klass("summary0d")
                  .html(cu.value > 0 ? cu.toString() : ""))
                .add(Q("td").klass("summary0c")
                  .html(cu.value < 0 ? cu.abs().toString() : "")));
            }
          }
        }
      }
      return Q("div")
        .add(Q("div")
          .klass("head")
          .html(_("Statements") + "<br><i>" + _("Summary") + "</i>"))
        .add(table)
      ;
    }

    function fPM (): Domo {
      return fPB(true, 0);
    }

    function fPA (): Domo {
      return fPB(true, 1);
    }

    function fPS (): Domo {
      return fPB(true, 2);
    }

    function fBM (): Domo {
      return fPB(false, 0);
    }

    function fBA (): Domo {
      return fPB(false, 1);
    }

    function fBS (): Domo {
      return fPB(false, 2);
    }

    function summary (): Domo {
      return switch (type) {
      case "S":
        switch (deep) {
        case "M": fSM();
        case "A": fSA();
        default : fSS();
        }
      case "P":
        switch (deep) {
        case "M": fPM();
        case "A": fPA();
        default : fPS();
        }
      default :
        switch (deep) {
        case "M": fBM();
        case "A": fBA();
        default : fBS();
        }
      }
    }

    wg
      .removeAll()
      .add(Q("div")
        .klass("head")
        .text(_("Summaries")))
      .add(Q("table")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td")
            .add(menu1.wg))
          .add(Q("td")
            .add(menu2.wg))))
      .add(summary())
    ;
  }
}

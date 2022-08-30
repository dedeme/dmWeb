// Copyright 31-Jul-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.wgs;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.It;
import dm.Opt;
import data.OperatorSet;
import data.Book;
import data.Step;
import data.Prop;
import I18n._;

/// Demonstration widget.
class DemoWg {
  final wg: Domo;
  final book: Book;
  final steps: Array<Step>;

  public function new (wg: Domo, book: Book, steps: Array<Step>) {
    this.wg = wg;
    this.book = book;
    this.steps = steps;

    show();
  }

  // View ----------------------------------------------------------------------

  public function show (): Void {
    function tdN (): Domo {
      return Q("td")
        .style(
          "width:5px;white-space:nowrap;" +
          "text-align:left;font-family:monospace;padding:0px;"
        )
      ;
    }
    function tdRs (): Domo {
      return Q("td")
        .style(
          "width:5px;white-space:nowrap;" +
          "text-align:left;font-family:monospace;padding:0px;"
        )
      ;
    }
    function tdPrTitle (): Domo {
      return Q("td")
        .style("width:800px;text-align:left;font-family:monospace;padding:0px;")
      ;
    }
    function tdPr (): Domo {
      return Q("td")
        .style("width:800px;text-align:left;padding:0px;")
      ;
    }
    function mkTab (level: Int): Domo {
      return Q("span")
        .style("font-family:monospace;")
        .html(It.range(level).reduce("", (r, e) -> r + "│&nbsp;"))
      ;
    }

    final tb = Q("table")
      .att("align", "center")
      .klass("flat")
      .add(Q("tr")
        .add(tdN()
          .text(_("No.")))
        .add(tdRs()
          .html("&nbsp;" + _("Reason") + "&nbsp;"))
        .add(tdPrTitle()
          .text(_("Proposition"))))
      .add(Q("tr")
        .add(tdN().add(Q("hr")))
        .add(tdRs().add(Q("hr")))
        .add(tdPr().add(Q("hr"))))
    ;

    if (steps.length == 0) {
      tb
        .add(Q("tr")
          .add(tdN().html("&nbsp;"))
          .add(tdRs())
          .add(tdPr()))
      ;
    } else {
      var tabs: Array<Domo> = [];
      var ix = 1;
      var level = 0;
      for (step in steps) {
        tabs.push(mkTab(level));
        final nm = tdN().text("" + ix + ".");
        final rs = tdRs();
        var prStr = "";
        switch (step.reason) {
          case Sup(p): {
            tabs[tabs.length - 1].html(
              tabs[tabs.length - 1].getHtml() + "┌&nbsp;"
            );
            rs.html("&nbsp" + _("Sup") + "&nbsp;");
            prStr = Prop.toString(p);
            ++level;
          }
          case Theorem(id, index, replacements): {
            rs
              .att("title",
                  Prop.toString(Opt.eget(book.get(id)).conclusion) + "\n" +
                  replacements.map(r ->
                      r.key + " >> " + Prop.toString(r.value)
                    ).join("\n")
                )
              .html("&nbsp;├─ " + id + " in " + (index + 1) + "&nbsp;")
            ;
            prStr = Prop.toString(step.prop);
          }
          case IN: {
            rs.html("&nbsp+ " + OperatorSet.N + "&nbsp;");
            prStr = Prop.toString(step.prop);
            tabs[tabs.length - 1] = mkTab(level - 1);
            tabs[tabs.length - 2].html(
              mkTab(level - 1).getHtml() +
              (switch (steps[tabs.length - 2].reason) {
                case Sup(p): "⊏";
                default : "└";
              }) + "&nbsp;"
            );
            --level;
          }
          case EN: {
            rs.html("&nbsp– " + OperatorSet.N + "&nbsp;");
            prStr = Prop.toString(step.prop);
          }
          case IC: {
            rs.html("&nbsp+ " + OperatorSet.C + "&nbsp;");
            prStr = Prop.toString(step.prop);
            tabs[tabs.length - 1] = mkTab(level - 1);
            tabs[tabs.length - 2].html(
              mkTab(level - 1).getHtml() +
              (switch (steps[tabs.length - 2].reason) {
                case Sup(p): "⊏";
                default : "└";
              }) + "&nbsp;"
            );
            --level;
          }
          case EC: {
            rs.html("&nbsp– " + OperatorSet.C + "&nbsp;");
            prStr = Prop.toString(step.prop);
          }
          case IA: {
            rs.html("&nbsp+ " + OperatorSet.A + "&nbsp;");
            prStr = Prop.toString(step.prop);
          }
          case EA: {
            rs.html("&nbsp– " + OperatorSet.A + "&nbsp;");
            prStr = Prop.toString(step.prop);
          }
          case IK: {
            rs.html("&nbsp+ " + OperatorSet.K + "&nbsp;");
            prStr = Prop.toString(step.prop);
          }
          case EK: {
            rs.html("&nbsp– " + OperatorSet.K + "&nbsp;");
            prStr = Prop.toString(step.prop);
          }
          case IE: {
            rs.html("&nbsp+ " + OperatorSet.E + "&nbsp;");
            prStr = Prop.toString(step.prop);
          }
          case EE: {
            rs.html("&nbsp– " + OperatorSet.E + "&nbsp;");
            prStr = Prop.toString(step.prop);
          }
          case ID: {
            rs.html("&nbsp+ " + OperatorSet.D + "&nbsp;");
            prStr = Prop.toString(step.prop);
          }
          case ED: {
            rs.html("&nbsp– " + OperatorSet.D + "&nbsp;");
            prStr = Prop.toString(step.prop);
          }
          default:
        }
        final pr = tdPr()
          .add(tabs[tabs.length - 1])
          .add(Q("span")
            .style("color:" + (step.isActive ? "#000000": "#999999") + ";")
            .html(prStr))
        ;
        tb.add(Q("tr").add(nm).add(rs).add(pr));
        ++ix;
      }
    }

    wg
      .removeAll()
      .add(tb)
    ;
  }

}

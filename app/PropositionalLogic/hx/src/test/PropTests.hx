// Copyright 02-Aug-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package test;

import dm.Test;
import data.Prop;

class PropTests {
  public static function run () {
    final t = new Test("Proposition");

    final p1 = Prop.mkAtomic("p9");
    t.yes(Prop.isValid(p1));
    t.yes(Prop.eq(p1, p1));
    t.yes(Prop.eq(p1, Prop.fromJs(Prop.toJs(p1))));

    final p2 = Prop.mkN(p1);
    t.yes(Prop.isValid(p2));
    t.yes(!Prop.eq(p2, p1));
    t.yes(!Prop.eq(p1, p2));
    t.yes(Prop.eq(p2, p2));
    t.yes(Prop.eq(p2, Prop.fromJs(Prop.toJs(p2))));

    final p3 = Prop.mkC(p1, p2);
    t.yes(Prop.isValid(p3));
    t.yes(!Prop.eq(p3, p1));
    t.yes(!Prop.eq(p2, p3));
    t.yes(Prop.eq(p3, p3));
    t.yes(Prop.eq(p3, Prop.fromJs(Prop.toJs(p3))));

    final p4 = Prop.mkA(p1, p2);
    t.yes(Prop.isValid(p4));
    t.yes(!Prop.eq(p4, p1));
    t.yes(!Prop.eq(p3, p4));
    t.yes(Prop.eq(p4, p4));
    t.yes(Prop.eq(p4, Prop.fromJs(Prop.toJs(p4))));

    final p5 = Prop.mkK(p1, p2);
    t.yes(Prop.isValid(p5));
    t.yes(!Prop.eq(p5, p1));
    t.yes(!Prop.eq(p3, p5));
    t.yes(Prop.eq(p5, p5));
    t.yes(Prop.eq(p5, Prop.fromJs(Prop.toJs(p5))));

    final p6 = Prop.mkD(p1, p3);
    t.yes(Prop.isValid(p6));
    t.yes(!Prop.eq(p6, p1));
    t.yes(!Prop.eq(p3, p6));
    t.yes(Prop.eq(p6, p6));
    t.yes(Prop.eq(p6, Prop.fromJs(Prop.toJs(p6))));

    final p7 = Prop.mkD(p3, p2);
    t.yes(Prop.isValid(p7));
    t.yes(!Prop.eq(p7, p1));
    t.yes(!Prop.eq(p3, p7));
    t.yes(Prop.eq(p7, p7));
    t.yes(Prop.eq(p7, Prop.fromJs(Prop.toJs(p7))));

    t.log();
  }
}

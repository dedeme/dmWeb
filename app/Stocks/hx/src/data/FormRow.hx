// Copyright 25-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Opt;

class FormRow {
  public var date(default, null): String;
  public var bs(default, null): Float;
  public var bp(default, null): Float;
  public var bt(default, null): Float;
  public var ss(default, null): Float;
  public var sp(default, null): Float;
  public var st(default, null): Float;
  public var ts(default, null): Float;
  public var tp(default, null): Float;
  public var tt(default, null): Float;
  public var profits(default, null): Option<Float>;
  public var ttProfits(default, null): Float;
  public var fees(default, null): Option<Float>;
  public var ttFees(default, null): Float;

  public function new (
    date: String,
    bs: Float, bp: Float, bt: Float,
    ss: Float, sp: Float, st: Float,
    ts: Float, tp: Float, tt: Float,
    profits: Option<Float>, ttProfits: Float,
    fees: Option<Float>, ttFees: Float
  ) {
    this.date = date;
    this.bs = bs;
    this.bp = bp;
    this.bt = bt;
    this.ss = ss;
    this.sp = sp;
    this.st = st;
    this.ts = ts;
    this.tp = tp;
    this.tt = tt;
    this.profits = profits;
    this.ttProfits = ttProfits;
    this.fees = fees;
    this.ttFees = ttFees;
  }
}

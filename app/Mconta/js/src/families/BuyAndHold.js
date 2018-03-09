// Copyright 19-Jan-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

goog.provide("families_BuyAndHold");
goog.require("Family");

families_BuyAndHold = class {
  constructor () {
  }

  mkFamily () {
    function bests(intFormat, floatFormat, span) {
      return It.from([
        ]).addIt(It.range(span).map(i => $("td")));
    }

    function trace(intFormat, floatFormat, head, body) {
      return $("span").html(_("BH trace data"))
    }

    function traceError(quotes, t, r) {
      return r;
    }

    function traceBody(quotes, t) {
      return It.empty();
    }

    return new Family(
      0,
      bests,
      trace,
      traceError,
      traceBody
    );
  }
}

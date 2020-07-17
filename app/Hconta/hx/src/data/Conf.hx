// Copyright 27-Jun-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Js;
import dm.Dt;

/// Application configuration.
class Conf {
  /// Can be "en" or "es".
  public var language: String;
  /// All the years with data (sorted from before to after).
  public var years: Array<String>;
  /// Current year.
  public var currentYear: String;

  public function new (
    language: String, years: Array<String>, currentYear: String
  ) {
    this.language = language;
    this.years = years;
    this.currentYear = currentYear;
  }

  public function isLastYear (): Bool {
    return currentYear == years[years.length - 1];
  }
}

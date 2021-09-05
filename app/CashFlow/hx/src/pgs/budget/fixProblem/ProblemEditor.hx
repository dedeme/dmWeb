// Copyright 30-Aug-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.budget.fixProblem;

interface ProblemEditor {
  public function deactivate (): Void;
  public function updateAccount (acc: String): Void;
}

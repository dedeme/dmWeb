// Copyright 31-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Global constants
class Cts {
  /// Home for cgi data.
  public static final webHome = "dmcgi/" + cm.Cts.appName;
  /// Home for command data.
  public static final dbHome = "/home/deme/.dmHxApp/" + cm.Cts.appName;

	/// Minimum value to analyze.
	public static final rangesMin = 12; // id 120000 -> parameter 0.12
	/// Group of 10000 to analyze
	public static final rangesGroups = 10; // from 120000 inclusive to 220000 exclusive [0.12-0.22)
	/// Number of parameters per group.
	public static final rangesGroupElements = 10000;
  /// Entries in a parameter ranking.
  public static final rankingSize = 40;
  /// Number of parameter rankings saved.
  public static final totalRankings = 4;
}

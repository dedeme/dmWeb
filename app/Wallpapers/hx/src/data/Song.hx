// Copyright 30-Apr-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Mac;
import dm.Js;

/// Song data (Serializable)
/// Records:
///   level: Int        - Picture level (1, 2, o 3).
///   sights: Int - Number of sights in normal mode.
///   id: String        - Picture name.
///   time: Int        - Duration in seconds.
@:build(dm.Mac.record([
  "level: Int",
  "sights: Int",
  "id: String",
  "time: Int"
  ], true))
class Song {}

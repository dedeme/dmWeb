// Copyright 17-May-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Mac;
import dm.Js;

/// Picture data (Serializable)
/// Records:
///   level: Int        - Song level (0(unknown), 1(ok), o 2(well)).
///   speed: Int        - Song speed (0(slow), 1(fast)).
///   id: String        - Song name.
@:build(dm.Mac.record([
  "level: Int",
  "speed: Int",
  "id: String"
  ], true))
class DanceSong {}

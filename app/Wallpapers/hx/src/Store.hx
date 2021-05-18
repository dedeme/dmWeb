// Copyright 11-May-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import dm.Js;
import dm.Opt;
import dm.It;
import dm.Rnd;
import data.Song;

class Store {
  static final songsKey = "Wallpapers_songs";
  static final selKey = "Wallpapers_sel";

  /// Returns the current song.
  ///   songIds: Server song ids.
  public static function getSel (songIds: Array<String>): Song {
    final songs = sync(songIds);
    switch (dm.Store.get(selKey)) {
      case Some(s): {
        switch (It.from(songs).find(e -> e.id == s)) {
          case Some (rs) : return rs;
          case None : return getNext(songIds);
        }
      }
      case None: return getNext(songIds);
    }
  }

  /// Returns the current stored song id.
  /// NOTE: It may not be in filesystem.
  public static function getSelId (): String {
    return switch (dm.Store.get(selKey)) {
      case Some(s): s;
      case None: "";
    }
  }

  /// Updates the field 'lapse' of the current song.
  ///   lapse: Current time of current song.
  public static function setCurrentLapse(lapse: Float): Void {
    final songs = Js.from(Opt.eget(dm.Store.get(songsKey)))
      .ra()
      .map(e -> Song.fromJs(e))
    ;
    final sel = Opt.eget(dm.Store.get(selKey));
    for (s in songs) {
      if (s.id == sel) {
        s.lapse = lapse;
      }
    }
    dm.Store.put(songsKey, Js.wa(songs.map(e -> e.toJs())).to());
  }

  /// Updates the field 'level' of the song 'id'
  public static function setLevel(id: String, level: Int): Void {
    final songs = Js.from(Opt.eget(dm.Store.get(songsKey)))
      .ra()
      .map(e -> Song.fromJs(e))
    ;
    for (s in songs) {
      if (s.id == id) {
        s.level = level;
      }
    }
    dm.Store.put(songsKey, Js.wa(songs.map(e -> e.toJs())).to());
  }

  /// Updates the field 'lapse' of the song 'id'
  public static function setLapse(id: String, lapse: Float): Void {
    final songs = Js.from(Opt.eget(dm.Store.get(songsKey)))
      .ra()
      .map(e -> Song.fromJs(e))
    ;
    for (s in songs) {
      if (s.id == id) {
        s.lapse = lapse;
      }
    }
    dm.Store.put(songsKey, Js.wa(songs.map(e -> e.toJs())).to());
  }

  /// Selects the song 'id'
  public static function setSel(id: String): Void {
    final songs = Js.from(Opt.eget(dm.Store.get(songsKey)))
      .ra()
      .map(e -> Song.fromJs(e))
    ;
    for (s in songs) {
      if (s.id == id) {
        dm.Store.put(selKey, id);
      }
    }
  }

  /// Returns the next song.
  ///   songIds: Server song ids.
  public static function getNext (songIds: Array<String>): Song {
    final songs = sync(songIds);
    final filtered = It.from(songs).filter(e -> e.sights < e.level).to();

    if (filtered.length == 0) {
      for (s in songs) {
        s.sights = 0;
      }
      dm.Store.put(songsKey, Js.wa(songs.map(e -> e.toJs())).to());
      return getNext(songIds);
    }

    final s = filtered[Rnd.i(filtered.length)];
    ++s.sights;
    dm.Store.put(songsKey, Js.wa(songs.map(e -> e.toJs())).to());
    dm.Store.put(selKey, s.id);

    return s;
  }

  // Synchornizes server with local songs
  //   songs : Server songs.
  //   return: Local songs.
  public static function sync (songs: Array<String>): Array<Song> {
    final ssongs = Js.from(Opt.oget(dm.Store.get(songsKey), "[]"))
      .ra()
      .map(e -> Song.fromJs(e))
    ;
    final stSongs = It.from(ssongs).filter(e -> songs.contains(e.id)).to();
    for (s in songs) {
      if (It.from(stSongs).indexf(e -> e.id == s) == -1) {
        stSongs.push(Song.mk(s));
      }
    }
    dm.Store.put(songsKey, Js.wa(stSongs.map(e -> e.toJs())).to());
    return stSongs;
  }

}

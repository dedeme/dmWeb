// Copyright 15-Jun-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package dm;

/// Class to connect with an Internet server.
class Client {
  var isDmCgi: Bool;
  var appName: String;
  var fExpired: Void -> Void;
  var key = "0";
  var connectionKey = "";

  /// Connected user.
  public var user(default, null) = "";

  /// Connected user level.
  public var level(default, null) = "";

  /// Constructor.
  ///   isDmCgi : If is 'true' server is accessed through 'dmcgi'.
  ///   appName : Used to customize LocalStore.
  ///   fexpired: Function to launch an expired page.
  public function new (isDmCgi: Bool, appName: String, fExpired: Void -> Void) {
    this.isDmCgi = isDmCgi;
    this.appName = appName;
    this.fExpired = fExpired;
  }

  function setSessionId (value: String): Void {
    Store.put("Client_sessionId_" + appName, value);
  }

  function sendServer (rq: String, fn: String -> Void): Void {
    final request = new js.html.XMLHttpRequest();

    request.onload = _ -> {
      if (request.status == 200) fn(StringTools.trim(request.responseText));
      else throw (new haxe.Exception(request.statusText));
    };

    request.onerror = _ -> throw (new haxe.Exception("Network Error"));

    request.open(
      "POST",
      "http://" + js.Browser.location.host +
        (isDmCgi ? "/cgi-bin/ccgi.sh" : ""),
      true
    );
    request.setRequestHeader(
      "Content-Type",
      "text/plain"
    );

    request.send(appName + ":" + rq);
  }

  // Send data to server.
  // If isSecure is 'true' a connectionKey will be sent to server.
  function sendCommon (
    isSecure: Bool, data: Map<String, Js>, fn: Map<String, Js> -> Void
  ): Void {
    final fn2 = rp -> {
      var data: Map<String, Js>;
      try {
        final jdata = Cryp.decryp(key, rp);
        data = Js.from(jdata).ro();
      } catch (e) {
        try {
          final jdata = Cryp.decryp("nosession", rp);
          final data = Js.from(jdata).ro();
          if (data.exists("expired")) {
            fExpired();
            return;
          }
          throw(e);
        } catch (e2) {
          throw('RAW SERVER RESPONSE:\n${rp}\nCLIENT ERROR:\n${e}');
        }
      }
      fn(data);
    }

    final rq = isSecure
      ? sessionId() + ":" +
        connectionKey + ":" + Cryp.cryp(key, Js.wo(data).to())
      : sessionId() + ":" + Cryp.cryp(key, Js.wo(data).to())
    ;
    sendServer(rq, fn2);
  }

  /// Returns the session identifier. Before connection its value is
  /// 'B64.encode("0")'.
  public function sessionId (): String {
    return switch (Store.get("Client_sessionId_" + appName)) {
      case Some(ss): ss;
      case None: B64.encode("0");
    }
  }

  /// Try to connect with server.
  /// If connection fails, it sends 'false' to 'fn'. Otherwise it sends 'true'.
  public function connect (fn: Bool -> Void): Void {
    final fn2 = rp -> {
      try {
        final jdata = Cryp.decryp(sessionId(), rp);
        final data = Js.from(jdata).ro();
        key = data.get("key").rs();
        if (key == "") {
          fn(false);
          return;
        }
        user = data.get("user").rs();
        level = data.get("level").rs();
        connectionKey = data.get("conKey").rs();
        fn(true);
      } catch (e) {
        trace('RAW SERVER RESPONSE:\n${rp}\nCLIENT ERROR:\n${e}');
      }
    }
    sendServer(sessionId(), fn2);
  }

  /// Try to authenticate user.
  /// If authentication fails, it sends 'false' to 'fn'. Otherwise it sends
  /// 'true'.
  ///   user          : User name.
  ///   pass          : Password as it is written by user.
  ///   withExpiration: If its value is 'true', connection will be temporary.
  ///   fn            : Callback
  public function authentication (
    user: String, pass: String, withExpiration: Bool, fn: Bool -> Void
  ) {
    final fn2 = rp -> {
      try {
        final jdata = Cryp.decryp(key, rp);
        final data = Js.from(jdata).ro();
        final sessionId = data.get("sessionId").rs();
        if (sessionId == "") {
          fn(false);
          return;
        }
        setSessionId(sessionId);
        this.user = user;
        key = data.get("key").rs();
        level = data.get("level").rs();
        connectionKey = data.get("conKey").rs();
        fn(true);
      } catch (e) {
        trace('RAW SERVER RESPONSE:\n${rp}\nCLIENT ERROR:\n${e}');
      }
    }

    key = Cryp.key(appName, klen);
    final p = Client.crypPass(pass);
    final exp = withExpiration ? "1" : "0";
    sendServer(":" + Cryp.cryp(key, '${user}:${p}:${exp}'), fn2);
  }

  /// Sends normal data to server.
  public function send(
    rq: Map<String, Js>, fn: Map<String, Js> -> Void
  ): Void {
    sendCommon(false, rq, fn);
  }

  /// Sends data to server protected against 'out of date' state.
  public function ssend(
    rq: Map<String, Js>, fn: Map<String, Js> -> Void
  ): Void {
    sendCommon(true, rq, fn);
  }

  /// Request to server a "long run" task.
  /// Process:
  ///    Client: Automaticaly adds a string field called "longRunFile" set
  ///            to "" and send 'rq'.
  ///    Server: Launch the task in thread apart and return "longRunFile" with
  ///            the path of a file which will contain a response.
  ///    Client: Set "longRunFile" with the value returned by server and send
  ///            'rq' every second until server indicates the end of the task or
  ///            it passes 1 minute.
  ///    Server: Adds to response a field called "longRunEnd" set to 'true' if
  ///            the task is finished or 'false' otherwise.
  public function longRun (
    rq: Map<String, Js>, fn: Map<String, Js> -> Void
  ): Void {
    final fn2 = rp -> {
      final tm = new haxe.Timer(1000);
      tm.run = () -> {
        var count: Int = 0;
        final fn3 = (rp2: Map<String, Js>) -> {
          if (rp2.exists("longRunEnd") && rp2.get("longRunEnd").rb()) {
            tm.stop();
            fn(rp2);
            return;
          }
          if (count > 60) tm.stop;
          ++count;
        }
        send(rp, fn3);
      }
    }

    rq.set("longRunFile", Js.ws(""));
    send(rq, fn2);
  }

  // Static --------------------------------------------------------------------

  static final klen = 300;

  /// Processing of user password before sending it to server.
  public static function crypPass (pass: String): String {
    return Cryp.key(pass, klen);
  }
}

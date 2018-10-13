# Copyright 06-Oct-2017 ºDeme
# GNU General Public License - V3 <http://www.gnu.org/licenses/>

require "http/server"

module Dummy2
  def self.process(rq : JSON::Any) : Tuple(Bool, String)
    if pg = rq["page"]
      case pg.as_s
      when "main"
        Main.process rq
      when "settings"
        Settings.process rq
      when "backups"
        Backups.process rq
      else
        Cgi.fail "Page '#{pg.as_s}' is unknown"
      end
    else
      Cgi.fail "Field 'page' is missing"
    end
  end

  # Start main -------------------------------------------------------

  Sys.init Cts::APP_NAME
  Iout.init

  spawn do
    Sys.exec("firefox", [Cts::BROWSER_PATH])
  end

  Cgi.new(Cts::APP_NAME, Cts::PORT, Cts::WEB_DIRECTORY).listen(
    ->process(JSON::Any)
  )

end

# Copyright 09-Oct-2017 ºDeme
# GNU General Public License - V3 <http://www.gnu.org/licenses/>

module MyMarket
  module Main
    extend self

    def process(rq : JSON::Any) : Tuple(Bool, String)
      if r = rq["rq"]
        case r.as_s
        when "init"
          Cgi.ok do |jsb|
            jsb.field "conf" do jsb.raw Iout.read_conf_s end
            jsb.field("closed", true)
          end
        when "logout"
          spawn do
            Iout.automatic_backup
            sleep 1
            exit 0
          end
          Cgi.ok
        else
          Cgi.fail "Main-rq: value '#{r.as_s}' is unknown"
        end
      else
        Cgi.fail "Main: Field 'rq' is missing"
      end
    end
  end
end

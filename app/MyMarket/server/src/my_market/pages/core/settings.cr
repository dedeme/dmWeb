# Copyright 09-Oct-2017 ºDeme
# GNU General Public License - V3 <http://www.gnu.org/licenses/>

module MyMarket
  module Settings
    extend self

    def process(rq : JSON::Any) : Tuple(Bool, String)
      case rq["rq"].as_s
      when "init"
        user = Iout.read_conf.user
        years = Years.new user
        Cgi.ok do |jsb|
          jsb.field "users" do
            jsb.array do
              Users.list.each { |u| jsb.string u }
            end
          end
          jsb.field "years" do
            jsb.array do
              years.list.each { |y| jsb.string y }
            end
          end
        end
      when "setLang"
        Iout.set_lang rq["lang"].as_s
        Cgi.ok
      when "addUser"
        Users.add rq["user"].as_s
        Cgi.ok
      when "removeUser"
        Users.remove rq["user"].as_s
        Cgi.ok
      when "modifyUser"
        Users.modify(rq["old_user"].as_s, rq["user"].as_s)
        Cgi.ok
      when "selectUser"
        Iout.set_user rq["user"].as_s
        Cgi.ok
      else
        Cgi.fail "Settings-rq: value '#{rq["rq"].as_s}' is unknown"
      end
    end
  end
end

# Copyright 06-Oct-2017 ºDeme
# GNU General Public License - V3 <http://www.gnu.org/licenses/>

module Dummy2
  module Settings
    extend self

    def process(rq : JSON::Any) : Tuple(Bool, String)
      if r = rq["rq"]
        case r.as_s
        when "setLang"
          if lang = rq["lang"]
            Iout.set_lang lang.as_s
            Cgi.ok
          else
            Cgi.fail "Settings-rq-setLang: Field 'lang' is missing"
          end
        else
          Cgi.fail "Settings-rq: value '#{r.as_s}' is unknown"
        end
      else
        Cgi.fail "Settings: Field 'rq' is missing"
      end
    end
  end
end

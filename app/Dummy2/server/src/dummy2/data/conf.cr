# Copyright 06-Oct-2017 ºDeme
# GNU General Public License - V3 <http://www.gnu.org/licenses/>

module Dummy2
  # Data of configuration
  class Conf
    property lang
    property menu
    def initialize(@lang : String, @menu : String)
    end

    def to_s : String
      JSON.build do |jsb|
        jsb.object do
          jsb.field("lang", @lang)
          jsb.field("menu", @menu)
        end
      end
    end

    def self.from_s(js : String) : Conf
      js = JSON.parse js
      Conf.new(js["lang"].as_s, js["menu"].as_s)
    end
  end
end

# Copyright 09-Oct-2017 ºDeme
# GNU General Public License - V3 <http://www.gnu.org/licenses/>

module MyMarket
  # Data of configuration
  class Conf
    property lang
    property user
    property year
    def initialize(@lang : String, @user : String, @year : String)
    end

    def to_s : String
      JSON.build do |jsb|
        jsb.object do
          jsb.field("lang", @lang)
          jsb.field("user", @user)
          jsb.field("year", @year)
        end
      end
    end

    def self.from_s(js : String) : Conf
      js = JSON.parse js
      Conf.new(
        js["lang"].as_s,
        js["user"].as_s,
        js["year"].as_s
      )
    end
  end
end

# Copyright 10-Oct-2017 ºDeme
# GNU General Public License - V3 <http://www.gnu.org/licenses/>

require "json"

module MyMarket
  class Ledger
    property stocks = 0.0
    property cash = 0.0
    property capital = 0.0
    property sells = 0.0
    property fees = 0.0
    property incomes = 0.0 # Dividends and similar
    property differences = 0.0

    def initialize
    end

    def to_json(jsb : JSON::Builder)
      jsb.array do
        jsb.number stocks
        jsb.number cash
        jsb.number capital
        jsb.number sells
        jsb.number fees
        jsb.number incomes
        jsb.number differences
      end
    end

    def to_s : String
      JSON.build { |jsb| to_json jsb }
    end
  end
end

# Copyright 10-Oct-2017 ºDeme
# GNU General Public License - V3 <http://www.gnu.org/licenses/>

require "json"

module MyMarket
  # Annotation
  class Ann
    enum Type
      Sell
      Buy
      Withdrawal
      Income
      Profits # Dividends and similar
      Fees    # Direct broker fees, out of operations
      Pdif    # Correction positive (+ cash)
      Ndif    # Correction negative (- cash)
      InitialCapital
      InitialCash
      InitialStock
    end

    getter date : String
    getter type : Type
    getter nick : String
    getter stocks : Int32
    getter money : Float64
    getter description : String

    protected def initialize(@date, @type, @nick, @stocks, @money, @description)
    end

    def year : String
      @date[0..3]
    end

    def to_json(jsb : JSON::Builder)
      jsb.array do
        jsb.string @date
        jsb.string @type.to_s
        jsb.string @nick
        jsb.number @stocks
        jsb.number @money
        jsb.string @description
      end
    end

    def self.from_json(js : JSON::Any) : Ann
      Ann.new(
        js[0].as_s,
        Type.parse(js[1].as_s),
        js[2].as_s,
        js[3].as_i,
        js[4].as_f,
        js[5].as_s
      )
    end

    def to_s : String
      JSON.build { |jsb| to_json jsb }
    end

    def self.from_s(s : String) : Ann
      Ann.from_json(JSON.parse s)
    end
  end
end

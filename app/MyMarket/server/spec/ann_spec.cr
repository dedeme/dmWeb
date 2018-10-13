# Copyright 02-Oct-2017 ºDeme
# GNU General Public License - V3 <http://www.gnu.org/licenses/>

require "../src/my_market/data/ann"
require "spec"
require "json"

include MyMarket

describe Ann do
  it "All" do
    s = %(["20181009","Sell","TEF",450,12.5,""])
    o = Ann.from_s s
    o.to_s.should eq s
    o.date.should eq "20181009"
    o.type.should eq Ann::Type::Sell
    o.nick.should eq "TEF"
    o.stocks.should eq 450
    o.money.should eq 12.5
    o.description.should eq ""
    o.year.should eq "2018"

    s = %(["20161009","Buy","TEF",450,12.5,""])
    o = Ann.from_s s
    o.to_s.should eq s
    o.date.should eq "20161009"
    o.type.should eq Ann::Type::Buy
    o.nick.should eq "TEF"
    o.stocks.should eq 450
    o.money.should eq 12.5
    o.description.should eq ""
    o.year.should eq "2016"

  end
end

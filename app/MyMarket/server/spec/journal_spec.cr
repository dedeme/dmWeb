# Copyright 11-Oct-2017 ºDeme
# GNU General Public License - V3 <http://www.gnu.org/licenses/>

require "../src/my_market/data/journal"
require "spec"
require "json"

include MyMarket

describe Ann do
  it "All" do
    j = Journal.new
    j.make_books
    j.to_s.should eq "[]"
  end
end

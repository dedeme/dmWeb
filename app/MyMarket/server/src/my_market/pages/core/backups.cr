# Copyright 09-Oct-2017 ºDeme
# GNU General Public License - V3 <http://www.gnu.org/licenses/>

module MyMarket
  module Backups
    extend self

    def process(rq : JSON::Any) : Tuple(Bool, String)
      if r = rq["rq"]
        case r.as_s
        when "lists"
          backups, trash = Iout.backups_trash_lists.values
          Cgi.ok do |jsb|
            jsb.field "backups" do
              jsb.array do backups.each { |b| jsb.string b } end
            end
            jsb.field "trash" do
              jsb.array do trash.each { |b| jsb.string b } end
            end
          end
        when "backup"
          Cgi.ok do |jsb|
            jsb.field "name" do jsb.string Iout.backup end
          end
        when "restoreStart"
          Iout.restore_start
          Cgi.ok
        when "restoreAppend"
          if data = rq["data"]
            Iout.restore_append (B64.decode_bytes data.as_s)
            Cgi.ok
          else
            Cgi.fail "Backups-restoreStart: Field 'data' is missing"
          end
        when "restoreEnd"
          Cgi.ok do |jsb|
            jsb.field "fail" do jsb.string Iout.restore_end end
          end
        when "autorestore"
          if file = rq["file"]
            Iout.autorestore file.as_s
            Cgi.ok
          else
            Cgi.fail "Backups-autorestore: Field 'file' is missing"
          end
        when "restoreTrash"
          if file = rq["file"]
            Iout.restoreTrash file.as_s
            Cgi.ok
          else
            Cgi.fail "Backups-restoreTrash: Field 'file' is missing"
          end
        when "clearTrash"
          Iout.clearTrash
          Cgi.ok
        else
          Cgi.fail "Backups-rq: value '#{r.as_s}' is unknown"
        end
      else
        Cgi.fail "Backups: Field 'rq' is missing"
      end
    end
  end
end

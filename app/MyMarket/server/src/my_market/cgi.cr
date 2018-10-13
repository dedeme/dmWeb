# Copyright 09-Oct-2017 ºDeme
# GNU General Public License - V3 <http://www.gnu.org/licenses/>

require "http/server"
require "json"
require "dedeme_lib"

include DedemeLib

module MyMarket
  class Cgi
    Klen = 300

    # 'app_name' is used as key in the first communication.
    #
    # 'path' is the directory where pages are. e.g. "/dm/www2/Dummy2"
    def initialize(
      @app_name : String,
      @port : Int32,
      @path : String
    )
      @first_time = true
      @key = B64.genk Klen
      @session_id = B64.genk Klen
      @connection_id = B64.genk Klen
    end

    # Creates server and block the program waiting requests.
    #
    # Normal request are send in 'action'
    #
    # Connection and authentication request are processed automatically
    def listen(action : JSON::Any -> Tuple(Bool, String))
      server = HTTP::Server.new([
        HTTP::ErrorHandler.new,
        HTTP::LogHandler.new,
        HTTP::StaticFileHandler.new(
          @path, true, false
        ),
      ])  do |context|
        context.response.content_type = "text/plain"
        io = context.request.body
        if io.nil?
          context.response.print "Request body is missing"
          next
        end
        rq = io.gets_to_end
        if !rq.starts_with?("#{@app_name}:")
          context.response.print "Request does not start with application name"
          next
        end
        ix = rq.index ':'
        if ix.nil?
          context.response.print "Request does not start with application name"
          next
        end

        rq = rq[ix + 1...rq.size]
        ix = rq.index ':'
        rp = ""
        key = @key
        # Connection -------------------------------------------------
        if ix.nil?
          key = rq  # key is changed
          if key != @session_id
            rp = JSON.build do |jsb|
              jsb.object do
                jsb.field("key", "")
                jsb.field("connectionId", "")
              end
            end
          else
            rp = JSON.build do |jsb|
              jsb.object do
                jsb.field("key", @key)
                jsb.field("connectionId", @connection_id)
              end
            end
          end
        # Authentication ---------------------------------------------
        elsif ix == 0
          if @first_time
            @first_time = false
            rp = JSON.build do |jsb|
              jsb.object do
                jsb.field("key", @key)
                jsb.field("sessionId", @session_id)
              end
            end
            key = B64.key(@app_name, Klen) # key is changed
          else
            context.response.print "Connection already open"
            next
          end
        # Normal message ---------------------------------------------
        else
          ss_id = rq[0...ix]
          msg = B64.decryp(@key, rq[ix + 1...rq.size])
          if ss_id == @session_id
            fail, rp = action.call(JSON.parse msg)
            if fail
              context.response.print rp
              next
            end
          else
            context.response.print "Bad session id"
            next
          end
        end

        # Response ---------------------------------------------------
        context.response.print B64.cryp(key, rp)
      end

      server.bind_tcp "localhost", @port
      puts "Listening on #{@port}"
      server.listen
    end

    # Returns a 'fail' response.
    def self.fail(msg : String) : Tuple(Bool, String)
      {true, msg}
    end

    # Returns a 'ok' response. This response contains an object and you should
    # supply its fields.
    # ```
    # Cgi.ok do |jsb|
    #   jsb.field "name" do jsb.string "Louis" end
    #   jsb.field "age" do jsb.number 23 end
    # end
    # ```
    def self.ok : Tuple(Bool, String)
      {false, JSON.build { |jsb| jsb.object do yield jsb end } }
    end

    # Returns an empty 'ok' response ({false, "{}"})
    def self.ok : Tuple(Bool, String)
      {false, "{}"}
    end

  end
end

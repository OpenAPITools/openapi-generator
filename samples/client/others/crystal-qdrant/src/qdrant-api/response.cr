module Qdrant::Api
  struct Response(T)
    getter value : T
    getter status : Int32
    getter headers : HTTP::Headers

    def initialize(@value : T, @status : Int32, @headers : HTTP::Headers); end

    def success? : Bool
      200 <= @status < 300
    end
  end
end

require "kemal"
require "base64"

module KemalBasicAuth
  class Handler < Kemal::Handler
    BASIC                 = "Basic"
    AUTH                  = "Authorization"
    AUTH_MESSAGE          = "Could not verify your access level for that URL.\nYou have to login with proper credentials"
    HEADER_LOGIN_REQUIRED = "Basic realm=\"Login Required\""

    def initialize(@username : String, @password : String)
    end

    def call(env)
      if env.request.headers[AUTH]?
        if value = env.request.headers[AUTH]
          if value.size > 0 && value.starts_with?(BASIC)
            if authorize?(value)
              return call_next(env)
            end
          end
        end
      end

      env.response.status_code = 401
      env.response.headers["WWW-Authenticate"] = HEADER_LOGIN_REQUIRED
      env.response.print(AUTH_MESSAGE)
    end

    private def authorize?(value) : String?
      username, password = Base64.decode_string(value[BASIC.size + 1..-1]).split(":")
      [@username, @password] == [username, password] ? "" : nil
    end
  end
end

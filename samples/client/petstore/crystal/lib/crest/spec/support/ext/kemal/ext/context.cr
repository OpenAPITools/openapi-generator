class HTTP::Server
  class Context
    # TODO https://github.com/kemalcr/kemal/pull/561
    def redirect(url : String, status_code : Int32 = 302, *, body : String = "")
      @response.headers.add "Location", url
      @response.write(body.to_slice)
      @response.status_code = status_code
    end
  end
end

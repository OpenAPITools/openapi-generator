module Kemal
  # Route is the main building block of Kemal.
  #
  # It takes 3 parameters: http *method*, *path* and a *handler* to specify
  # what action to be done if the route is matched.
  struct Route
    getter method, path, handler
    @handler : HTTP::Server::Context -> String

    def initialize(@method : String, @path : String, &handler : HTTP::Server::Context -> _)
      @handler = ->(context : HTTP::Server::Context) do
        output = handler.call(context)
        output.is_a?(String) ? output : ""
      end
    end
  end
end

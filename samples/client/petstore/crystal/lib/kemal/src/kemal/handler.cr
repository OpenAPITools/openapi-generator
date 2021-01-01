module Kemal
  # `Kemal::Handler` is a subclass of `HTTP::Handler`.
  #
  # It adds `only`, `only_match?`, `exclude`, `exclude_match?`.
  # These methods are useful for the conditional execution of custom handlers .
  class Handler
    include HTTP::Handler

    @@only_routes_tree = Radix::Tree(String).new
    @@exclude_routes_tree = Radix::Tree(String).new

    macro only(paths, method = "GET")
      class_name = {{@type.name}}
      method_downcase = {{method.downcase}}
      class_name_method = "#{class_name}/#{method_downcase}"
      ({{paths}}).each do |path|
        @@only_routes_tree.add class_name_method + path, '/' + method_downcase + path
      end
    end

    macro exclude(paths, method = "GET")
      class_name = {{@type.name}}
      method_downcase = {{method.downcase}}
      class_name_method = "#{class_name}/#{method_downcase}"
      ({{paths}}).each do |path|
        @@exclude_routes_tree.add class_name_method + path, '/' + method_downcase + path
      end
    end

    def call(env : HTTP::Server::Context)
      call_next(env)
    end

    # Processes the path based on `only` paths which is a `Array(String)`.
    # If the path is not found on `only` conditions the handler will continue processing.
    # If the path is found in `only` conditions it'll stop processing and will pass the request
    # to next handler.
    #
    # However this is not done automatically. All handlers must inherit from `Kemal::Handler`.
    #
    # ```
    # class OnlyHandler < Kemal::Handler
    #   only ["/"]
    #
    #   def call(env)
    #     return call_next(env) unless only_match?(env)
    #     puts "If the path is / i will be doing some processing here."
    #   end
    # end
    # ```
    def only_match?(env : HTTP::Server::Context)
      @@only_routes_tree.find(radix_path(env.request.method, env.request.path)).found?
    end

    # Processes the path based on `exclude` paths which is a `Array(String)`.
    # If the path is not found on `exclude` conditions the handler will continue processing.
    # If the path is found in `exclude` conditions it'll stop processing and will pass the request
    # to next handler.
    #
    # However this is not done automatically. All handlers must inherit from `Kemal::Handler`.
    #
    # ```
    # class ExcludeHandler < Kemal::Handler
    #   exclude ["/"]
    #
    #   def call(env)
    #     return call_next(env) if exclude_match?(env)
    #     puts "If the path is not / i will be doing some processing here."
    #   end
    # end
    # ```
    def exclude_match?(env : HTTP::Server::Context)
      @@exclude_routes_tree.find(radix_path(env.request.method, env.request.path)).found?
    end

    private def radix_path(method : String, path : String)
      "#{self.class}/#{method.downcase}#{path}"
    end
  end
end

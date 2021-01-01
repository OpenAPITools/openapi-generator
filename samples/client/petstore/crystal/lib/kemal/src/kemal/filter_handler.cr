module Kemal
  # :nodoc:
  class FilterHandler
    include HTTP::Handler
    INSTANCE = new

    # This middleware is lazily instantiated and added to the handlers as soon as a call to `after_X` or `before_X` is made.
    def initialize
      @tree = Radix::Tree(Array(FilterBlock)).new
      Kemal.config.add_filter_handler(self)
    end

    # The call order of the filters is `before_all -> before_x -> X -> after_x -> after_all`.
    def call(context : HTTP::Server::Context)
      return call_next(context) unless context.route_found?
      call_block_for_path_type("ALL", context.request.path, :before, context)
      call_block_for_path_type(context.request.method, context.request.path, :before, context)
      if Kemal.config.error_handlers.has_key?(context.response.status_code)
        raise Kemal::Exceptions::CustomException.new(context)
      end
      call_next(context)
      call_block_for_path_type(context.request.method, context.request.path, :after, context)
      call_block_for_path_type("ALL", context.request.path, :after, context)
      context
    end

    # :nodoc: This shouldn't be called directly, it's not private because
    # I need to call it for testing purpose since I can't call the macros in the spec.
    # It adds the block for the corresponding verb/path/type combination to the tree.
    def _add_route_filter(verb : String, path, type, &block : HTTP::Server::Context -> _)
      lookup = lookup_filters_for_path_type(verb, path, type)
      if lookup.found? && lookup.payload.is_a?(Array(FilterBlock))
        lookup.payload << FilterBlock.new(&block)
      else
        @tree.add radix_path(verb, path, type), [FilterBlock.new(&block)]
      end
    end

    # This can be called directly but it's simpler to just use the macros,
    # it will check if another filter is not already defined for this
    # verb/path/type and proceed to call `add_route_filter`
    def before(verb : String, path : String = "*", &block : HTTP::Server::Context -> _)
      _add_route_filter verb, path, :before, &block
    end

    # This can be called directly but it's simpler to just use the macros,
    # it will check if another filter is not already defined for this
    # verb/path/type and proceed to call `add_route_filter`
    def after(verb : String, path : String = "*", &block : HTTP::Server::Context -> _)
      _add_route_filter verb, path, :after, &block
    end

    # This will fetch the block for the verb/path/type from the tree and call it.
    private def call_block_for_path_type(verb : String?, path : String, type, context : HTTP::Server::Context)
      lookup = lookup_filters_for_path_type(verb, path, type)
      if lookup.found? && lookup.payload.is_a? Array(FilterBlock)
        blocks = lookup.payload
        blocks.each &.call(context)
      end
    end

    # This checks is filter is already defined for the verb/path/type combination
    private def filter_for_path_type_defined?(verb : String, path : String, type)
      lookup = @tree.find radix_path(verb, path, type)
      lookup.found? && lookup.payload.is_a? FilterBlock
    end

    # This returns a lookup for verb/path/type
    private def lookup_filters_for_path_type(verb : String?, path : String, type)
      @tree.find radix_path(verb, path, type)
    end

    private def radix_path(verb : String?, path : String, type : Symbol)
      "#{type}/#{verb}/#{path}"
    end

    # :nodoc:
    class FilterBlock
      property block : HTTP::Server::Context -> String

      def initialize(&block : HTTP::Server::Context -> _)
        @block = ->(context : HTTP::Server::Context) { block.call(context).to_s }
      end

      def call(context : HTTP::Server::Context)
        @block.call(context)
      end
    end
  end
end

require "./node"

module Radix
  # A Result is the comulative output of walking our [Radix tree](https://en.wikipedia.org/wiki/Radix_tree)
  # `Radix::Tree` implementation.
  #
  # It provides helpers to retrieve the information obtained from walking
  # our tree using `Radix::Tree#find`
  #
  # This information can be used to perform actions in case of the *path*
  # that was looked on the Tree was found.
  #
  # A Result is also used recursively by `Radix::Tree#find` when collecting
  # extra information like *params*.
  class Result(T)
    @key : String?

    getter params
    getter! payload : T?

    # :nodoc:
    def initialize
      @nodes = [] of Node(T)
      @params = {} of String => String
    end

    # Returns whatever a *payload* was found by `Tree#find` and is part of
    # the result.
    #
    # ```
    # result = Radix::Result(Symbol).new
    # result.found?
    # # => false
    #
    # root = Radix::Node(Symbol).new("/", :root)
    # result.use(root)
    # result.found?
    # # => true
    # ```
    def found?
      payload? ? true : false
    end

    # Returns a String built based on the nodes used in the result
    #
    # ```
    # node1 = Radix::Node(Symbol).new("/", :root)
    # node2 = Radix::Node(Symbol).new("about", :about)
    #
    # result = Radix::Result(Symbol).new
    # result.use node1
    # result.use node2
    #
    # result.key
    # # => "/about"
    # ```
    #
    # When no node has been used, returns an empty String.
    #
    # ```
    # result = Radix::Result(Nil).new
    # result.key
    # # => ""
    # ```
    def key
      @key ||= begin
        String.build { |io|
          @nodes.each do |node|
            io << node.key
          end
        }
      end
    end

    # Adjust result information by using the details of the given `Node`.
    #
    # * Collect `Node` for future references.
    # * Use *payload* if present.
    def use(node : Node(T), payload = true)
      # collect nodes
      @nodes << node

      if payload && node.payload?
        @payload = node.payload
      end
    end
  end
end

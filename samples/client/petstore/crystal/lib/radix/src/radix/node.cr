module Radix
  # A Node represents one element in the structure of a [Radix tree](https://en.wikipedia.org/wiki/Radix_tree)
  #
  # Carries a *payload* and might also contain references to other nodes
  # down in the organization inside *children*.
  #
  # Each node also carries identification in relation to the kind of key it
  # contains, which helps with characteristics of the node like named
  # parameters or catch all kind (globbing).
  #
  # Is not expected direct usage of a node but instead manipulation via
  # methods within `Tree`.
  class Node(T)
    include Comparable(self)

    # :nodoc:
    enum Kind : UInt8
      Normal
      Named
      Glob
    end

    getter key
    getter? placeholder
    property children = [] of Node(T)
    property! payload : T | Nil

    # :nodoc:
    protected getter kind = Kind::Normal

    # Returns the priority of the Node based on it's *key*
    #
    # This value will be directly associated to the key size up until a
    # special elements is found.
    #
    # ```
    # Radix::Node(Nil).new("a").priority
    # # => 1
    #
    # Radix::Node(Nil).new("abc").priority
    # # => 3
    #
    # Radix::Node(Nil).new("/src/*filepath").priority
    # # => 5
    #
    # Radix::Node(Nil).new("/search/:query").priority
    # # => 8
    # ```
    getter priority : Int32

    # Instantiate a Node
    #
    # - *key* - A `String` that represents this node.
    # - *payload* - An optional payload for this node.
    #
    # When *payload* is not supplied, ensure the type of the node is provided
    # instead:
    #
    # ```
    # # Good, node type is inferred from payload (Symbol)
    # node = Radix::Node.new("/", :root)
    #
    # # Good, node type is now Int32 but payload is optional
    # node = Radix::Node(Int32).new("/")
    #
    # # Error, node type cannot be inferred (compiler error)
    # node = Radix::Node.new("/")
    # ```
    def initialize(@key : String, @payload : T? = nil, @placeholder = false)
      @priority = compute_priority
    end

    # Compares this node against *other*, returning `-1`, `0` or `1` depending
    # on whether this node differentiates from *other*.
    #
    # Comparison is done combining node's `kind` and `priority`. Nodes of
    # same kind are compared by priority. Nodes of different kind are
    # ranked.
    #
    # ### Normal nodes
    #
    # ```
    # node1 = Radix::Node(Nil).new("a")  # normal
    # node2 = Radix::Node(Nil).new("bc") # normal
    # node1 <=> node2                    # => 1
    # ```
    #
    # ### Normal vs named or glob nodes
    #
    # ```
    # node1 = Radix::Node(Nil).new("a")         # normal
    # node2 = Radix::Node(Nil).new(":query")    # named
    # node3 = Radix::Node(Nil).new("*filepath") # glob
    # node1 <=> node2                           # => -1
    # node1 <=> node3                           # => -1
    # ```
    #
    # ### Named vs glob nodes
    #
    # ```
    # node1 = Radix::Node(Nil).new(":query")    # named
    # node2 = Radix::Node(Nil).new("*filepath") # glob
    # node1 <=> node2                           # => -1
    # ```
    def <=>(other : self)
      result = kind <=> other.kind
      return result if result != 0

      other.priority <=> priority
    end

    # Returns `true` if the node key contains a glob parameter in it
    # (catch all)
    #
    # ```
    # node = Radix::Node(Nil).new("*filepath")
    # node.glob? # => true
    #
    # node = Radix::Node(Nil).new("abc")
    # node.glob? # => false
    # ```
    def glob?
      kind.glob?
    end

    # Changes current *key*
    #
    # ```
    # node = Radix::Node(Nil).new("a")
    # node.key
    # # => "a"
    #
    # node.key = "b"
    # node.key
    # # => "b"
    # ```
    #
    # This will also result in change of node's `priority`
    #
    # ```
    # node = Radix::Node(Nil).new("a")
    # node.priority
    # # => 1
    #
    # node.key = "abcdef"
    # node.priority
    # # => 6
    # ```
    def key=(@key)
      # reset kind on change of key
      @kind = Kind::Normal
      @priority = compute_priority
    end

    # Returns `true` if the node key contains a named parameter in it
    #
    # ```
    # node = Radix::Node(Nil).new(":query")
    # node.named? # => true
    #
    # node = Radix::Node(Nil).new("abc")
    # node.named? # => false
    # ```
    def named?
      kind.named?
    end

    # Returns `true` if the node key does not contain an special parameter
    # (named or glob)
    #
    # ```
    # node = Radix::Node(Nil).new("a")
    # node.normal? # => true
    #
    # node = Radix::Node(Nil).new(":query")
    # node.normal? # => false
    # ```
    def normal?
      kind.normal?
    end

    # :nodoc:
    private def compute_priority
      reader = Char::Reader.new(@key)

      while reader.has_next?
        case reader.current_char
        when '*'
          @kind = Kind::Glob
          break
        when ':'
          @kind = Kind::Named
          break
        else
          reader.next_char
        end
      end

      reader.pos
    end

    # :nodoc:
    protected def sort!
      @children.sort!
    end
  end
end

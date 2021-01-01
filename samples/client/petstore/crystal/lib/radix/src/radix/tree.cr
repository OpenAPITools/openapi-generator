require "./node"
require "./result"

module Radix
  # A [Radix tree](https://en.wikipedia.org/wiki/Radix_tree) implementation.
  #
  # It allows insertion of *path* elements that will be organized inside
  # the tree aiming to provide fast retrieval options.
  #
  # Each inserted *path* will be represented by a `Node` or segmented and
  # distributed within the `Tree`.
  #
  # You can associate a *payload* at insertion which will be return back
  # at retrieval time.
  class Tree(T)
    # :nodoc:
    class DuplicateError < Exception
      def initialize(path)
        super("Duplicate trail found '#{path}'")
      end
    end

    # :nodoc:
    class SharedKeyError < Exception
      def initialize(new_key, existing_key)
        super("Tried to place key '#{new_key}' at same level as '#{existing_key}'")
      end
    end

    # Returns the root `Node` element of the Tree.
    #
    # On a new tree instance, this will be a placeholder.
    getter root : Node(T)

    def initialize
      @root = Node(T).new("", placeholder: true)
    end

    # Inserts given *path* into the Tree
    #
    # * *path* - An `String` representing the pattern to be inserted.
    # * *payload* - Required associated element for this path.
    #
    # If no previous elements existed in the Tree, this will replace the
    # defined placeholder.
    #
    # ```
    # tree = Radix::Tree(Symbol).new
    #
    # # /         (:root)
    # tree.add "/", :root
    #
    # # /         (:root)
    # # \-abc     (:abc)
    # tree.add "/abc", :abc
    #
    # # /         (:root)
    # # \-abc     (:abc)
    # #     \-xyz (:xyz)
    # tree.add "/abcxyz", :xyz
    # ```
    #
    # Nodes inside the tree will be adjusted to accommodate the different
    # segments of the given *path*.
    #
    # ```
    # tree = Radix::Tree(Symbol).new
    #
    # # / (:root)
    # tree.add "/", :root
    #
    # # /                   (:root)
    # # \-products/:id      (:product)
    # tree.add "/products/:id", :product
    #
    # # /                    (:root)
    # # \-products/
    # #           +-featured (:featured)
    # #           \-:id      (:product)
    # tree.add "/products/featured", :featured
    # ```
    #
    # Catch all (globbing) and named parameters *path* will be located with
    # lower priority against other nodes.
    #
    # ```
    # tree = Radix::Tree(Symbol).new
    #
    # # /           (:root)
    # tree.add "/", :root
    #
    # # /           (:root)
    # # \-*filepath (:all)
    # tree.add "/*filepath", :all
    #
    # # /           (:root)
    # # +-about     (:about)
    # # \-*filepath (:all)
    # tree.add "/about", :about
    # ```
    def add(path : String, payload : T)
      root = @root

      # replace placeholder with new node
      if root.placeholder?
        @root = Node(T).new(path, payload)
      else
        add path, payload, root
      end
    end

    # :nodoc:
    private def add(path : String, payload : T, node : Node(T))
      key_reader = Char::Reader.new(node.key)
      path_reader = Char::Reader.new(path)

      # move cursor position to last shared character between key and path
      while path_reader.has_next? && key_reader.has_next?
        break if path_reader.current_char != key_reader.current_char

        path_reader.next_char
        key_reader.next_char
      end

      # determine split point difference between path and key
      # compare if path is larger than key
      if path_reader.pos == 0 ||
         (path_reader.pos < path.bytesize && path_reader.pos >= node.key.bytesize)
        # determine if a child of this node contains the remaining part
        # of the path
        added = false

        new_key = path_reader.string.byte_slice(path_reader.pos)
        node.children.each do |child|
          # if child's key starts with named parameter, compare key until
          # separator (if present).
          # Otherwise, compare just first character
          if child.key[0]? == ':' && new_key[0]? == ':'
            unless _same_key?(new_key, child.key)
              raise SharedKeyError.new(new_key, child.key)
            end
          else
            next unless child.key[0]? == new_key[0]?
          end

          # when found, add to this child
          added = true
          add new_key, payload, child
          break
        end

        # if no existing child shared part of the key, add a new one
        unless added
          node.children << Node(T).new(new_key, payload)
        end

        # adjust priorities
        node.sort!
      elsif path_reader.pos == path.bytesize && path_reader.pos == node.key.bytesize
        # determine if path matches key and potentially be a duplicate
        # and raise if is the case

        if node.payload?
          raise DuplicateError.new(path)
        else
          # assign payload since this is an empty node
          node.payload = payload
        end
      elsif path_reader.pos > 0 && path_reader.pos < node.key.bytesize
        # determine if current node key needs to be split to accomodate new
        # children nodes

        # build new node with partial key and adjust existing one
        new_key = node.key.byte_slice(path_reader.pos)
        swap_payload = node.payload? ? node.payload : nil

        new_node = Node(T).new(new_key, swap_payload)
        new_node.children.replace(node.children)

        # clear payload and children (this is no longer and endpoint)
        node.payload = nil
        node.children.clear

        # adjust existing node key to new partial one
        node.key = path_reader.string.byte_slice(0, path_reader.pos)
        node.children << new_node
        node.sort!

        # determine if path still continues
        if path_reader.pos < path.bytesize
          new_key = path.byte_slice(path_reader.pos)
          node.children << Node(T).new(new_key, payload)
          node.sort!

          # clear payload (no endpoint)
          node.payload = nil
        else
          # this is an endpoint, set payload
          node.payload = payload
        end
      end
    end

    # Returns a `Result` instance after walking the tree looking up for
    # *path*
    #
    # It will start walking the tree from the root node until a matching
    # endpoint is found (or not).
    #
    # ```
    # tree = Radix::Tree(Symbol).new
    # tree.add "/about", :about
    #
    # result = tree.find "/products"
    # result.found?
    # # => false
    #
    # result = tree.find "/about"
    # result.found?
    # # => true
    #
    # result.payload
    # # => :about
    # ```
    def find(path : String)
      result = Result(T).new
      root = @root

      # walk the tree from root (first time)
      find path, result, root, first: true

      result
    end

    # :nodoc:
    private def find(path : String, result : Result, node : Node, first = false)
      # special consideration when comparing the first node vs. others
      # in case of node key and path being the same, return the node
      # instead of walking character by character
      if first && (path.bytesize == node.key.bytesize && path == node.key) && node.payload?
        result.use node
        return
      end

      key_reader = Char::Reader.new(node.key)
      path_reader = Char::Reader.new(path)

      # walk both path and key while both have characters and they continue
      # to match. Consider as special cases named parameters and catch all
      # rules.
      while key_reader.has_next? && path_reader.has_next? &&
            (key_reader.current_char == '*' ||
            key_reader.current_char == ':' ||
            path_reader.current_char == key_reader.current_char)
        case key_reader.current_char
        when '*'
          # deal with catch all (globbing) parameter
          # extract parameter name from key (exclude *) and value from path
          name = key_reader.string.byte_slice(key_reader.pos + 1)
          value = path_reader.string.byte_slice(path_reader.pos)

          # add this to result
          result.params[name] = value

          result.use node
          return
        when ':'
          # deal with named parameter
          # extract parameter name from key (from : until / or EOL) and
          # value from path (same rules as key)
          key_size = _detect_param_size(key_reader)
          path_size = _detect_param_size(path_reader)

          # obtain key and value using calculated sizes
          # for name: skip ':' by moving one character forward and compensate
          # key size.
          name = key_reader.string.byte_slice(key_reader.pos + 1, key_size - 1)
          value = path_reader.string.byte_slice(path_reader.pos, path_size)

          # add this information to result
          result.params[name] = value

          # advance readers positions
          key_reader.pos += key_size
          path_reader.pos += path_size
        else
          # move to the next character
          key_reader.next_char
          path_reader.next_char
        end
      end

      # check if we reached the end of the path & key
      if !path_reader.has_next? && !key_reader.has_next?
        # check endpoint
        if node.payload?
          result.use node
          return
        end
      end

      # still path to walk, check for possible trailing slash or children
      # nodes
      if path_reader.has_next?
        # using trailing slash?
        if node.key.bytesize > 0 &&
           path_reader.pos + 1 == path.bytesize &&
           path_reader.current_char == '/'
          result.use node
          return
        end

        # not found in current node, check inside children nodes
        new_path = path_reader.string.byte_slice(path_reader.pos)
        node.children.each do |child|
          # check if child key is a named parameter, catch all or shares parts
          # with new path
          if (child.key[0]? == '*' || child.key[0]? == ':') ||
             _shared_key?(new_path, child.key)
            # consider this node for key but don't use payload
            result.use node, payload: false

            find new_path, result, child
            return
          end
        end

        # path differs from key, no use searching anymore
        return
      end

      # key still contains characters to walk
      if key_reader.has_next?
        # determine if there is just a trailing slash?
        if key_reader.pos + 1 == node.key.bytesize &&
           key_reader.current_char == '/'
          result.use node
          return
        end

        # check if remaining part is catch all
        if key_reader.pos < node.key.bytesize &&
           ((key_reader.current_char == '/' && key_reader.peek_next_char == '*') ||
           key_reader.current_char == '*')
          # skip to '*' only if necessary
          unless key_reader.current_char == '*'
            key_reader.next_char
          end

          # deal with catch all, but since there is nothing in the path
          # return parameter as empty
          name = key_reader.string.byte_slice(key_reader.pos + 1)

          result.params[name] = ""

          result.use node
          return
        end
      end
    end

    # :nodoc:
    private def _detect_param_size(reader)
      # save old position
      old_pos = reader.pos

      # move forward until '/' or EOL is detected
      while reader.has_next?
        break if reader.current_char == '/'

        reader.next_char
      end

      # calculate the size
      count = reader.pos - old_pos

      # restore old position
      reader.pos = old_pos

      count
    end

    # Internal: allow inline comparison of *char* against 3 defined markers:
    #
    # - Path separator (`/`)
    # - Named parameter (`:`)
    # - Catch all (`*`)
    @[AlwaysInline]
    private def _check_markers(char)
      (char == '/' || char == ':' || char == '*')
    end

    # Internal: Compares *path* against *key* for differences until the
    # following criteria is met:
    #
    # - End of *path* or *key* is reached.
    # - A separator (`/`) is found.
    # - A character between *path* or *key* differs
    #
    # ```
    # _same_key?("foo", "bar")         # => false (mismatch at 1st character)
    # _same_key?("foo/bar", "foo/baz") # => true (only `foo` is compared)
    # _same_key?("zipcode", "zip")     # => false (`zip` is shorter)
    # ```
    private def _same_key?(path, key)
      path_reader = Char::Reader.new(path)
      key_reader = Char::Reader.new(key)

      different = false

      while (path_reader.has_next? && path_reader.current_char != '/') &&
            (key_reader.has_next? && key_reader.current_char != '/')
        if path_reader.current_char != key_reader.current_char
          different = true
          break
        end

        path_reader.next_char
        key_reader.next_char
      end

      (!different) &&
        (path_reader.current_char == '/' || !path_reader.has_next?)
    end

    # Internal: Compares *path* against *key* for equality until one of the
    # following criterias is met:
    #
    # - End of *path* or *key* is reached.
    # - A separator (`/`) is found.
    # - A named parameter (`:`) or catch all (`*`) is found.
    # - A character in *path* differs from *key*
    #
    # ```
    # _shared_key?("foo", "bar")         # => false (mismatch at 1st character)
    # _shared_key?("foo/bar", "foo/baz") # => true (only `foo` is compared)
    # _shared_key?("zipcode", "zip")     # => true (only `zip` is compared)
    # _shared_key?("s", "/new")          # => false (1st character is a separator)
    # ```
    private def _shared_key?(path, key)
      path_reader = Char::Reader.new(path)
      key_reader = Char::Reader.new(key)

      if (path_reader.current_char != key_reader.current_char) &&
         _check_markers(key_reader.current_char)
        return false
      end

      different = false

      while (path_reader.has_next? && !_check_markers(path_reader.current_char)) &&
            (key_reader.has_next? && !_check_markers(key_reader.current_char))
        if path_reader.current_char != key_reader.current_char
          different = true
          break
        end

        path_reader.next_char
        key_reader.next_char
      end

      (!different) &&
        (!key_reader.has_next? || _check_markers(key_reader.current_char))
    end

    # :nodoc:
    private def deprecation(message : String)
      STDERR.puts message
      STDERR.flush
    end
  end
end

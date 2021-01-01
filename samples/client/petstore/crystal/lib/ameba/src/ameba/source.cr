module Ameba
  # An entity that represents a Crystal source file.
  # Has path, lines of code and issues reported by rules.
  class Source
    include InlineComments
    include Reportable

    # Path to the source file.
    getter path : String

    # Crystal code (content of a source file).
    getter code : String

    @lines : Array(String)?
    @ast : Crystal::ASTNode?
    @fullpath : String?

    # Creates a new source by `code` and `path`.
    #
    # For example:
    #
    # ```
    # path = "./src/source.cr"
    # Ameba::Source.new File.read(path), path
    # ```
    #
    def initialize(@code : String, @path = "")
    end

    # Returns lines of code splitted by new line character.
    # Since `code` is immutable and can't be changed, this
    # method caches lines in an instance variable, so calling
    # it second time will not perform a split, but will return
    # lines instantly.
    #
    # ```
    # source = Ameba::Source.new "a = 1\nb = 2", path
    # source.lines # => ["a = 1", "b = 2"]
    # ```
    #
    def lines
      @lines ||= @code.split("\n")
    end

    # Returns AST nodes constructed by `Crystal::Parser`.
    #
    # ```
    # source = Ameba::Source.new code, path
    # source.ast
    # ```
    #
    def ast
      @ast ||=
        Crystal::Parser.new(code)
          .tap { |parser| parser.wants_doc = true }
          .tap { |parser| parser.filename = @path }
          .parse
    end

    def fullpath
      @fullpath ||= File.expand_path @path
    end

    # Returns true if *filepath* matches the source's path, false if it does not.
    def matches_path?(filepath)
      path == filepath || path == File.expand_path(filepath)
    end
  end
end

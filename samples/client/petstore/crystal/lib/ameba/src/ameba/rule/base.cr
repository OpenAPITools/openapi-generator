module Ameba::Rule
  # List of names of the special rules, which
  # behave differently than usual rules.
  SPECIAL = [
    Lint::Syntax.rule_name,
    Lint::UnneededDisableDirective.rule_name,
  ]

  # Represents a base of all rules. In other words, all rules
  # inherits from this struct:
  #
  # ```
  # struct MyRule < Ameba::Rule::Base
  #   def test(source)
  #     if invalid?(source)
  #       issue_for line, column, "Something wrong."
  #     end
  #   end
  #
  #   private def invalid?(source)
  #     # ...
  #   end
  # end
  # ```
  #
  # Enforces rules to implement an abstract `#test` method which
  # is designed to test the source passed in. If source has issues
  # that are tested by this rule, it should add an issue.
  #
  abstract struct Base
    include Config::RuleConfig

    # This method is designed to test the source passed in. If source has issues
    # that are tested by this rule, it should add an issue.
    #
    # Be default it uses a node visitor to traverse all the nodes in the source.
    # Must be overriten for other type of rules.
    def test(source : Source)
      AST::NodeVisitor.new self, source
    end

    def test(source : Source, node : Crystal::ASTNode, *opts)
      # can't be abstract
    end

    # A convenient addition to `#test` method that does the same
    # but returns a passed in `source` as an addition.
    #
    # ```
    # source = MyRule.new.catch(source)
    # source.valid?
    # ```
    #
    def catch(source : Source)
      source.tap { |s| test s }
    end

    # Returns a name of this rule, which is basically a class name.
    #
    # ```
    # struct MyRule < Ameba::Rule::Base
    #   def test(source)
    #   end
    # end
    #
    # MyRule.new.name # => "MyRule"
    # ```
    #
    def name
      {{@type}}.rule_name
    end

    # Returns a group this rule belong to.
    #
    # ```
    # struct MyGroup::MyRule < Ameba::Rule::Base
    #   # ...
    # end
    #
    # MyGroup::MyRule.new.group # => "MyGroup"
    # ```
    #
    def group
      {{@type}}.group_name
    end

    # Checks whether the source is excluded from this rule.
    # It searches for a path in `excluded` property which matches
    # the one of the given source.
    #
    # ```
    # my_rule.excluded?(source) # => true or false
    # ```
    #
    def excluded?(source)
      excluded.try &.any? do |path|
        source.matches_path?(path) ||
          Dir.glob(path).any? { |glob| source.matches_path? glob }
      end
    end

    # Returns true if this rule is special and behaves differently than
    # usual rules.
    #
    # ```
    # my_rule.special? # => true or false
    # ```
    #
    def special?
      SPECIAL.includes? name
    end

    def ==(other)
      name == other.try &.name
    end

    def hash
      name.hash
    end

    macro issue_for(*args)
      source.add_issue self, {{*args}}
    end

    protected def self.rule_name
      name.gsub("Ameba::Rule::", "").gsub("::", "/")
    end

    protected def self.group_name
      rule_name.split("/")[0...-1].join("/")
    end

    protected def self.subclasses
      {{ @type.subclasses }}
    end

    macro inherited
      protected def self.path_to_source_file
        __FILE__
      end
    end

    # Returns documentation for this rule if any.
    #
    # ```
    # module Ameba
    #   # This is a test rule.
    #   # Does nothing.
    #   struct MyRule < Ameba::Rule::Base
    #     def test(source)
    #     end
    #   end
    # end
    #
    # MyRule.parsed_doc # => "This is a test rule.\nDoes nothing."
    # ```
    def self.parsed_doc
      source = File.read(path_to_source_file)
      nodes = Crystal::Parser.new(source).tap(&.wants_doc = true).parse
      type_name = rule_name.split("/").last?
      DocFinder.new(nodes, type_name).doc
    end

    # :nodoc:
    private class DocFinder < Crystal::Visitor
      getter doc : String?
      getter type_name : String?

      def initialize(nodes, @type_name)
        self.accept(nodes)
      end

      def visit(node : Crystal::ASTNode)
        return false if @doc

        if node.responds_to?(:name) &&
           (name = node.name) &&
           name.is_a?(Crystal::Path) &&
           name.names.last? == @type_name
          @doc = node.doc
        end

        true
      end
    end
  end

  # Returns a list of all available rules.
  #
  # ```
  # Ameba::Rule.rules # => [Rule1, Rule2, ....]
  # ```
  #
  def self.rules
    Base.subclasses
  end
end

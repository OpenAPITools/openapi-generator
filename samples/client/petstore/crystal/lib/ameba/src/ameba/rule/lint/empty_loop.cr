module Ameba::Rule::Lint
  # A rule that disallows empty loops.
  #
  # This is considered invalid:
  #
  # ```
  # while false
  # end
  #
  # until 10
  # end
  #
  # loop do
  #   # nothing here
  # end
  # ```
  #
  # And this is valid:
  #
  # ```
  # a = 1
  # while a < 10
  #   a += 1
  # end
  #
  # until socket_opened?
  # end
  #
  # loop do
  #   do_something_here
  # end
  # ```
  #
  # YAML configuration example:
  #
  # ```
  # Lint/EmptyLoop:
  #   Enabled: true
  # ```
  struct EmptyLoop < Base
    include AST::Util

    properties do
      description "Disallows empty loops"
    end

    MSG = "Empty loop detected"

    def test(source, node : Crystal::Call)
      return unless loop?(node)

      check_node(source, node, node.block)
    end

    def test(source, node : Crystal::While)
      check_node(source, node, node.body) if literal?(node.cond)
    end

    def test(source, node : Crystal::Until)
      check_node(source, node, node.body) if literal?(node.cond)
    end

    private def check_node(source, node, loop_body)
      body = loop_body.is_a?(Crystal::Block) ? loop_body.body : loop_body
      issue_for node, MSG if body.nil? || body.nop?
    end
  end
end

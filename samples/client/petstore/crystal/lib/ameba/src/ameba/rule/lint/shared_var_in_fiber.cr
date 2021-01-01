module Ameba::Rule::Lint
  # A rule that disallows using shared variables in fibers,
  # which are mutated during iterations.
  #
  # In most cases it leads to unexpected behaviour and is undesired.
  #
  # For example, having this example:
  #
  # ```
  # n = 0
  # channel = Channel(Int32).new
  #
  # while n < 3
  #   n = n + 1
  #   spawn { channel.send n }
  # end
  #
  # 3.times { puts channel.receive } # => # 3, 3, 3
  # ```
  #
  # The problem is there is only one shared between fibers variable `n`
  # and when `channel.receive` is executed its value is `3`.
  #
  # To solve this, the code above needs to be rewritten to the following:
  #
  # ```
  # n = 0
  # channel = Channel(Int32).new
  #
  # while n < 3
  #   n = n + 1
  #   m = n
  #   spawn do { channel.send m }
  # end
  #
  # 3.times { puts channel.receive } # => # 1, 2, 3
  # ```
  #
  # This rule is able to find the shared variables between fibers, which are mutated
  # during iterations. So it reports the issue on the first sample and passes on
  # the second one.
  #
  # There are also other technics to solve the problem above which are
  # [officially documented](https://crystal-lang.org/reference/guides/concurrency.html)
  #
  # YAML configuration example:
  #
  # ```
  # Lint/SharedVarInFiber:
  #   Enabled: true
  # ```
  #
  struct SharedVarInFiber < Base
    properties do
      description "Disallows shared variables in fibers."
    end

    MSG = "Shared variable `%s` is used in fiber"

    def test(source)
      AST::ScopeVisitor.new self, source
    end

    def test(source, node, scope : AST::Scope)
      return unless scope.spawn_block?

      scope.references.each do |ref|
        next if (variable = scope.find_variable(ref.name)).nil?
        next if variable.scope == scope || !mutated_in_loop?(variable)

        issue_for ref.node, MSG % variable.name
      end
    end

    # Variable is mutated in loop if it was declared above the loop and assigned inside.
    private def mutated_in_loop?(variable)
      declared_in = variable.assignments.first?.try &.branch

      variable.assignments
        .reject { |assign| assign.scope.spawn_block? }
        .any? do |assign|
          assign.branch.try(&.in_loop?) && assign.branch != declared_in
        end
    end
  end
end

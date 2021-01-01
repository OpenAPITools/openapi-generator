module Ameba::Rule::Style
  # A rule that disallows tautological predicate names, meaning those that
  # start with the prefix `has_` or the prefix `is_`. Ignores if the alternative isn't valid Crystal code (e.g. `is_404?`).
  #
  # Favour these:
  #
  # ```
  # def valid?(x)
  # end
  #
  # def picture?(x)
  # end
  # ```
  #
  # Over these:
  #
  # ```
  # def is_valid?(x)
  # end
  #
  # def has_picture?(x)
  # end
  # ```
  #
  # YAML configuration example:
  #
  # ```
  # Style/PredicateName:
  #   Enabled: true
  # ```
  #
  struct PredicateName < Base
    properties do
      description "Disallows tautological predicate names"
      enabled false
    end

    MSG = "Favour method name '%s?' over '%s'"

    def test(source, node : Crystal::Def)
      if node.name =~ /^(is|has)_(\w+)\?/
        alternative = $2
        return unless alternative =~ /^[a-z][a-zA-Z_0-9]*$/

        issue_for node, MSG % {alternative, node.name}
      end
    end
  end
end

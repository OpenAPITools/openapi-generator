module Ameba::Rule::Lint
  # A rule that disallows duplicated keys in hash literals.
  #
  # This is considered invalid:
  #
  # ```
  # h = {"foo" => 1, "bar" => 2, "foo" => 3}
  # ```
  #
  # And it has to written as this instead:
  #
  # ```
  # h = {"foo" => 1, "bar" => 2}
  # ```
  #
  # YAML configuration example:
  #
  # ```
  # Lint/HashDuplicatedKey:
  #   Enabled: true
  # ```
  #
  struct HashDuplicatedKey < Base
    properties do
      description "Disallows duplicated keys in hash literals"
    end

    MSG = "Duplicated keys in hash literal: %s"

    def test(source, node : Crystal::HashLiteral)
      return unless (keys = duplicated_keys(node.entries)).any?

      issue_for node, MSG % keys.join(", ")
    end

    private def duplicated_keys(entries)
      entries.map(&.key)
        .group_by(&.itself)
        .select { |_, v| v.size > 1 }
        .map { |k, _| k }
    end
  end
end

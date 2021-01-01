module Ameba::Rule::Lint
  # A rule that disallows redundant `each_with_object` calls.
  #
  # For example, this is considered invalid:
  #
  # ```
  # collection.each_with_object(0) do |e|
  #   # ...
  # end
  #
  # collection.each_with_object(0) do |e, _|
  #   # ...
  # end
  # ```
  #
  # and it should be written as follows:
  #
  # ```
  # collection.each do |e|
  #   # ...
  # end
  # ```
  #
  # YAML configuration example:
  #
  # ```
  # Lint/RedundantWithObject:
  #   Enabled: true
  # ```
  #
  struct RedundantWithObject < Base
    properties do
      description "Disallows redundant `with_object` calls"
    end

    def test(source, node : Crystal::Call)
      return if node.name != "each_with_object" ||
                node.args.size != 1 ||
                node.block.nil? ||
                with_index_arg?(node.block.not_nil!)

      issue_for node.name_location,
        node.name_end_location,
        "Use each instead of each_with_object"
    end

    private def with_index_arg?(block : Crystal::Block)
      block.args.size >= 2 && block.args.last.name != "_"
    end
  end
end

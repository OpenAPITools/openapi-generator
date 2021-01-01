module Ameba::Rule::Lint
  # A rule that disallows redundant `with_index` calls.
  #
  # For example, this is considered invalid:
  # ```
  # collection.each.with_index do |e|
  #   # ...
  # end
  #
  # collection.each_with_index do |e, _|
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
  # Lint/RedundantWithIndex:
  #   Enabled: true
  # ```
  #
  struct RedundantWithIndex < Base
    properties do
      description "Disallows redundant `with_index` calls"
    end

    def test(source, node : Crystal::Call)
      args, block = node.args, node.block

      return if args.size > 1 || block.nil? || with_index_arg?(block.not_nil!)

      case node.name
      when "with_index"
        report source, node, "Remove redundant with_index"
      when "each_with_index"
        report source, node, "Use each instead of each_with_index"
      else
        # nop
      end
    end

    private def with_index_arg?(block : Crystal::Block)
      block.args.size >= 2 && block.args.last.name != "_"
    end

    private def report(source, node, msg)
      issue_for node.name_location, node.name_end_location, msg
    end
  end
end

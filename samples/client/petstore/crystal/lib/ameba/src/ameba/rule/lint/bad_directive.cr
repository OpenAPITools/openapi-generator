module Ameba::Rule::Lint
  # A rule that reports incorrect comment directives for Ameba.
  #
  # For example, the user can mistakenly add a directive
  # to disable a rule that even doesn't exist:
  #
  # ```
  # # ameba:disable BadRuleName
  # def foo
  #   :bar
  # end
  # ```
  #
  # YAML configuration example:
  #
  # ```
  # Lint/BadDirective:
  #   Enabled: true
  # ```
  #
  struct BadDirective < Base
    properties do
      description "Reports bad comment directives"
    end

    AVAILABLE_ACTIONS = InlineComments::Action.names.map(&.downcase)
    ALL_RULE_NAMES    = Rule.rules.map(&.rule_name)
    ALL_GROUP_NAMES   = Rule.rules.map(&.group_name).uniq!

    def test(source)
      Tokenizer.new(source).run do |token|
        next unless token.type == :COMMENT
        next unless directive = source.parse_inline_directive(token.value.to_s)

        check_action source, token, directive[:action]
        check_rules source, token, directive[:rules]
      end
    end

    private def check_action(source, token, action)
      return if InlineComments::Action.parse?(action)

      issue_for token,
        "Bad action in comment directive: '%s'. Possible values: %s" % {
          action, AVAILABLE_ACTIONS.join(", "),
        }
    end

    private def check_rules(source, token, rules)
      bad_names = rules - ALL_RULE_NAMES - ALL_GROUP_NAMES
      issue_for token, "Such rules do not exist: %s" % bad_names.join(", ") unless bad_names.empty?
    end
  end
end

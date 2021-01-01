module Ameba
  # A module that utilizes inline comments parsing and processing logic.
  module InlineComments
    COMMENT_DIRECTIVE_REGEX = /# ameba:(?<action>\w+) (?<rules>\w+(?:\/\w+)?(?:,? \w+(?:\/\w+)?)*)/

    # Available actions in the inline comments
    enum Action
      Disable
      Enable
    end

    # Returns true if current location is disabled for a particular rule,
    # false otherwise.
    #
    # Location is disabled in two cases:
    #   1. The line of the location ends with a comment directive.
    #   2. The line above the location is a comment directive.
    #
    # For example, here are two examples of disabled location:
    #
    # ```
    # # ameba:disable Style/LargeNumbers
    # Time.epoch(1483859302)
    #
    # Time.epoch(1483859302) # ameba:disable Style/LargeNumbers
    # ```
    #
    # But here are examples which are not considered as disabled location:
    #
    # ```
    # # ameba:disable Style/LargeNumbers
    # #
    # Time.epoch(1483859302)
    #
    # if use_epoch? # ameba:disable Style/LargeNumbers
    #   Time.epoch(1483859302)
    # end
    # ```
    #
    def location_disabled?(location, rule)
      return false if Rule::SPECIAL.includes?(rule.name)
      return false unless line_number = location.try &.line_number.try &.- 1
      return false unless line = lines[line_number]?

      line_disabled?(line, rule) ||
        (line_number > 0 &&
          (prev_line = lines[line_number - 1]) &&
          comment?(prev_line) &&
          line_disabled?(prev_line, rule))
    end

    # Parses inline comment directive. Returns a tuple that consists of
    # an action and parsed rules if directive found, nil otherwise.
    #
    # ```
    # line = "# ameba:disable Rule1, Rule2"
    # directive = parse_inline_directive(line)
    # directive[:action] # => "disable"
    # directive[:rules]  # => ["Rule1", "Rule2"]
    # ```
    #
    # It ignores the directive if it is commented out.
    #
    # ```
    # line = "# # ameba:disable Rule1, Rule2"
    # parse_inline_directive(line) # => nil
    # ```
    #
    def parse_inline_directive(line)
      if directive = COMMENT_DIRECTIVE_REGEX.match(line)
        return if commented_out?(line.gsub(directive[0], ""))
        {
          action: directive["action"],
          rules:  directive["rules"].split(/[\s,]/, remove_empty: true),
        }
      end
    end

    # Returns true if the line at the given `line_number` is a comment.
    def comment?(line_number : Int32)
      if line = lines[line_number]?
        comment?(line)
      end
    end

    private def comment?(line : String)
      line.lstrip.starts_with? '#'
    end

    private def line_disabled?(line, rule)
      return false unless directive = parse_inline_directive(line)
      Action.parse?(directive[:action]).try(&.disable?) &&
        (directive[:rules].includes?(rule.name) || directive[:rules].includes?(rule.group))
    end

    private def commented_out?(line)
      commented = false

      lexer = Crystal::Lexer.new(line).tap(&.comments_enabled = true)
      Tokenizer.new(lexer).run { |t| commented = true if t.type == :COMMENT }
      commented
    end
  end
end

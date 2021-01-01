module Ameba::Rule::Style
  # A rule that disallows usage of large numbers without underscore.
  # These do not affect the value of the number, but can help read
  # large numbers more easily.
  #
  # For example, these are considered invalid:
  #
  # ```
  # 10000
  # 141592654
  # 5.12345
  # ```
  #
  # And has to be rewritten as the following:
  #
  # ```
  # 10_000
  # 141_592_654
  # 5.123_45
  # ```
  #
  # YAML configuration example:
  #
  # ```
  # Style/LargeNumbers:
  #   Enabled: true
  #   IntMinDigits: 5 # i.e. integers higher than 9999
  # ```
  #
  struct LargeNumbers < Base
    properties do
      description "Disallows usage of large numbers without underscore"
      int_min_digits 5
      enabled false
    end

    MSG = "Large numbers should be written with underscores: %s"

    def test(source)
      Tokenizer.new(source).run do |token|
        next unless token.type == :NUMBER && decimal?(token.raw)

        parsed = parse_number token.raw

        if allowed?(*parsed) && (expected = underscored *parsed) != token.raw
          issue_for token, MSG % expected
        end
      end
    end

    private def decimal?(value)
      value !~ /^0(x|b|o)/
    end

    private def allowed?(_sign, value, fraction, _suffix)
      return true if !fraction.nil? && fraction.size > 3

      digits = value.chars.select &.to_s.=~ /[0-9]/
      digits.size >= int_min_digits
    end

    private def underscored(sign, value, fraction, suffix)
      value = slice_digits(value.reverse) { |slice| slice }.reverse
      fraction = "." + slice_digits(fraction) { |slice| slice } if fraction

      "#{sign}#{value}#{fraction}#{suffix}"
    end

    private def slice_digits(value, by = 3)
      ([] of String).tap do |slices|
        value.chars.reject(&.== '_').each_slice(by) do |slice|
          slices << (yield slice).join
        end
      end.join("_")
    end

    private def parse_number(value)
      value, sign = parse_sign(value)
      value, suffix = parse_suffix(value)
      value, fraction = parse_fraction(value)

      {sign, value, fraction, suffix}
    end

    private def parse_sign(value)
      if "+-".includes?(value[0])
        sign = value[0]
        value = value[1..-1]
      end
      {value, sign}
    end

    private def parse_suffix(value)
      if pos = (value =~ /e/ || value =~ /_?(i|u|f)/)
        suffix = value[pos..-1]
        value = value[0..pos - 1]
      end
      {value, suffix}
    end

    private def parse_fraction(value)
      if comma = value.index('.')
        fraction = value[comma + 1..-1]
        value = value[0..comma - 1]
      end
      {value, fraction}
    end
  end
end

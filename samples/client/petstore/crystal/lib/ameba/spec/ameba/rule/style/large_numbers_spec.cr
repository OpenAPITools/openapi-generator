require "../../../spec_helper"

module Ameba
  subject = Rule::Style::LargeNumbers.new

  private def it_transforms(number, expected)
    it "transforms large number #{number}" do
      s = Source.new number
      Rule::Style::LargeNumbers.new.catch(s).should_not be_valid
      s.issues.first.message.should contain expected
    end
  end

  describe Rule::Style::LargeNumbers do
    it "passes if large number does not require underscore" do
      s = Source.new %q(
        1 2 3 4 5 6 7 8 9 10 11 12 13 14 15
        16 17 18 19 20 30 40 50 60 70 80 90
        100
        999
        1000
        1_000
        9999
        9_999
        10_000
        100_000
        200_000
        300_000
        400_000
        500_000
        600_000
        700_000
        800_000
        900_000
        1_000_000

       -9_223_372_036_854_775_808
        9_223_372_036_854_775_807

        141_592_654
        141_592_654.0
        141_592_654.001
        141_592_654.001_2
        141_592_654.001_23
        141_592_654.001_234
        141_592_654.001_234_5

        0b1101
        0o123
        0xFE012D
        0xfe012d
        0xfe012dd11

        1_i8
        12_i16
        123_i32
        1_234_i64

        12_u8
        123_u16
        1_234_u32
        9_223_372_036_854_775_808_u64
        9_223_372_036_854_775_808.000_123_456_789_f64

        +100_u32
        -900_000_i32

        1_234.5e-7
        11_234e10_f32
        +1.123
        -0.000_5

        1200.0
        1200.01
        1200.012
      )
      subject.catch(s).should be_valid
    end

    it_transforms "10000", "10_000"
    it_transforms "+10000", "+10_000"
    it_transforms "-10000", "-10_000"

    it_transforms "9223372036854775808", "9_223_372_036_854_775_808"
    it_transforms "-9223372036854775808", "-9_223_372_036_854_775_808"
    it_transforms "+9223372036854775808", "+9_223_372_036_854_775_808"

    it_transforms "1_00000", "100_000"

    it_transforms "10000_i16", "10_000_i16"
    it_transforms "10000_i32", "10_000_i32"
    it_transforms "10000_i64", "10_000_i64"

    it_transforms "10000_u16", "10_000_u16"
    it_transforms "10000_u32", "10_000_u32"
    it_transforms "10000_u64", "10_000_u64"

    it_transforms "123456_f32", "123_456_f32"
    it_transforms "123456_f64", "123_456_f64"

    it_transforms "123456.5e-7_f32", "123_456.5e-7_f32"
    it_transforms "123456e10_f64", "123_456e10_f64"

    it_transforms "123456.5e-7", "123_456.5e-7"
    it_transforms "123456e10", "123_456e10"

    it_transforms "3.00_1", "3.001"
    it_transforms "3.0012", "3.001_2"
    it_transforms "3.00123", "3.001_23"
    it_transforms "3.001234", "3.001_234"
    it_transforms "3.0012345", "3.001_234_5"

    it "reports rule, pos and message" do
      s = Source.new %q(
        1200000
      ), "source.cr"
      subject.catch(s).should_not be_valid
      issue = s.issues.first
      issue.rule.should_not be_nil
      issue.location.to_s.should eq "source.cr:1:1"
      issue.end_location.should be_nil
      issue.message.should match /1_200_000/
    end

    context "properties" do
      it "allows to configure integer min digits" do
        s = Source.new %q(1200000)
        rule = Rule::Style::LargeNumbers.new
        rule.int_min_digits = 10
        rule.catch(s).should be_valid
      end
    end
  end
end

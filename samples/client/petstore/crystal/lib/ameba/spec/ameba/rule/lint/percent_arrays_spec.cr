require "../../../spec_helper"

module Ameba::Rule::Lint
  describe PercentArrays do
    subject = PercentArrays.new

    it "passes if percent arrays are written correctly" do
      s = Source.new %q(
        %i(one two three)
        %w(one two three)

        %i(1 2 3)
        %w(1 2 3)

        %i()
        %w()
      )
      subject.catch(s).should be_valid
    end

    it "fails if string percent array has commas" do
      s = Source.new %( %w(one, two) )
      subject.catch(s).should_not be_valid
    end

    it "fails if string percent array has quotes" do
      s = Source.new %( %w("one" "two") )
      subject.catch(s).should_not be_valid
    end

    it "fails if symbols percent array has commas" do
      s = Source.new %( %i(one, two) )
      subject.catch(s).should_not be_valid
    end

    it "fails if symbols percent array has a colon" do
      s = Source.new %( %i(:one :two) )
      subject.catch(s).should_not be_valid
    end

    it "reports rule, location and message for %i" do
      s = Source.new %(
        %i(:one)
      ), "source.cr"

      subject.catch(s).should_not be_valid
      issue = s.issues.first
      issue.rule.should_not be_nil
      issue.location.to_s.should eq "source.cr:1:1"
      issue.message.should eq(
        "Symbols `,:` may be unwanted in %i array literals"
      )
    end

    it "reports rule, location and message for %w" do
      s = Source.new %(
        %w("one")
      ), "source.cr"

      subject.catch(s).should_not be_valid
      issue = s.issues.first
      issue.rule.should_not be_nil
      issue.location.to_s.should eq "source.cr:1:1"
      issue.end_location.should be_nil
      issue.message.should eq(
        "Symbols `,\"` may be unwanted in %w array literals"
      )
    end

    context "properties" do
      it "allows to configure string_array_unwanted_symbols" do
        rule = PercentArrays.new
        rule.string_array_unwanted_symbols = ","
        s = Source.new %( %w("one") )
        rule.catch(s).should be_valid
      end

      it "allows to configure symbol_array_unwanted_symbols" do
        rule = PercentArrays.new
        rule.symbol_array_unwanted_symbols = ","
        s = Source.new %( %i(:one) )
        rule.catch(s).should be_valid
      end
    end
  end
end

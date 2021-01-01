require "../../../spec_helper"

module Ameba
  subject = Rule::Lint::EmptyExpression.new

  def it_detects_empty_expression(code)
    it "detects empty expression" do
      s = Source.new code
      rule = Rule::Lint::EmptyExpression.new
      rule.catch(s).should_not be_valid
    end
  end

  describe Rule::Lint::EmptyExpression do
    it "passes if there is no empty expression" do
      s = Source.new %(
        def method()
        end

        method()
        method(1, 2, 3)
        method(nil)

        a = nil
        a = ""
        a = 0

        nil
        :any.nil?

        begin "" end
        [nil] << nil
      )
      subject.catch(s).should be_valid
    end

    it_detects_empty_expression %(())
    it_detects_empty_expression %(((())))
    it_detects_empty_expression %(a = ())
    it_detects_empty_expression %((();()))
    it_detects_empty_expression %(if (); end)
    it_detects_empty_expression %(
      if foo
        1
      elsif ()
        2
      end
    )
    it_detects_empty_expression %(
      case foo
      when :foo then ()
      end
    )
    it_detects_empty_expression %(
      case foo
      when :foo then 1
      else
        ()
      end
    )
    it_detects_empty_expression %(
      case foo
      when () then 1
      end
    )
    it_detects_empty_expression %(
      def method
        a = 1
        ()
      end
    )
    it_detects_empty_expression %(
      def method
      rescue
        ()
      end
    )
    it_detects_empty_expression %(
      def method
        begin
        end
      end
    )
    it_detects_empty_expression %(
      begin; end
    )
    it_detects_empty_expression %(
      begin
        nil
      end
    )
    it_detects_empty_expression %(
      begin
        ()
      end
    )

    it "does not report emtpy expression in macro" do
      s = Source.new %q(
        module MyModule
          macro conditional_error_for_inline_callbacks
            \{%
              raise ""
            %}
          end

          macro before_save(x = nil)
          end
        end
      )
      subject.catch(s).should be_valid
    end

    it "reports rule, location and message" do
      s = Source.new %(
        if ()
        end
      ), "source.cr"
      subject.catch(s).should_not be_valid
      issue = s.issues.first
      issue.rule.should_not be_nil
      issue.location.to_s.should eq "source.cr:1:4"
      issue.end_location.to_s.should eq "source.cr:1:5"
      issue.message.should eq "Avoid empty expressions"
    end
  end
end

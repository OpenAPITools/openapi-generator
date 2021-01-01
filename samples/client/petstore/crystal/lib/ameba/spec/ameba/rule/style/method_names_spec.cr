require "../../../spec_helper"

module Ameba
  subject = Rule::Style::MethodNames.new

  private def it_reports_method_name(code, expected)
    it "reports method name #{expected}" do
      s = Source.new code
      Rule::Style::MethodNames.new.catch(s).should_not be_valid
      s.issues.first.message.should contain expected
    end
  end

  describe Rule::Style::MethodNames do
    it "passes if method names are underscore-cased" do
      s = Source.new %(
        class Person
          def first_name
          end

          def date_of_birth
          end

          def homepage_url
          end

          def valid?
          end

          def name
          end
        end
      )
      subject.catch(s).should be_valid
    end

    it_reports_method_name %(def firstName; end), "first_name"
    it_reports_method_name %(def date_of_Birth; end), "date_of_birth"
    it_reports_method_name %(def homepageURL; end), "homepage_url"

    it "reports rule, pos and message" do
      s = Source.new %(
        def bad_Name(a)
        end
      ), "source.cr"
      subject.catch(s).should_not be_valid
      issue = s.issues.first
      issue.rule.should_not be_nil
      issue.location.to_s.should eq "source.cr:1:5"
      issue.end_location.to_s.should eq "source.cr:1:12"
      issue.message.should eq(
        "Method name should be underscore-cased: bad_name, not bad_Name"
      )
    end
  end
end

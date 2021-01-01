require "../../../spec_helper"

module Ameba::Rule::Metrics
  subject = CyclomaticComplexity.new
  complex_method = <<-CODE
    def hello(a, b, c)
      if a && b && c
        begin
          while true
            return if false && b
          end
          ""
        rescue
          ""
        end
      end
    end
  CODE

  describe CyclomaticComplexity do
    it "passes for empty methods" do
      source = Source.new %(
        def hello
        end
      )
      subject.catch(source).should be_valid
    end

    it "reports one issue for a complex method" do
      subject.max_complexity = 5
      source = Source.new(complex_method, "source.cr")
      subject.catch(source).should_not be_valid

      issue = source.issues.first
      issue.rule.should eq subject
      issue.location.to_s.should eq "source.cr:1:5"
      issue.end_location.to_s.should eq "source.cr:1:10"
      issue.message.should eq "Cyclomatic complexity too high [8/5]"
    end

    it "doesn't report an issue for an increased threshold" do
      subject.max_complexity = 100
      source = Source.new(complex_method, "source.cr")
      subject.catch(source).should be_valid
    end
  end
end

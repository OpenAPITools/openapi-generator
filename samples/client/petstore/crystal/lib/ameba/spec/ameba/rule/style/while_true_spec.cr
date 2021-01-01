require "../../../spec_helper"

valid_source = <<-EOF
a = 1
loop do
  a += 1
  break if a > 5
end
EOF

invalid_source = <<-EOF
a = 1
while true
  a += 1
  break if a > 5
end
EOF

module Ameba::Rule::Style
  subject = WhileTrue.new

  describe WhileTrue do
    it "passes if there is no `while true`" do
      source = Source.new valid_source
      subject.catch(source).should be_valid
    end

    it "fails if there is `while true`" do
      source = Source.new invalid_source
      subject.catch(source).should_not be_valid
    end

    it "reports rule, pos and message" do
      source = Source.new invalid_source, "source.cr"
      subject.catch(source).should_not be_valid

      issue = source.issues.first
      issue.location.to_s.should eq "source.cr:2:1"
      issue.end_location.to_s.should eq "source.cr:5:3"
      issue.message.should eq "While statement using true literal as condition"
    end
  end
end

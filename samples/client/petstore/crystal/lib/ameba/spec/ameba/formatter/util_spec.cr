require "../../spec_helper"

module Ameba::Formatter
  class Subject
    include Util
  end

  subject = Subject.new

  describe Util do
    describe "#affected_code" do
      it "returns nil if there is no such a line number" do
        source = Source.new %(
          a = 1
        )
        location = Crystal::Location.new("filename", 2, 1)
        subject.affected_code(source, location).should be_nil
      end

      it "returns correct line if it is found" do
        source = Source.new %(
          a = 1
        )
        location = Crystal::Location.new("filename", 1, 1)
        subject.affected_code(source, location).should eq "> a = 1\n  \e[33m^\e[0m"
      end
    end
  end
end

require "../spec_helper"

module Ameba
  describe Source do
    describe ".new" do
      it "allows to create a source by code and path" do
        s = Source.new("code", "path")
        s.path.should eq "path"
        s.code.should eq "code"
        s.lines.should eq ["code"]
      end
    end

    describe "#fullpath" do
      it "returns a relative path of the source" do
        s = Source.new "", "./source_spec.cr"
        s.fullpath.should contain "source_spec.cr"
      end

      it "returns fullpath if path is blank" do
        s = Source.new "", ""
        s.fullpath.should_not be_nil
      end
    end

    describe "#matches_path?" do
      it "returns true if source's path is matched" do
        s = Source.new "", "source.cr"
        s.matches_path?("source.cr").should be_true
      end

      it "returns false if source's path is not matched" do
        s = Source.new "", "source.cr"
        s.matches_path?("new_source.cr").should be_false
      end
    end
  end
end

require "../spec_helper"
require "../../src/slang"

class SlangView
  Kilt.file "spec/fixtures/test.slang"
end

describe "kilt/slang" do

  it "renders slang" do
    Kilt.render("spec/fixtures/test.slang").should eq("<span>#{Process.pid}</span>")
  end

  it "works with classes" do
    SlangView.new.to_s.should eq("<span>#{Process.pid}</span>")
  end

end

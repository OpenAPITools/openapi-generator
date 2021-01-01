require "./spec_helper"

class View
  Kilt.file "spec/fixtures/test.ecr"
end

describe Kilt do

  it "renders ecr" do
    Kilt.render("spec/fixtures/test.ecr").should eq("<span>#{Process.pid}</span>")
  end

  it "works with classes" do
    View.new.to_s.should eq("<span>#{Process.pid}</span>")
  end

  it "raises with unsupported filetype" do
    expect_raises(Kilt::Exception, "Unsupported template engine for extension: \"abc\"") {
      Kilt.render("test.abc")
    }
  end

  it "renders registered engine" do
    Kilt.register_engine "raw", Raw.embed
    Kilt.render("spec/fixtures/test.raw").should eq("Hello World!")
  end

end

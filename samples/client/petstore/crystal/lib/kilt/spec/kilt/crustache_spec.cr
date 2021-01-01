require "../spec_helper"
require "../../src/crustache"

class MustacheView
  def has_key?(name)
    name == "pid"
  end

  def [](name)
    name == "pid" ? Process.pid : nil
  end

  Kilt.file "spec/fixtures/test.mustache", "__kilt_io__", self
end

describe "kilt/crustache" do

  it "renders crustache" do
    Kilt.render("spec/fixtures/test.mustache", { "pid" => Process.pid }).should eq("<span>#{Process.pid}</span>")
  end

  it "works with classes" do
    MustacheView.new.to_s.should eq("<span>#{Process.pid}</span>")
  end

end

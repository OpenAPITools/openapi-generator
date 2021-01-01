require "../spec_helper"

describe Crest::VERSION do
  it "should have version" do
    (Crest::VERSION).should be_a(String)
  end
end

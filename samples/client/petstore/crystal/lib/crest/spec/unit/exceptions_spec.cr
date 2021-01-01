require "../spec_helper"

describe Crest::RequestFailed do
  it "should return descendant class for existing status code" do
    (Crest::RequestFailed.subclass_by_status_code(404)).should eq(Crest::NotFound)
  end

  it "should return self for nonexistent status code" do
    (Crest::RequestFailed.subclass_by_status_code(777)).should eq(Crest::RequestFailed)
  end
end

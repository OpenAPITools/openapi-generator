require "../spec_helper"

describe Crest::DataForm do
  describe "#generate" do
    it "generate form" do
      input = {:file => {"one" => "one", "two" => "two"}}
      parsed_input = [{"file[one]", "one"}, {"file[two]", "two"}]
      content_type = "multipart/form-data"

      form = Crest::DataForm.generate(input)

      form.content_type.should contain(content_type)
      form.params.should eq(input)
      form.parsed_params.should eq(parsed_input)
    end

    it "generate form with multipart" do
      file = File.open("#{__DIR__}/../support/fff.png")
      input = {:file => file, "param1" => "value1"}

      parsed_input = [{"file", file}, {"param1", "value1"}]
      content_type = "multipart/form-data"

      form = Crest::DataForm.generate(input)

      form.content_type.should contain(content_type)
      form.params.should eq(input)
      form.parsed_params.should eq(parsed_input)
    end
  end
end

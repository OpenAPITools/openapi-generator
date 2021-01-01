require "../spec_helper"

describe Crest::UrlencodedForm do
  describe "#generate" do
    it "generate nested form" do
      input = {"year" => "2017 - today", "param2" => 2}
      parsed_input = "year=2017+-+today&param2=2"
      content_type = "application/x-www-form-urlencoded"

      form = Crest::UrlencodedForm.generate(input)

      form.content_type.should contain(content_type)
      form.params.should eq(input)
      form.form_data.should eq(parsed_input)
    end

    it "generate nested form" do
      input = {:file => {"one" => "one", "two" => "two"}}
      parsed_input = "file%5Bone%5D=one&file%5Btwo%5D=two"
      content_type = "application/x-www-form-urlencoded"

      form = Crest::UrlencodedForm.generate(input)

      form.content_type.should contain(content_type)
      form.params.should eq(input)
      form.form_data.should eq(parsed_input)
    end
  end
end

require "../spec_helper"

describe Crest::Resource do
  describe "#initialize" do
    it "initialize new resource" do
      resource = Crest::Resource.new("http://localhost", headers: {"X-Something" => "1"})

      resource.url.should eq("http://localhost")
      resource.headers.should eq({"X-Something" => "1"})
    end

    it "initialize new resource with proxy params" do
      resource = Crest::Resource.new("http://localhost", p_addr: "localhost", p_port: 3128)

      resource.p_addr.should eq("localhost")
      resource.p_port.should eq(3128)
    end

    it "initialize new resource with logger" do
      resource = Crest::Resource.new("http://localhost", logging: true)

      (resource.logging).should eq(true)
      (resource.logger).should be_a(Crest::Logger)
    end

    it "initialize new resource without headers" do
      resource = Crest::Resource.new("http://localhost")

      resource.url.should eq("http://localhost")
      resource.headers.should eq({} of String => String)
    end

    it "initialize new resource with []" do
      site = Crest::Resource.new("http://localhost", headers: {"X-Something" => "1"})
      resource = site["/resource"]

      resource.url.should eq("http://localhost/resource")
      resource.headers.should eq({"X-Something" => "1"})
    end

    it "initialize new resource with params" do
      resource = Crest::Resource.new("http://localhost", params: {"foo" => "123", "bar" => "456"})

      resource.url.should eq("http://localhost")
      resource.params.should eq({"foo" => "123", "bar" => "456"})
    end
  end
end

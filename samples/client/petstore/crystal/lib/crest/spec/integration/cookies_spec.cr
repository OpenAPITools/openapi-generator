require "../spec_helper"

describe Crest do
  describe "Cookies" do
    context Crest::Request do
      it "should set cookies" do
        response = Crest::Request.execute(:get, "#{TEST_SERVER_URL}", cookies: {"k1" => "v1", "k2" => "v2"})
        (response.status_code).should eq(200)
        (response.cookies).should eq({"k1" => "v1", "k2" => "v2"})
      end

      it "should set cookies in the block" do
        request = Crest::Request.new(:get, TEST_SERVER_URL, cookies: {"k1" => "v1"}) do |req|
          req.cookies["k2"] = "v2"
        end

        response = request.execute

        (response.status_code).should eq(200)
        (response.cookies).should eq({"k1" => "v1", "k2" => "v2"})
      end

      it "should access cookies from the server" do
        response = Crest::Request.execute(:get, "#{TEST_SERVER_URL}/cookies/set", params: {"k1" => "v1", "k2" => "v2"})

        (response.status_code).should eq(200)
        (JSON.parse(response.body)).should eq({"cookies" => {"k1" => "v1", "k2" => "v2"}})
        (response.cookies).should eq({"k1" => "v1", "k2" => "v2"})
      end

      it "should set cookies from server with redirect" do
        response = Crest::Request.execute(:get, "#{TEST_SERVER_URL}/cookies/set_redirect", params: {"k1" => "v1", "k2" => "v2"})

        (response.status_code).should eq(200)
        (response.cookies).should eq({"k1" => "v1", "k2" => "v2"})
        (response.headers.[]("Cookie")).should eq("k1=v1; k2=v2")
        (JSON.parse(response.body)).should eq({"cookies" => {"k1" => "v1", "k2" => "v2"}})
      end
    end
  end
end

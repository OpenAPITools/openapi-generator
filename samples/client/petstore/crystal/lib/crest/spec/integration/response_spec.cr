require "../spec_helper"

describe Crest::Response do
  it "response instance should respond to helper methods" do
    response = Crest.get("#{TEST_SERVER_URL}")
    (response.body).should eq("Hello World!")
    (response.invalid?).should be_false
    (response.informational?).should be_false
    (response.success?).should be_true
    (response.redirection?).should be_false
    (response.redirect?).should be_false
    (response.client_error?).should be_false
    (response.server_error?).should be_false
  end

  it "response instance should have status and status_code" do
    response = Crest.get("#{TEST_SERVER_URL}")

    (response.status).should be_a(HTTP::Status)
    (response.status_code).should be_a(Int32)
  end

  it "response instance should have filename if available" do
    filename = "filename.jpg"
    response = Crest.get("#{TEST_SERVER_URL}/download?filename=#{filename}")

    (response.filename).should eq(filename)
  end

  it "response instance should have filename nil unless available" do
    response = Crest.get("#{TEST_SERVER_URL}")

    (response.filename).should be_nil
  end

  it "#to_curl" do
    response = Crest.get("#{TEST_SERVER_URL}")

    (response.to_curl).should eq("curl -X GET #{TEST_SERVER_URL}")
  end

  describe "to_s" do
    it "does to_s" do
      response = Crest.get("#{TEST_SERVER_URL}")

      response.to_s.should eq("Hello World!")
    end
  end

  describe "inspect" do
    it "does inspect" do
      response = Crest.get("#{TEST_SERVER_URL}")

      (response.inspect).should eq("<Crest::Response 200 \"Hello World...\">")
    end
  end
end

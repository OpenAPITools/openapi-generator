require "../spec_helper"

describe Crest do
  it "do GET request" do
    response = Crest.get("#{TEST_SERVER_URL}")
    (response.body).should eq("Hello World!")
  end

  it "do GET request with params" do
    response = Crest.get("#{TEST_SERVER_URL}/resize", params: {:width => 100, :height => 100})
    (response.body).should eq("Width: 100, height: 100")
  end

  it "do GET request with different params" do
    response = Crest.get("#{TEST_SERVER_URL}/resize", params: {"width" => 100, :height => "100"})
    (response.body).should eq("Width: 100, height: 100")
  end

  it "do GET request with params with nil" do
    response = Crest.get("#{TEST_SERVER_URL}/add_key", params: {:json => nil, :key => 123})
    (response.body).should eq("JSON: key[123]")
  end

  it "do POST request" do
    response = Crest.post("#{TEST_SERVER_URL}/post/1/comments", form: {:title => "Title"})
    (response.body).should eq("Post with title `Title` created")
  end

  it "upload file" do
    file = File.open("#{__DIR__}/../support/fff.png")
    response = Crest.post("#{TEST_SERVER_URL}/upload", form: {:file => file})
    (response.body).should match(/Upload OK/)
  end

  it "do POST nested params" do
    response = Crest.post("#{TEST_SERVER_URL}/post_nested", form: {:params1 => "one", :nested => {:params2 => "two"}})
    (response.body).should eq("params1=one&nested%5Bparams2%5D=two")
  end

  it "do PUT request" do
    response = Crest.put("#{TEST_SERVER_URL}/post/1/comments/1", form: {:title => "Put Update"})
    (response.body).should eq("Update Comment `1` for Post `1` with title `Put Update`")
  end

  it "do PATCH request" do
    response = Crest.patch("#{TEST_SERVER_URL}/post/1/comments/1", form: {:title => "Patch Update"})
    (response.body).should eq("Update Comment `1` for Post `1` with title `Patch Update`")
  end

  it "do DELETE request" do
    response = Crest.delete("#{TEST_SERVER_URL}/post/1/comments/1")
    (response.body).should eq("Delete Comment `1` for Post `1`")
  end

  it "do GET request with block without handle errors" do
    body = ""

    Crest.get("#{TEST_SERVER_URL}/404", handle_errors: false) do |resp|
      case resp
      when .success?
        body = resp.body_io.gets_to_end
      when .client_error?
        body = "Client error"
      when .server_error?
        body = "Server error"
      else
        raise "Unknown response with code #{resp.status_code}"
      end
    end

    body.should eq("Client error")
  end
end

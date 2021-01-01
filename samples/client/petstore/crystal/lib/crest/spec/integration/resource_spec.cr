require "../spec_helper"

describe Crest::Response do
  it "do GET request" do
    resource = Crest::Resource.new("#{TEST_SERVER_URL}/post/1/comments")
    response = resource.get
    (response.body).should eq("Post 1: comments")
  end

  it "do GET request when base url ends with /" do
    resource = Crest::Resource.new("#{TEST_SERVER_URL}/")
    response = resource.get("/post/1/comments")
    (response.body).should eq("Post 1: comments")
  end

  it "do GET request when path does not start with /" do
    resource = Crest::Resource.new("#{TEST_SERVER_URL}")
    response = resource.get("post/1/comments")
    (response.body).should eq("Post 1: comments")
  end

  it "do GET request with []" do
    site = Crest::Resource.new("#{TEST_SERVER_URL}")
    response = site["/post/1/comments"].get
    (response.body).should eq("Post 1: comments")
  end

  it "do GET request with [] when base url ends with /" do
    site = Crest::Resource.new("#{TEST_SERVER_URL}/")
    response = site["/post/1/comments"].get
    (response.body).should eq("Post 1: comments")
  end

  it "do multiple GET requests with []" do
    site = Crest::Resource.new("#{TEST_SERVER_URL}")

    response1 = site["/post/1/comments"].get
    response2 = site["/post/2/comments"].get

    (response1.body).should eq("Post 1: comments")
    (response2.body).should eq("Post 2: comments")
  end

  it "do GET request with params" do
    resource = Crest::Resource.new("#{TEST_SERVER_URL}/resize")
    response = resource.get(params: {:width => "100", :height => 100})
    (response.body).should eq("Width: 100, height: 100")
  end

  it "do GET request with [] and params" do
    resource = Crest::Resource.new(TEST_SERVER_URL)
    response = resource["/resize"].get(params: {:width => 100, :height => 100})
    (response.body).should eq("Width: 100, height: 100")
  end

  it "do GET request with suburl and params" do
    resource = Crest::Resource.new(TEST_SERVER_URL)
    response = resource.get("resize", params: {:width => 100, :height => 100})
    (response.body).should eq("Width: 100, height: 100")
  end

  it "do GET request with [] and default params" do
    resource = Crest::Resource.new(
      TEST_SERVER_URL,
      params: {:width => 100, :height => 100}
    )
    response = resource["/resize"].get
    (response.body).should eq("Width: 100, height: 100")
  end

  it "do GET request with suburl and default params" do
    resource = Crest::Resource.new(
      TEST_SERVER_URL,
      params: {:width => 100}
    )
    response = resource.get("/resize", params: {:height => 100})
    (response.body).should eq("Width: 100, height: 100")
  end

  it "should accept block" do
    resource = Crest::Resource.new(TEST_SERVER_URL) do |res|
      res.headers.merge!({"foo" => "bar"})
    end

    response = resource["/headers"].get

    (JSON.parse(response.body)["headers"]["foo"]).should eq("bar")
  end

  it "initializer can accept HTTP::Client as http_client" do
    uri = URI.parse(TEST_SERVER_URL)

    client = HTTP::Client.new(uri)
    client.before_request do |request|
      request.headers.add("foo", "bar")
    end

    resource = Crest::Resource.new(TEST_SERVER_URL, http_client: client)
    response = resource["/headers"].get

    (JSON.parse(response.body)["headers"]["foo"]).should eq("bar")
  end

  it "access http_client in instance of Crest::Resource" do
    resource = Crest::Resource.new(TEST_SERVER_URL)
    resource.http_client.before_request do |req|
      req.headers.add("foo", "bar")
    end

    response = resource["/headers"].get

    (JSON.parse(response.body)["headers"]["foo"]).should eq("bar")
  end

  it "change HTTP::Client in Crest::Resource" do
    uri = URI.parse(TEST_SERVER_URL)

    client = HTTP::Client.new(uri)
    client.read_timeout = 5.minutes

    resource = Crest::Resource.new(TEST_SERVER_URL, http_client: client)

    resource.http_client.read_timeout = 1.second

    expect_raises IO::TimeoutError do
      resource["/delay/2"].get
    end
  end

  it "do POST request" do
    resource = Crest::Resource.new("#{TEST_SERVER_URL}/post/1/comments")
    response = resource.post(form: {:title => "Title"})
    (response.body).should eq("Post with title `Title` created")
  end

  it "do POST request with []" do
    site = Crest::Resource.new(TEST_SERVER_URL)
    response = site["/post/1/comments"].post(form: {:title => "Title"})
    (response.body).should eq("Post with title `Title` created")
  end

  it "do POST request with suburl" do
    site = Crest::Resource.new(TEST_SERVER_URL)
    response = site.post("/post/1/comments", form: {:title => "Title"})
    (response.body).should eq("Post with title `Title` created")
  end

  it "do POST request with [] and default params" do
    site = Crest::Resource.new(TEST_SERVER_URL, params: {"key" => "key"})
    response = site["/resize"].post(
      form: {:height => 100, "width" => "100"},
      params: {:secret => "secret"}
    )
    (response.body).should eq("Width: 100, height: 100. Key: key, secret: secret")
  end

  it "upload file" do
    file = File.open("#{__DIR__}/../support/fff.png")
    resource = Crest::Resource.new("#{TEST_SERVER_URL}/upload")
    response = resource.post(form: {:file => file})

    (response.body).should match(/Upload OK/)
  end

  it "upload file with []" do
    file = File.open("#{__DIR__}/../support/fff.png")
    resource = Crest::Resource.new("#{TEST_SERVER_URL}")
    response = resource["/upload"].post(form: {:file => file})

    (response.body).should match(/Upload OK/)
  end

  it "do PUT request" do
    resource = Crest::Resource.new("#{TEST_SERVER_URL}/post/1/comments/1")
    response = resource.put(form: {:title => "Put Update"})
    (response.body).should eq("Update Comment `1` for Post `1` with title `Put Update`")
  end

  it "do PATCH request" do
    resource = Crest::Resource.new("#{TEST_SERVER_URL}/post/1/comments/1")
    response = resource.patch(form: {:title => "Patch Update"})
    (response.body).should eq("Update Comment `1` for Post `1` with title `Patch Update`")
  end

  it "do DELETE request" do
    resource = Crest::Resource.new("#{TEST_SERVER_URL}/post/1/comments/1")
    response = resource.delete
    (response.body).should eq("Delete Comment `1` for Post `1`")
  end

  it "do GET request with logging" do
    resource = Crest::Resource.new(TEST_SERVER_URL, logging: true)
    response = resource["/post/1/comments"].get
    (response.body).should eq("Post 1: comments")
  end

  it "do OPTIONS request" do
    resource = Crest::Resource.new(TEST_SERVER_URL)
    response = resource.options

    (response.headers["Allow"]).should eq("OPTIONS, GET")
  end

  it "#to_curl" do
    resource = Crest::Resource.new("#{TEST_SERVER_URL}")
    response = resource["/post/1/comments"].get

    (response.to_curl).should eq("curl -X GET #{TEST_SERVER_URL}/post/1/comments")
  end
end

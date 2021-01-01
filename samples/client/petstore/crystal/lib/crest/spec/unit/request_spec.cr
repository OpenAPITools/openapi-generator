require "../spec_helper"

describe Crest::Request do
  describe "#initialize" do
    it "new HTTP request" do
      request = Crest::Request.new(:get, "http://localhost/get")
      (request.url).should eq("http://localhost/get")
      (request.max_redirects).should eq(10)
      (request.host).should eq("localhost")
      (request.port).should eq(80)
      (request.tls?).should eq(nil)
      (request.user).should be_nil
      (request.password).should be_nil
      (request.proxy).should be_nil
      (request.logging).should be_false
    end

    it "new HTTPS request" do
      request = Crest::Request.new(:get, "https://localhost/get")
      (request.url).should eq("https://localhost/get")
      (request.host).should eq("localhost")
      (request.port).should eq(443)
      (request.tls?).should be_a(OpenSSL::SSL::Context::Client)
    end

    it "initialize the GET request" do
      request = Crest::Request.new(:get, "http://localhost", headers: {"Content-Type" => "application/json"})
      (request.method).should eq("GET")
      (request.url).should eq("http://localhost")
      (request.headers).should eq(HTTP::Headers{"Content-Type" => "application/json"})
      (request.form_data).should eq(nil)
    end

    it "initialize the GET request with params" do
      request = Crest::Request.new(:get, "http://localhost", params: {:foo => "hello world", :bar => 456})
      (request.method).should eq("GET")
      (request.url).should eq("http://localhost?foo=hello+world&bar=456")
      (request.form_data).should eq(nil)
    end

    it "initialize the GET request with params in url" do
      request = Crest::Request.new(:get, "http://localhost?json", params: {:key => 123})
      (request.method).should eq("GET")
      (request.url).should eq("http://localhost?json&key=123")
      (request.form_data).should eq(nil)
    end

    it "initialize the GET request with nil value in params" do
      request = Crest::Request.new(:get, "http://localhost", params: {:json => nil, :key => 123})
      (request.method).should eq("GET")
      (request.url).should eq("http://localhost?json=&key=123")
      (request.form_data).should eq(nil)
    end

    it "initialize the GET request with cookies" do
      request = Crest::Request.new(:get, "http://localhost", cookies: {:foo => "123", :bar => 456})
      (request.headers).should eq(HTTP::Headers{"Cookie" => "foo=123; bar=456"})
    end

    it "initialize the POST request with form" do
      request = Crest::Request.new(:post, "http://localhost", form: {:foo => "bar"})
      (request.method).should eq("POST")
      (request.url).should eq("http://localhost")
      (request.headers["Content-Type"]).should eq("application/x-www-form-urlencoded")
      (request.form_data.to_s).should eq("foo=bar")
    end

    it "initialize the POST request with form as a string" do
      request = Crest::Request.new(:post, "http://localhost", headers: {"Content-Type" => "application/json"}, form: {:foo => "bar"}.to_json)
      (request.method).should eq("POST")
      (request.url).should eq("http://localhost")
      (request.headers["Content-Type"]).should eq("application/json")
      (request.form_data.to_s).should eq("{\"foo\":\"bar\"}")
    end

    it "initialize the POST and encode string" do
      request = Crest::Request.new(:post, "http://localhost", form: {:title => "New @Title"})
      (request.method).should eq("POST")
      (request.url).should eq("http://localhost")
      (request.form_data).should eq("title=New+%40Title")
    end

    it "initialize the POST request with multipart" do
      file = File.open("#{__DIR__}/../support/fff.png")
      request = Crest::Request.new(:post, "http://localhost", form: {:file => file})
      (request.method).should eq("POST")
      (request.url).should eq("http://localhost")
      (request.headers["Content-Type"]).should contain("multipart/form-data; boundary=")
      (request.form_data.to_s).should contain("form-data; name=\"file\"; filename=")
    end

    it "POST request with nested hashes" do
      request = Crest::Request.new(:post, "http://localhost", form: {:params1 => "one", :nested => {:params2 => "two"}})
      (request.headers["Content-Type"]).should eq("application/x-www-form-urlencoded")
      (request.form_data.to_s).should eq("params1=one&nested%5Bparams2%5D=two")
    end

    it "initialize the PUT request with form" do
      request = Crest::Request.new(:put, "http://localhost", form: {:foo => "bar"})
      (request.method).should eq("PUT")
      (request.url).should eq("http://localhost")
      (request.headers["Content-Type"]).should eq("application/x-www-form-urlencoded")
      (request.form_data.to_s).should eq("foo=bar")
    end

    it "initialize the OPTIONS request" do
      request = Crest::Request.new(:options, "http://localhost")
      (request.method).should eq("OPTIONS")
      (request.url).should eq("http://localhost")
    end

    it "initialize Request with :max_redirects" do
      request = Crest::Request.new(:get, "http://localhost", max_redirects: 3)
      (request.max_redirects).should eq(3)
    end

    it "initialize Request with basic auth params" do
      request = Crest::Request.new(:get, "http://localhost", user: "user", password: "password")
      (request.user).should eq("user")
      (request.password).should eq("password")
    end

    it "initialize Request with proxy params" do
      request = Crest::Request.new(:get, "http://localhost", p_addr: "localhost", p_port: 3128)
      (request.proxy).should be_a(HTTP::Proxy::Client)
    end

    it "initialize Request with :logging and logger" do
      request = Crest::Request.new(:get, "http://localhost", logging: true)
      (request.logging).should eq(true)
      (request.logger).should be_a(Crest::Logger)
    end
  end

  describe "#to_curl" do
    it "converts request to cURL command" do
      request = Crest::Request.new(:get, "http://localhost")
      (request.to_curl).should eq("curl -X GET http://localhost")
    end
  end
end

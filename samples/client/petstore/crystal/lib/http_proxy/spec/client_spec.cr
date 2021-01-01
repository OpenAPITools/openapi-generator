require "./spec_helper"

describe HTTP::Proxy::Client do
  describe "#initialize" do
    it "with host and port" do
      client = HTTP::Proxy::Client.new("127.0.0.1", 8080)
      (client.host).should eq("127.0.0.1")
      (client.port).should eq(8080)
      (client.username).should eq(nil)
      (client.password).should eq(nil)
    end

    it "with username and password" do
      client = HTTP::Proxy::Client.new("127.0.0.1", 8080, username: "user", password: "password")
      (client.username).should eq("user")
      (client.password).should eq("password")
    end

    context "HTTP::Client#set_proxy" do
      it "should make HTTP request with proxy" do
        with_proxy_server do |host, port, wants_close|
          proxy_client = HTTP::Proxy::Client.new(host, port)

          uri = URI.parse("http://httpbin.org")
          client = HTTP::Client.new(uri)
          client.set_proxy(proxy_client)
          response = client.get("/get")

          (client.proxy?).should eq(true)
          (response.status_code).should eq(200)
        ensure
          wants_close.send(nil)
        end
      end

      it "should make HTTPS request with proxy" do
        with_proxy_server do |host, port, wants_close|
          proxy_client = HTTP::Proxy::Client.new(host, port)

          uri = URI.parse("https://httpbin.org")
          client = HTTP::Client.new(uri)
          client.set_proxy(proxy_client)
          response = client.get("/get")

          (client.proxy?).should eq(true)
          (response.status_code).should eq(200)
        ensure
          wants_close.send(nil)
        end
      end
    end
  end
end

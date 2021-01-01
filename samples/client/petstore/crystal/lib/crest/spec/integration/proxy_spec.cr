require "../spec_helper"

describe Crest do
  describe "With proxy server" do
    it "should make request" do
      with_proxy_server do |host, port, wants_close|
        response = Crest.get("#{TEST_SERVER_URL}/", p_addr: host, p_port: port)
        (response.status_code).should eq(200)
        (response.body).should contain("Hello World!")
        (response.request.p_addr).should eq("127.0.0.1")
        (response.request.p_port).should eq(8080)
      ensure
        wants_close.send(nil)
      end
    end

    it "should redirect with proxy" do
      with_proxy_server do |_, _, wants_close|
        response = Crest.get("#{TEST_SERVER_URL}/redirect/1", p_addr: "127.0.0.1", p_port: 8080)
        (response.status_code).should eq(200)
        (response.body).should contain("Hello World!")
        (response.url).should eq("#{TEST_SERVER_URL}/")
        (response.history.size).should eq(1)
        (response.history.first.url).should eq("#{TEST_SERVER_URL}/redirect/1")
        (response.history.first.status_code).should eq(302)
        (response.request.p_addr).should eq("127.0.0.1")
        (response.request.p_port).should eq(8080)
      ensure
        wants_close.send(nil)
      end
    end
  end

  describe Crest::Resource do
    it "should make request" do
      with_proxy_server do |host, port, wants_close|
        resource = Crest::Resource.new("#{TEST_SERVER_URL}", p_addr: host, p_port: port)
        response = resource.get

        (response.status_code).should eq(200)
        (response.body).should contain("Hello World!")
        (response.request.p_addr).should eq("127.0.0.1")
        (response.request.p_port).should eq(8080)
      ensure
        wants_close.send(nil)
      end
    end

    it "should make suburl request" do
      with_proxy_server do |host, port, wants_close|
        resource = Crest::Resource.new("#{TEST_SERVER_URL}", p_addr: host, p_port: port)
        response = resource["/post/1/comments"].get

        (response.status_code).should eq(200)
        (response.body).should contain("Post 1: comments")
      ensure
        wants_close.send(nil)
      end
    end
  end
end

require "../spec_helper"

describe Crest do
  describe "Streaming" do
    it "should stream Response#execute" do
      count = 5
      body = String::Builder.new
      request = Crest::Request.new(:get, "#{TEST_SERVER_URL}/stream/#{count}")

      request.execute do |resp|
        while line = resp.body_io.gets
          body << line
        end
      end

      body.to_s.should eq("Hello World!" * count)
    end

    it "should stream Request.execute" do
      count = 5
      body = String::Builder.new
      Crest::Request.get("#{TEST_SERVER_URL}/stream/#{count}") do |resp|
        while line = resp.body_io.gets
          body << line
        end
      end

      body.to_s.should eq("Hello World!" * count)
    end

    it "should stream Request.get" do
      count = 5
      body = String::Builder.new
      Crest::Request.execute(:get, "#{TEST_SERVER_URL}/stream/#{count}") do |resp|
        while line = resp.body_io.gets
          body << line
        end
      end

      body.to_s.should eq("Hello World!" * count)
    end

    it "should stream Crest#get" do
      count = 5
      body = String::Builder.new
      Crest.get("#{TEST_SERVER_URL}/stream/#{count}") do |resp|
        while line = resp.body_io.gets
          body << line
        end
      end

      body.to_s.should eq("Hello World!" * count)
    end

    it "should stream Resource#get with []" do
      count = 5
      body = String::Builder.new
      resource = Crest::Resource.new(TEST_SERVER_URL)
      resource["/stream/#{count}"].get do |resp|
        while line = resp.body_io.gets
          body << line
        end
      end

      body.to_s.should eq("Hello World!" * count)
    end

    it "should stream Resource#get" do
      count = 5
      body = String::Builder.new
      resource = Crest::Resource.new(TEST_SERVER_URL)
      resource.get("/stream/#{count}") do |resp|
        while line = resp.body_io.gets
          body << line
        end
      end

      body.to_s.should eq("Hello World!" * count)
    end

    it "should stream Crest#get with redirects" do
      body = String::Builder.new

      Crest.get("#{TEST_SERVER_URL}/redirect/2") do |resp|
        while line = resp.body_io.gets
          body << line
        end
      end

      body.to_s.should eq("Hello World!")
    end

    it "should stream Response#execute with redirects" do
      count = 5
      body = String::Builder.new
      request = Crest::Request.new(:get, "#{TEST_SERVER_URL}/redirect_stream/#{count}")

      request.execute do |resp|
        while line = resp.body_io.gets
          body << line
        end
      end

      body.to_s.should eq("Hello World!" * 5)
    end

    it "should stream Resource#get with redirects" do
      count = 5
      body = String::Builder.new
      resource = Crest::Resource.new(TEST_SERVER_URL)
      resource["/redirect_stream/#{count}"].get do |resp|
        while line = resp.body_io.gets
          body << line
        end
      end

      body.to_s.should eq("Hello World!" * count)
    end
  end
end

require "../spec_helper"

describe Crest do
  describe "Raise exception" do
    it "404" do
      expect_raises Crest::RequestFailed, "HTTP status code 404: Not Found" do
        Crest.get("#{TEST_SERVER_URL}/404")
      end
    end

    it "request with rescue" do
      response = begin
        Crest.get("#{TEST_SERVER_URL}/500")
      rescue ex : Crest::NotFound
        ex.response
      rescue ex : Crest::InternalServerError
        ex.response
      end

      response.body.should eq("500 error")
    end

    it "404 with Resource" do
      expect_raises Crest::RequestFailed, "HTTP status code 404: Not Found" do
        resource = Crest::Resource.new(TEST_SERVER_URL)
        resource["/404"].get
      end
    end

    it "404 raises NotFound" do
      expect_raises Crest::NotFound, "HTTP status code 404: Not Found" do
        resource = Crest::Resource.new(TEST_SERVER_URL)
        resource["/404"].get
      end
    end

    it "500" do
      expect_raises Crest::RequestFailed, "HTTP status code 500" do
        Crest.get("#{TEST_SERVER_URL}/500")
      end
    end

    it "500 raises InternalServerError" do
      expect_raises Crest::InternalServerError, "HTTP status code 500" do
        Crest.get("#{TEST_SERVER_URL}/500")
      end
    end

    it "call .response on the exception to get the server's response" do
      response =
        begin
          Crest.get("#{TEST_SERVER_URL}/404")
        rescue ex : Crest::RequestFailed
          ex.response
        end

      (response.status_code).should eq(404)
    end

    context "with handle_errors: false" do
      it "do not raise error for Crest" do
        response = Crest.get("#{TEST_SERVER_URL}/404", handle_errors: false)

        (response.status_code).should eq(404)
      end

      it "do not raise error for Request" do
        request = Crest::Request.new(:get, "#{TEST_SERVER_URL}/404", handle_errors: false)
        response = request.execute

        (response.status_code).should eq(404)
      end

      it "do not raise error for Resource" do
        resource = Crest::Resource.new(TEST_SERVER_URL, handle_errors: false)
        response = resource["/404"].get

        (response.status_code).should eq(404)
      end
    end
  end
end

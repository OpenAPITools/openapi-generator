require "./spec_helper"

describe "Route" do
  describe "match?" do
    it "matches the correct route" do
      get "/route1" do
        "Route 1"
      end
      get "/route2" do
        "Route 2"
      end
      request = HTTP::Request.new("GET", "/route2")
      client_response = call_request_on_app(request)
      client_response.body.should eq("Route 2")
    end

    it "doesn't allow a route declaration start without /" do
      expect_raises Kemal::Exceptions::InvalidPathStartException, "Route declaration get \"route\" needs to start with '/', should be get \"/route\"" do
        get "route" do
          "Route 1"
        end
      end
    end
  end
end

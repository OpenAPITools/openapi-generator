require "./spec_helper"

describe "Context" do
  context "headers" do
    it "sets content type" do
      get "/" do |env|
        env.response.content_type = "application/json"
        "Hello"
      end
      request = HTTP::Request.new("GET", "/")
      client_response = call_request_on_app(request)
      client_response.headers["Content-Type"].should eq("application/json")
    end

    it "parses headers" do
      get "/" do |env|
        name = env.request.headers["name"]
        "Hello #{name}"
      end
      headers = HTTP::Headers.new
      headers["name"] = "kemal"
      request = HTTP::Request.new("GET", "/", headers)
      client_response = call_request_on_app(request)
      client_response.body.should eq "Hello kemal"
    end

    it "sets response headers" do
      get "/" do |env|
        env.response.headers.add "Accept-Language", "tr"
      end
      request = HTTP::Request.new("GET", "/")
      client_response = call_request_on_app(request)
      client_response.headers["Accept-Language"].should eq "tr"
    end
  end

  context "storage" do
    it "can store primitive types" do
      before_get "/" do |env|
        env.set "before_get", "Kemal"
        env.set "before_get_int", 123
        env.set "before_get_float", 3.5
      end

      get "/" do |env|
        {
          before_get:       env.get("before_get"),
          before_get_int:   env.get("before_get_int"),
          before_get_float: env.get("before_get_float"),
        }
      end

      request = HTTP::Request.new("GET", "/")
      io = IO::Memory.new
      response = HTTP::Server::Response.new(io)
      context = HTTP::Server::Context.new(request, response)
      Kemal::FilterHandler::INSTANCE.call(context)
      Kemal::RouteHandler::INSTANCE.call(context)

      context.get("before_get").should eq "Kemal"
      context.get("before_get_int").should eq 123
      context.get("before_get_float").should eq 3.5
    end

    it "can store custom types" do
      before_get "/" do |env|
        t = TestContextStorageType.new
        t.id = 32
        a = AnotherContextStorageType.new

        env.set "before_get_context_test", t
        env.set "another_context_test", a
      end

      get "/" do |env|
        {
          before_get_context_test: env.get("before_get_context_test"),
          another_context_test:    env.get("another_context_test"),
        }
      end

      request = HTTP::Request.new("GET", "/")
      io = IO::Memory.new
      response = HTTP::Server::Response.new(io)
      context = HTTP::Server::Context.new(request, response)
      Kemal::FilterHandler::INSTANCE.call(context)
      Kemal::RouteHandler::INSTANCE.call(context)

      context.get("before_get_context_test").as(TestContextStorageType).id.should eq 32
      context.get("another_context_test").as(AnotherContextStorageType).name.should eq "kemal-context"
    end

    it "fetches non-existent keys from store with get?" do
      get "/" { }

      request = HTTP::Request.new("GET", "/")
      io = IO::Memory.new
      response = HTTP::Server::Response.new(io)
      context = HTTP::Server::Context.new(request, response)
      Kemal::FilterHandler::INSTANCE.call(context)
      Kemal::RouteHandler::INSTANCE.call(context)

      context.get?("non_existent_key").should be_nil
      context.get?("another_non_existent_key").should be_nil
    end
  end
end

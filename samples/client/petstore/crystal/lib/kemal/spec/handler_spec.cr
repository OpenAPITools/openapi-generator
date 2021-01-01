require "./spec_helper"

class CustomTestHandler < Kemal::Handler
  def call(env)
    env.response << "Kemal"
    call_next env
  end
end

class OnlyHandler < Kemal::Handler
  only ["/only"]

  def call(env)
    return call_next(env) unless only_match?(env)
    env.response.print "Only"
    call_next env
  end
end

class ExcludeHandler < Kemal::Handler
  exclude ["/exclude"]

  def call(env)
    return call_next(env) if exclude_match?(env)
    env.response.print "Exclude"
    call_next env
  end
end

class PostOnlyHandler < Kemal::Handler
  only ["/only", "/route1", "/route2"], "POST"

  def call(env)
    return call_next(env) unless only_match?(env)
    env.response.print "Only"
    call_next env
  end
end

class PostExcludeHandler < Kemal::Handler
  exclude ["/exclude"], "POST"

  def call(env)
    return call_next(env) if exclude_match?(env)
    env.response.print "Exclude"
    call_next env
  end
end

class ExcludeHandlerPercentW < Kemal::Handler
  exclude %w[/exclude]

  def call(env)
    return call_next(env) if exclude_match?(env)
    env.response.print "Exclude"
    call_next env
  end
end

class PostOnlyHandlerPercentW < Kemal::Handler
  only %w[/only /route1 /route2], "POST"

  def call(env)
    return call_next(env) unless only_match?(env)
    env.response.print "Only"
    call_next env
  end
end

describe "Handler" do
  it "adds custom handler before before_*" do
    filter_middleware = Kemal::FilterHandler.new
    filter_middleware._add_route_filter("GET", "/", :before) do |env|
      env.response << " is"
    end

    filter_middleware._add_route_filter("GET", "/", :before) do |env|
      env.response << " so"
    end
    add_handler CustomTestHandler.new

    get "/" do
      " Great"
    end
    request = HTTP::Request.new("GET", "/")
    client_response = call_request_on_app(request)
    client_response.status_code.should eq(200)
    client_response.body.should eq("Kemal is so Great")
  end

  it "runs specified only_routes in middleware" do
    get "/only" do
      "Get"
    end
    add_handler OnlyHandler.new
    request = HTTP::Request.new("GET", "/only")
    client_response = call_request_on_app(request)
    client_response.body.should eq "OnlyGet"
  end

  it "doesn't run specified exclude_routes in middleware" do
    get "/" do
      "Get"
    end
    get "/exclude" do
      "Exclude"
    end
    add_handler ExcludeHandler.new
    request = HTTP::Request.new("GET", "/")
    client_response = call_request_on_app(request)
    client_response.body.should eq "ExcludeGet"
  end

  it "runs specified only_routes with method in middleware" do
    post "/only" do
      "Post"
    end
    get "/only" do
      "Get"
    end
    add_handler PostOnlyHandler.new
    request = HTTP::Request.new("POST", "/only")
    client_response = call_request_on_app(request)
    client_response.body.should eq "OnlyPost"
  end

  it "doesn't run specified exclude_routes with method in middleware" do
    post "/exclude" do
      "Post"
    end
    post "/only" do
      "Post"
    end
    add_handler PostOnlyHandler.new
    add_handler PostExcludeHandler.new
    request = HTTP::Request.new("POST", "/only")
    client_response = call_request_on_app(request)
    client_response.body.should eq "OnlyExcludePost"
  end

  it "adds a handler at given position" do
    post_handler = PostOnlyHandler.new
    add_handler post_handler, 1
    Kemal.config.setup
    Kemal.config.handlers[1].should eq post_handler
  end

  it "assigns custom handlers" do
    post_only_handler = PostOnlyHandler.new
    post_exclude_handler = PostExcludeHandler.new
    Kemal.config.handlers = [post_only_handler, post_exclude_handler]
    Kemal.config.handlers.should eq [post_only_handler, post_exclude_handler]
  end

  it "is able to use %w in macros" do
    post_only_handler = PostOnlyHandlerPercentW.new
    exclude_handler = ExcludeHandlerPercentW.new
    Kemal.config.handlers = [post_only_handler, exclude_handler]
    Kemal.config.handlers.should eq [post_only_handler, exclude_handler]
  end
end

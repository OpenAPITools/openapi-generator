require "./spec_helper"

describe "Kemal::InitHandler" do
  it "initializes context with Content-Type: text/html" do
    request = HTTP::Request.new("GET", "/")
    io = IO::Memory.new
    response = HTTP::Server::Response.new(io)
    context = HTTP::Server::Context.new(request, response)
    Kemal::InitHandler::INSTANCE.next = ->(_context : HTTP::Server::Context) {}
    Kemal::InitHandler::INSTANCE.call(context)
    context.response.headers["Content-Type"].should eq "text/html"
  end

  it "initializes context with X-Powered-By: Kemal" do
    request = HTTP::Request.new("GET", "/")
    io = IO::Memory.new
    response = HTTP::Server::Response.new(io)
    context = HTTP::Server::Context.new(request, response)
    Kemal::InitHandler::INSTANCE.call(context)
    context.response.headers["X-Powered-By"].should eq "Kemal"
  end

  it "does not initialize context with X-Powered-By: Kemal if disabled" do
    Kemal.config.powered_by_header = false
    request = HTTP::Request.new("GET", "/")
    io = IO::Memory.new
    response = HTTP::Server::Response.new(io)
    context = HTTP::Server::Context.new(request, response)
    Kemal::InitHandler::INSTANCE.call(context)
    context.response.headers["X-Powered-By"]?.should be_nil
  end
end

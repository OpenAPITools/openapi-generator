require "./spec_helper"

describe "Kemal::LogHandler" do
  it "logs to the given IO" do
    io = IO::Memory.new
    logger = Kemal::LogHandler.new io
    logger.write "Something"
    io.to_s.should eq "Something"
  end

  it "creates log message for each request" do
    request = HTTP::Request.new("GET", "/")
    io = IO::Memory.new
    context_io = IO::Memory.new
    response = HTTP::Server::Response.new(context_io)
    context = HTTP::Server::Context.new(request, response)
    logger = Kemal::LogHandler.new io
    logger.call(context)
    io.to_s.should_not be nil
  end
end

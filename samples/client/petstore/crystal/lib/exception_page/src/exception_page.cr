abstract class ExceptionPage
end

require "ecr"
require "./exception_page/*"

# :nodoc:
abstract class ExceptionPage
  @params : Hash(String, String)
  @headers : Hash(String, Array(String))
  @session : Hash(String, HTTP::Cookie)
  @method : String
  @path : String
  @message : String
  @query : String
  @frames = [] of Frame
  @title : String

  abstract def styles : Styles

  # Add an optional link to your project
  def project_url : String?
    nil
  end

  # Override this method to add extra HTML to the top of the stack trace heading
  def stack_trace_heading_html
    ""
  end

  # Override this method to add extra javascript to the page
  def extra_javascript
    ""
  end

  # :nodoc:
  def initialize(context : HTTP::Server::Context, @message, @title, @frames)
    @params = context.request.query_params.to_h
    @headers = context.response.headers.to_h
    @method = context.request.method
    @path = context.request.path
    @url = "#{context.request.host_with_port}#{context.request.path}"
    @query = context.request.query_params.to_s
    @session = context.response.cookies.to_h
  end

  def self.for_runtime_exception(context : HTTP::Server::Context, ex : Exception)
    title = "Error #{context.response.status_code}"
    frames = FrameGenerator.generate_frames(ex.inspect_with_backtrace)
    new(context, ex.message.to_s, title: title, frames: frames)
  end

  ECR.def_to_s "#{__DIR__}/exception_page/exception_page.ecr"
end

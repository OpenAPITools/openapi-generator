# Copyright (c) 2017 icyleaf
# Licensed under The MIT License (MIT)
# http://opensource.org/licenses/MIT

require "colorize"
require "../logger"

module Crest
  class CommonLogger < Logger
    def request(request : Crest::Request) : Nil
      message = String.build do |io|
        io << "| " << colorful_method(request.method)
        io << " | " << request.url
        io << " | " << request.form_data.to_s.inspect unless request.form_data.nil?
      end.to_s

      info(message)

      message
    end

    def response(response : Crest::Response) : Nil
      message = String.build do |io|
        io << "| " << colorful_status_code(response.status_code)
        io << " | " << response.url
        io << " | " << response.body.inspect
      end.to_s

      info(message)

      message
    end

    private def colorful_method(method)
      fore, back =
        case method
        when "GET"
          {:white, :blue}
        when "POST"
          {:white, :cyan}
        when "PUT"
          {:white, :yellow}
        when "DELETE"
          {:white, :red}
        when "PATCH"
          {:white, :green}
        when "HEAD"
          {:white, :magenta}
        else
          {:dark_gray, :white}
        end

      colorful((" %-7s" % method), fore, back)
    end

    private def colorful_status_code(status_code)
      fore, back =
        case status_code
        when 300..399
          {:dark_gray, :white}
        when 400..499
          {:white, :yellow}
        when 500..599
          {:white, :red}
        else
          {:white, :green}
        end

      colorful((" %-7s" % status_code), fore, back)
    end

    private def colorful(message, fore, back)
      Colorize.enabled = !@io.is_a?(File)
      message.colorize.fore(fore).back(back)
    end
  end
end

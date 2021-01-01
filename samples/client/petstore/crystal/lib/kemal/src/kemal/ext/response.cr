class HTTP::Server::Response
  class Output
    def close
      # ameba:disable Style/NegatedConditionsInUnless
      unless response.wrote_headers? && !response.headers.has_key?("Content-Range")
        response.content_length = @out_count
      end

      ensure_headers_written

      previous_def
    end
  end
end

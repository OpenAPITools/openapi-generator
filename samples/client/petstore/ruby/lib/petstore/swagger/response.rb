module Petstore
  module Swagger
    class Response
      require 'json'
      require 'date'

      attr_accessor :raw

      def initialize(raw)
        self.raw = raw
      end

      def code
        raw.code
      end

      def status_message
        raw.status_message
      end

      def body
        raw.body
      end

      def success?
        raw.success?
      end

      # Deserialize the raw response body to the given return type.
      #
      # @param [String] return_type some examples: "User", "Array[User]", "Hash[String,Integer]"
      def deserialize(return_type)
        return nil if body.nil? || body.empty?

        # ensuring a default content type
        content_type = raw.headers_hash['Content-Type'] || 'application/json'

        unless content_type.start_with?('application/json')
          fail "Content-Type is not supported: #{content_type}"
        end

        begin
          data = JSON.parse(body, :symbolize_names => true)
        rescue JSON::ParserError => e
          if return_type == 'String'
            return body
          else
            raise e
          end
        end

        build_models data, return_type
      end

      # Walk through the given data and, when necessary, build model(s) from
      # Hash data for array/hash values of the response.
      def build_models(data, return_type)
        case return_type
        when 'String', 'Integer', 'Float', 'BOOLEAN'
          # primitives, return directly
          data
        when 'DateTime'
          # parse date time (expecting ISO 8601 format)
          DateTime.parse data
        when 'Object'
          # generic object, return directly
          data
        when /\AArray<(.+)>\z/
          # e.g. Array<Pet>
          sub_type = $1
          data.map {|item| build_models(item, sub_type) }
        when /\AHash\<String, (.+)\>\z/
          # e.g. Hash<String, Integer>
          sub_type = $1
          {}.tap do |hash|
            data.each {|k, v| hash[k] = build_models(v, sub_type) }
          end
        else
          # models, e.g. Pet
          Petstore.const_get(return_type).new.tap do |model|
            model.build_from_hash data
          end
        end
      end

      # `headers_hash` is a Typhoeus-specific extension of Hash,
      # so simplify it back into a regular old Hash.
      def headers
        h = {}
        raw.headers_hash.each {|k,v| h[k] = v }
        h
      end

      # Extract the response format from the header hash
      # e.g. {'Content-Type' => 'application/json'}
      def format
        headers['Content-Type'].split("/").last.downcase
      end

      def json?
        format == 'json'
      end

      def xml?
        format == 'xml'
      end

      def pretty_body
        return unless body
        if format == 'json'
          JSON.pretty_generate(JSON.parse(body)).gsub(/\n/, '<br/>')
        else
          body
        end
      end

      def pretty_headers
        JSON.pretty_generate(headers).gsub(/\n/, '<br/>')
      end
    end
  end
end

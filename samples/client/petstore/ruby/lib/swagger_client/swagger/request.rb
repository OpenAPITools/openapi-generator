module SwaggerClient
  module Swagger
    class Request
      require 'uri'
      require 'addressable/uri'
      require 'typhoeus'

      attr_accessor :host, :path, :format, :params, :body, :http_method, :headers, :form_params, :auth_names

      # All requests must have an HTTP method and a path
      # Optionals parameters are :params, :headers, :body, :format, :host
      def initialize(http_method, path, attributes={})
        attributes[:format] ||= Swagger.configuration.format
        attributes[:params] ||= {}

        # Set default headers
        default_headers = {
          'Content-Type' => "application/#{attributes[:format].downcase}",
          'User-Agent' => Swagger.configuration.user_agent
        }

        # Merge argument headers into defaults
        attributes[:headers] = default_headers.merge(attributes[:headers] || {})

        # Stick in the auth token if there is one
        if Swagger.authenticated?
          attributes[:headers].merge!({:auth_token => Swagger.configuration.auth_token})
        end

        self.http_method = http_method.to_sym
        self.path = path
        attributes.each do |name, value|
          send("#{name.to_s.underscore.to_sym}=", value)
        end

        update_params_for_auth!
      end

      # Update hearder and query params based on authentication settings.
      def update_params_for_auth!
        (@auth_names || []).each do |auth_name|
          case auth_name
          when 'api_key'
            @headers ||= {}
            @headers['api_key'] = get_api_key_with_prefix('api_key')
          when 'petstore_auth'
            # TODO: support oauth
          
          end
        end
      end

      # Get API key (with prefix if set).
      # @param [String] param_name the parameter name of API key auth
      def get_api_key_with_prefix(param_name)
        if Swagger.configuration.api_key_prefix[param_name].present?
          "#{Swagger.configuration.api_key_prefix[param_name]} #{Swagger.configuration.api_key[param_name]}"
        else
          Swagger.configuration.api_key[param_name]
        end
      end

      # Construct a base URL
      def url(options = {})
        u = Addressable::URI.new(
          :scheme => Swagger.configuration.scheme,
          :host => Swagger.configuration.host,
          :path => self.interpreted_path,
          :query => self.query_string.sub(/\?/, '')
        ).to_s

        # Drop trailing question mark, if present
        u.sub! /\?$/, ''

        u
      end

      # Iterate over the params hash, injecting any path values into the path string
      # e.g. /word.{format}/{word}/entries => /word.json/cat/entries
      def interpreted_path
        p = self.path.dup

        # Stick a .{format} placeholder into the path if there isn't
        # one already or an actual format like json or xml
        # e.g. /words/blah => /words.{format}/blah
        if Swagger.configuration.inject_format
          unless ['.json', '.xml', '{format}'].any? {|s| p.downcase.include? s }
            p = p.sub(/^(\/?\w+)/, "\\1.#{format}")
          end
        end

        # Stick a .{format} placeholder on the end of the path if there isn't
        # one already or an actual format like json or xml
        # e.g. /words/blah => /words/blah.{format}
        if Swagger.configuration.force_ending_format
          unless ['.json', '.xml', '{format}'].any? {|s| p.downcase.include? s }
            p = "#{p}.#{format}"
          end
        end

        p = p.sub("{format}", self.format.to_s)

        URI.encode [Swagger.configuration.base_path, p].join("/").gsub(/\/+/, '/')
      end

      # Massage the request body into a state of readiness
      # If body is a hash, camelize all keys then convert to a json string
      def body=(value)
        if value.is_a?(Hash)
          value = value.inject({}) do |memo, (k,v)|
            memo[k.to_s.camelize(:lower).to_sym] = v
            memo
          end
        end
        @body = value
      end

      # If body is an object, JSONify it before making the actual request.
      # For form parameters, remove empty value
      def outgoing_body
        # http form
        if headers['Content-Type'] == 'application/x-www-form-urlencoded'
          data = form_params.dup
          data.each do |key, value|
            data[key] = value.to_s if value && !value.is_a?(File) # remove emtpy form parameter
          end
        elsif @body # http body is JSON
          data = @body.is_a?(String) ? @body : @body.to_json
        else
          data = nil
        end

        if Swagger.configuration.debug
          Swagger.logger.debug "HTTP request body param ~BEGIN~\n#{data}\n~END~\n"
        end

        data
      end

      # Construct a query string from the query-string-type params
      def query_string
        # Iterate over all params,
        # .. removing the ones that are part of the path itself.
        # .. stringifying values so Addressable doesn't blow up.
        query_values = {}
        self.params.each_pair do |key, value|
          next if self.path.include? "{#{key}}"                                   # skip path params
          next if value.blank? && value.class != FalseClass                       # skip empties
          if Swagger.configuration.camelize_params
            key = key.to_s.camelize(:lower).to_sym
          end
          query_values[key] = value.to_s
        end

        # We don't want to end up with '?' as our query string
        # if there aren't really any params
        return "" if query_values.blank?

        # Addressable requires query_values to be set after initialization..
        qs = Addressable::URI.new
        qs.query_values = query_values
        qs.to_s
      end

      def make
        request_options = {
          :ssl_verifypeer => Swagger.configuration.verify_ssl,
          :headers => self.headers.stringify_keys,
          :verbose => Swagger.configuration.debug
        }
        response = case self.http_method.to_sym
        when :get,:GET
          Typhoeus::Request.get(
            self.url,
            request_options
          )

        when :post,:POST
          Typhoeus::Request.post(
            self.url,
            request_options.merge(:body => self.outgoing_body)
          )

        when :patch,:PATCH
          Typhoeus::Request.patch(
            self.url,
            request_options.merge(:body => self.outgoing_body)
          )

        when :put,:PUT
          Typhoeus::Request.put(
            self.url,
            request_options.merge(:body => self.outgoing_body)
          )

        when :delete,:DELETE
          Typhoeus::Request.delete(
            self.url,
            request_options.merge(:body => self.outgoing_body)
          )
        end

        if Swagger.configuration.debug
          Swagger.logger.debug "HTTP response body ~BEGIN~\n#{response.body}\n~END~\n"
        end

        Response.new(response)
      end

      def response
        self.make
      end

      def response_code_pretty
        return unless @response.present?
        @response.code.to_s
      end

      def response_headers_pretty
        return unless @response.present?
        # JSON.pretty_generate(@response.headers).gsub(/\n/, '<br/>') # <- This was for RestClient
        @response.headers.gsub(/\n/, '<br/>') # <- This is for Typhoeus
      end

      # return 'Accept' based on an array of accept provided
      # @param [Array] header_accept_array Array fo 'Accept'
      # @return String Accept (e.g. application/json)
      def self.select_header_accept header_accept_array
        if header_accept_array.empty?
          return
        elsif header_accept_array.any?{ |s| s.casecmp('application/json')==0 }
          'application/json' # look for json data by default
        else
          header_accept_array.join(',')
        end
      end

      # return the content type based on an array of content-type provided
      # @param [Array] content_type_array Array fo content-type
      # @return String Content-Type (e.g. application/json)
      def self.select_header_content_type content_type_array
        if content_type_array.empty?
          'application/json' # use application/json by default
        elsif content_type_array.any?{ |s| s.casecmp('application/json')==0 }
          'application/json' # use application/json if it's included
        else
          content_type_array[0]; # otherwise, use the first one
        end
      end

      # static method to convert object (array, hash, object, etc) to JSON string
      # @param model object to be converted into JSON string
      # @return string JSON string representation of the object
      def self.object_to_http_body model
        return if model.nil?
        _body = nil
        if model.is_a?(Array)
          _body = model.map{|m| object_to_hash(m) }
        else
          _body = object_to_hash(model)
        end
        _body.to_json
      end

      # static method to convert object(non-array) to hash
      # @param obj object to be converted into JSON string
      # @return string JSON string representation of the object
      def self.object_to_hash obj
        if obj.respond_to?(:to_hash)
          obj.to_hash
        else
          obj
        end
      end

    end
  end
end

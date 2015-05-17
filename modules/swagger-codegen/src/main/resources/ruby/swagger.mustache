require 'monkey'
require 'swagger/configuration'
require 'swagger/request'
require 'swagger/response'
require 'swagger/version'
require 'logger'
require 'json'

module Swagger
    
  @configuration = Configuration.new

  class << self
    attr_accessor :logger
    
    # A Swagger configuration object. Must act like a hash and return sensible
    # values for all Swagger configuration options. See Swagger::Configuration.
    attr_accessor :configuration

    attr_accessor :resources
    
    # Call this method to modify defaults in your initializers.
    #
    # @example
    #   Swagger.configure do |config|
    #     config.api_key = '1234567890abcdef'     # required
    #     config.username = 'wordlover'           # optional, but needed for user-related functions
    #     config.password = 'i<3words'            # optional, but needed for user-related functions
    #     config.format = 'json'                  # optional, defaults to 'json'
    #   end
    #
    def configure
      yield(configuration) if block_given?

      # Configure logger.  Default to use Rails
      self.logger ||= configuration.logger || (defined?(Rails) ? Rails.logger : Logger.new(STDOUT))

      # remove :// from scheme
      configuration.scheme.sub!(/:\/\//, '')

      # remove http(s):// and anything after a slash
      configuration.host.sub!(/https?:\/\//, '')
      configuration.host = configuration.host.split('/').first

      # Add leading and trailing slashes to base_path
      configuration.base_path = "/#{configuration.base_path}".gsub(/\/+/, '/')
      configuration.base_path = "" if configuration.base_path == "/"
    end
    
    def authenticated?
      Swagger.configuration.auth_token.present?
    end
    
    def de_authenticate
      Swagger.configuration.auth_token = nil
    end
    
    def authenticate
      return if Swagger.authenticated?
      
      if Swagger.configuration.username.blank? || Swagger.configuration.password.blank?
        raise ClientError, "Username and password are required to authenticate."
      end
      
      request = Swagger::Request.new(
        :get, 
        "account/authenticate/{username}", 
        :params => {
          :username => Swagger.configuration.username, 
          :password => Swagger.configuration.password
        }
      )
      
      response_body = request.response.body
      Swagger.configuration.auth_token = response_body['token']
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

class ServerError < StandardError
end

class ClientError < StandardError
end

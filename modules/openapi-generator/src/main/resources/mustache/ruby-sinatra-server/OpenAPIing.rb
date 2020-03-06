require 'json'
require 'sinatra/base'
require 'sinatra/cross_origin'

class Configuration
  attr_accessor :base_path, :api_version, :openapi_version, :format_specifier

  def initialize
    @api_version = '1.0'
    @base_path = 'http://localhost:4567'
    @openapi_version = '1.1'
    @format_specifier = ".json"
  end
end

class OpenAPIing < Sinatra::Base
  register Sinatra::CrossOrigin

  @@routes = {}
  @@configuration = Configuration.new

  attr_accessor :configuration

  def self.configure
    get("/resources" + @@configuration.format_specifier) {
      cross_origin
      OpenAPIing.to_resource_listing
    }

    # for openapi.yaml
    get("/openapi.yaml") {
      cross_origin
      File.read("./openapi.yaml");
    }

    @@configuration ||= Configuration.new
    yield(@@configuration) if block_given?
  end
  
  def self.add_route(method, path, swag={}, opts={}, &block)
    #fullPath = swag["resourcePath"].to_s + @@configuration.format_specifier + path
    fullPath = path.gsub(/{(.*?)}/, ':\1')
    
    accepted = case method.to_s.downcase
      when 'get'
        get(fullPath, opts, &block)
        true
      when 'post'
        post(fullPath, opts, &block)
        true
      when 'delete'
        delete(fullPath, opts, &block)
        true
      when 'put' 
        put(fullPath, opts, &block)
        true
      else
        puts "Error adding route: #{method} #{fullPath}"
        false
    end

    if accepted then
      resourcePath = swag["resourcePath"].to_s
      ops = @@routes[resourcePath]
      if ops.nil?
        ops = Array.new
        @@routes.merge!(resourcePath => ops)

        get(resourcePath + @@configuration.format_specifier) do
          cross_origin
          OpenAPIing.to_api(resourcePath)
        end
      end

      swag.merge!("httpMethod" => method.to_s.upcase)
      ops.push(swag)
    end
  end
  
  def self.to_resource_listing
    apis = Array.new
    (@@routes.keys).each do |key|
      api = {
        "path" => (key + ".{format}"),
        "description" => "no description"
      }
      apis.push api
    end
  
    resource = {
      "apiVersion" => @@configuration.api_version,
      "openapiVersion" => @@configuration.openapi_version,
      "apis" => apis
    }

    resource.to_json
  end
  
  def self.to_api(resourcePath)
    apis = {}
    models = []

    @@routes[resourcePath].each do |route|
      endpoint = route["endpoint"].gsub(/:(\w+)(\/?)/,'{\1}\2')
      path = (resourcePath + ".{format}" + endpoint)
      api = apis[path]
      if api.nil?
        api = {"path" => path, "description" => "description", "operations" => []}
        apis.merge!(path => api)
      end
      
      parameters = route["parameters"]

      unless parameters.nil? then
        parameters.each do |param|
          av_string = param["allowableValues"]
          unless av_string.nil?
            if av_string.count('[') > 0
              pattern = /^([A-Z]*)\[(.*)\]/
              match = pattern.match av_string
              case match.to_a[1]
                when "LIST"
                  allowables = match.to_a[2].split(',')
                  param["allowableValues"] = {
                    "valueType" => "LIST",
                    "values" => allowables
                  }
                when "RANGE"
                  allowables = match.to_a[2].split(',')
                  param["allowableValues"] = {
                    "valueType" => "RANGE",
                    "min" => allowables[0],
                    "max" => allowables[1]
                  }
              end                
            end
          end
        end
      end
      
      op = {
        "httpMethod" => route["httpMethod"],
        "description" => route["summary"],
        "responseClass" => route["responseClass"],
        "notes" => route["notes"],
        "nickname" => route["nickname"],
        "summary" => route["summary"],
        "parameters" => route["parameters"]
      }
      api["operations"].push(op)
    end

    api_listing = {
      "apiVersion" => @@configuration.api_version,
      "openapiVersion" => @@configuration.openapi_version,
      "basePath" => @@configuration.base_path,
      "resourcePath" => resourcePath,
      "apis" => apis.values,
      "models" => models
    }
    api_listing.to_json
  end
end

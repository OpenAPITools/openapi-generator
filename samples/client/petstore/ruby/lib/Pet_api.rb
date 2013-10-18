require "uri"

class Pet_api
  basePath = "http://petstore.swagger.wordnik.com/api"
  # apiInvoker = APIInvoker

  def self.escapeString(string)
    URI.encode(string.to_s)
  end

  def self.get_pet_by_id (pet_id,opts={})
    query_param_keys = []

    # verify existence of params
    raise "pet_id is required" if pet_id.nil?
    # set default values and merge with input
    options = {
    :pet_id => pet_id}.merge(opts)

    #resource path
    path = "/pet/{petId}".sub('{format}','json').sub('{' + 'petId' + '}', escapeString(pet_id))
    
    
    # pull querystring keys from options
    queryopts = options.select do |key,value|
      query_param_keys.include? key
    end
    
    headers = nil
    post_body = nil
    response = Swagger::Request.new(:GET, path, {:params=>queryopts,:headers=>headers, :body=>post_body }).make.body
    Pet.new(response)
  end

def self.delete_pet (pet_id,opts={})
    query_param_keys = []

    # verify existence of params
    raise "pet_id is required" if pet_id.nil?
    # set default values and merge with input
    options = {
    :pet_id => pet_id}.merge(opts)

    #resource path
    path = "/pet/{petId}".sub('{format}','json').sub('{' + 'petId' + '}', escapeString(pet_id))
    
    
    # pull querystring keys from options
    queryopts = options.select do |key,value|
      query_param_keys.include? key
    end
    
    headers = nil
    post_body = nil
    Swagger::Request.new(:DELETE, path, {:params=>queryopts,:headers=>headers, :body=>post_body}).make
    
  end

def self.partial_update (pet_id,body,opts={})
    query_param_keys = []

    # verify existence of params
    raise "pet_id is required" if pet_id.nil?
    raise "body is required" if body.nil?
    # set default values and merge with input
    options = {
    :pet_id => pet_id,
      :body => body}.merge(opts)

    #resource path
    path = "/pet/{petId}".sub('{format}','json').sub('{' + 'petId' + '}', escapeString(pet_id))
    
    
    # pull querystring keys from options
    queryopts = options.select do |key,value|
      query_param_keys.include? key
    end
    
    headers = nil
    post_body = nil
    if body != nil
      if body.is_a?(Array)
        array = Array.new
        body.each do |item|
          if item.respond_to?("to_body".to_sym)
            array.push item.to_body
          else
            array.push item
          end
        end
        post_body = array

      else 
        if body.respond_to?("to_body".to_sym)
	        post_body = body.to_body
	      else
	        post_body = body
	      end
      end
    end
    response = Swagger::Request.new(:PATCH, path, {:params=>queryopts,:headers=>headers, :body=>post_body }).make.body
    response.map {|response|Pet.new(response)}
  end

def self.update_pet_with_form (pet_id,name,status,opts={})
    query_param_keys = []

    # verify existence of params
    raise "pet_id is required" if pet_id.nil?
    # set default values and merge with input
    options = {
    :pet_id => pet_id,
      :name => name,
      :status => status}.merge(opts)

    #resource path
    path = "/pet/{petId}".sub('{format}','json').sub('{' + 'petId' + '}', escapeString(pet_id))
    
    
    # pull querystring keys from options
    queryopts = options.select do |key,value|
      query_param_keys.include? key
    end
    
    headers = nil
    post_body = nil
    Swagger::Request.new(:POST, path, {:params=>queryopts,:headers=>headers, :body=>post_body}).make
    
  end

def self.upload_file (additional_metadata,body,opts={})
    query_param_keys = []

    # set default values and merge with input
    options = {
    :additional_metadata => additional_metadata,
      :body => body}.merge(opts)

    #resource path
    path = "/pet/uploadImage".sub('{format}','json')
    
    # pull querystring keys from options
    queryopts = options.select do |key,value|
      query_param_keys.include? key
    end
    
    headers = nil
    post_body = nil
    if body != nil
      if body.is_a?(Array)
        array = Array.new
        body.each do |item|
          if item.respond_to?("to_body".to_sym)
            array.push item.to_body
          else
            array.push item
          end
        end
        post_body = array

      else 
        if body.respond_to?("to_body".to_sym)
	        post_body = body.to_body
	      else
	        post_body = body
	      end
      end
    end
    Swagger::Request.new(:POST, path, {:params=>queryopts,:headers=>headers, :body=>post_body}).make
    
  end

def self.add_pet (body,opts={})
    query_param_keys = []

    # verify existence of params
    raise "body is required" if body.nil?
    # set default values and merge with input
    options = {
    :body => body}.merge(opts)

    #resource path
    path = "/pet".sub('{format}','json')
    
    # pull querystring keys from options
    queryopts = options.select do |key,value|
      query_param_keys.include? key
    end
    
    headers = nil
    post_body = nil
    if body != nil
      if body.is_a?(Array)
        array = Array.new
        body.each do |item|
          if item.respond_to?("to_body".to_sym)
            array.push item.to_body
          else
            array.push item
          end
        end
        post_body = array

      else 
        if body.respond_to?("to_body".to_sym)
	        post_body = body.to_body
	      else
	        post_body = body
	      end
      end
    end
    Swagger::Request.new(:POST, path, {:params=>queryopts,:headers=>headers, :body=>post_body}).make
    
  end

def self.update_pet (body,opts={})
    query_param_keys = []

    # verify existence of params
    raise "body is required" if body.nil?
    # set default values and merge with input
    options = {
    :body => body}.merge(opts)

    #resource path
    path = "/pet".sub('{format}','json')
    
    # pull querystring keys from options
    queryopts = options.select do |key,value|
      query_param_keys.include? key
    end
    
    headers = nil
    post_body = nil
    if body != nil
      if body.is_a?(Array)
        array = Array.new
        body.each do |item|
          if item.respond_to?("to_body".to_sym)
            array.push item.to_body
          else
            array.push item
          end
        end
        post_body = array

      else 
        if body.respond_to?("to_body".to_sym)
	        post_body = body.to_body
	      else
	        post_body = body
	      end
      end
    end
    Swagger::Request.new(:PUT, path, {:params=>queryopts,:headers=>headers, :body=>post_body}).make
    
  end

def self.find_pets_by_status (status= "available",opts={})
    query_param_keys = [:status]

    # verify existence of params
    raise "status is required" if status.nil?
    # set default values and merge with input
    options = {
    :status => status}.merge(opts)

    #resource path
    path = "/pet/findByStatus".sub('{format}','json')
    
    # pull querystring keys from options
    queryopts = options.select do |key,value|
      query_param_keys.include? key
    end
    
    headers = nil
    post_body = nil
    response = Swagger::Request.new(:GET, path, {:params=>queryopts,:headers=>headers, :body=>post_body }).make.body
    response.map {|response|Pet.new(response)}
  end

def self.find_pets_by_tags (tags,opts={})
    query_param_keys = [:tags]

    # verify existence of params
    raise "tags is required" if tags.nil?
    # set default values and merge with input
    options = {
    :tags => tags}.merge(opts)

    #resource path
    path = "/pet/findByTags".sub('{format}','json')
    
    # pull querystring keys from options
    queryopts = options.select do |key,value|
      query_param_keys.include? key
    end
    
    headers = nil
    post_body = nil
    response = Swagger::Request.new(:GET, path, {:params=>queryopts,:headers=>headers, :body=>post_body }).make.body
    response.map {|response|Pet.new(response)}
  end

end


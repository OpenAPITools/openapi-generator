require "uri"

class User_api
  basePath = "http://petstore.swagger.wordnik.com/api"
  # apiInvoker = APIInvoker

  def self.escapeString(string)
    URI.encode(string.to_s)
  end

  def self.create_users_with_array_input (body,opts={})
    query_param_keys = []

    # verify existence of params
    raise "body is required" if body.nil?
    # set default values and merge with input
    options = { :body => body}.merge(opts)

    #resource path
    path = "/user.{format}/createWithArray".sub('{format}','json')
    
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

def self.create_user (body,opts={})
    query_param_keys = []

    # verify existence of params
    raise "body is required" if body.nil?
    # set default values and merge with input
    options = { :body => body}.merge(opts)

    #resource path
    path = "/user.{format}".sub('{format}','json')
    
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

def self.create_users_with_list_input (body,opts={})
    query_param_keys = []

    # verify existence of params
    raise "body is required" if body.nil?
    # set default values and merge with input
    options = { :body => body}.merge(opts)

    #resource path
    path = "/user.{format}/createWithList".sub('{format}','json')
    
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

def self.update_user (username,body,opts={})
    query_param_keys = []

    # verify existence of params
    raise "username is required" if username.nil?
    raise "body is required" if body.nil?
    # set default values and merge with input
    options = { :username => username, :body => body}.merge(opts)

    #resource path
    path = "/user.{format}/{username}".sub('{format}','json').sub('{' + 'username' + '}', escapeString(username))
    
    
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

def self.delete_user (username,opts={})
    query_param_keys = []

    # verify existence of params
    raise "username is required" if username.nil?
    # set default values and merge with input
    options = { :username => username}.merge(opts)

    #resource path
    path = "/user.{format}/{username}".sub('{format}','json').sub('{' + 'username' + '}', escapeString(username))
    
    
    # pull querystring keys from options
    queryopts = options.select do |key,value|
      query_param_keys.include? key
    end
    
    headers = nil
    post_body = nil
    Swagger::Request.new(:DELETE, path, {:params=>queryopts,:headers=>headers, :body=>post_body}).make
    
  end

def self.get_user_by_name (username,opts={})
    query_param_keys = []

    # verify existence of params
    raise "username is required" if username.nil?
    # set default values and merge with input
    options = { :username => username}.merge(opts)

    #resource path
    path = "/user.{format}/{username}".sub('{format}','json').sub('{' + 'username' + '}', escapeString(username))
    
    
    # pull querystring keys from options
    queryopts = options.select do |key,value|
      query_param_keys.include? key
    end
    
    headers = nil
    post_body = nil
    response = Swagger::Request.new(:GET, path, {:params=>queryopts,:headers=>headers, :body=>post_body }).make.body
    User.new(response)
  end

def self.login_user (username,password,opts={})
    query_param_keys = [:username,:password]

    # verify existence of params
    raise "username is required" if username.nil?
    raise "password is required" if password.nil?
    # set default values and merge with input
    options = { :username => username, :password => password}.merge(opts)

    #resource path
    path = "/user.{format}/login".sub('{format}','json')
    
    # pull querystring keys from options
    queryopts = options.select do |key,value|
      query_param_keys.include? key
    end
    
    headers = nil
    post_body = nil
    response = Swagger::Request.new(:GET, path, {:params=>queryopts,:headers=>headers, :body=>post_body }).make.body
    string.new(response)
  end

def self.logout_user (opts={})
    query_param_keys = []

    # set default values and merge with input
    options = { }.merge(opts)

    #resource path
    path = "/user.{format}/logout".sub('{format}','json')
    
    # pull querystring keys from options
    queryopts = options.select do |key,value|
      query_param_keys.include? key
    end
    
    headers = nil
    post_body = nil
    Swagger::Request.new(:GET, path, {:params=>queryopts,:headers=>headers, :body=>post_body}).make
    
  end

end


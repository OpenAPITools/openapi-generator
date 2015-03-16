require "uri"

class UserApi
  basePath = "http://petstore.swagger.io/v2"
  # apiInvoker = APIInvoker

  def self.escapeString(string)
    URI.encode(string.to_s)
  end

  
  def self.createUser (body, opts={})
    query_param_keys = []

    
    
    # set default values and merge with input
    options = {
    
      :body => body
    
    }.merge(opts)

    #resource path
    path = "/user".sub('{format}','json')
    
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


  def self.createUsersWithArrayInput (body, opts={})
    query_param_keys = []

    
    
    # set default values and merge with input
    options = {
    
      :body => body
    
    }.merge(opts)

    #resource path
    path = "/user/createWithArray".sub('{format}','json')
    
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


  def self.createUsersWithListInput (body, opts={})
    query_param_keys = []

    
    
    # set default values and merge with input
    options = {
    
      :body => body
    
    }.merge(opts)

    #resource path
    path = "/user/createWithList".sub('{format}','json')
    
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


  def self.loginUser (username,password, opts={})
    query_param_keys = [:username,:password]

    
    
    # set default values and merge with input
    options = {
    
      :username => username,
      
    
      :password => password
    
    }.merge(opts)

    #resource path
    path = "/user/login".sub('{format}','json')
    
    # pull querystring keys from options
    queryopts = options.select do |key,value|
      query_param_keys.include? key
    end
    
    
    headers = nil
    
    
    post_body = nil
    

    
    response = Swagger::Request.new(:GET, path, {:params=>queryopts,:headers=>headers, :body=>post_body }).make.body
     string.new(response)
    
    
  
  end


  def self.logoutUser ( opts={})
    query_param_keys = []

    
    
    # set default values and merge with input
    options = {
    
    }.merge(opts)

    #resource path
    path = "/user/logout".sub('{format}','json')
    
    # pull querystring keys from options
    queryopts = options.select do |key,value|
      query_param_keys.include? key
    end
    
    
    headers = nil
    
    
    post_body = nil
    

    
    
    Swagger::Request.new(:GET, path, {:params=>queryopts,:headers=>headers, :body=>post_body}).make
    
  
  end


  def self.getUserByName (username, opts={})
    query_param_keys = []

    
    
    # set default values and merge with input
    options = {
    
      :username => username
    
    }.merge(opts)

    #resource path
    path = "/user/{username}".sub('{format}','json').sub('{' + 'username' + '}', escapeString(username))
    
    
    # pull querystring keys from options
    queryopts = options.select do |key,value|
      query_param_keys.include? key
    end
    
    
    headers = nil
    
    
    post_body = nil
    

    
    response = Swagger::Request.new(:GET, path, {:params=>queryopts,:headers=>headers, :body=>post_body }).make.body
     User.new(response)
    
    
  
  end


  def self.updateUser (username,body, opts={})
    query_param_keys = []

    
    
    # set default values and merge with input
    options = {
    
      :username => username,
      
    
      :body => body
    
    }.merge(opts)

    #resource path
    path = "/user/{username}".sub('{format}','json').sub('{' + 'username' + '}', escapeString(username))
    
    
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


  def self.deleteUser (username, opts={})
    query_param_keys = []

    
    
    # set default values and merge with input
    options = {
    
      :username => username
    
    }.merge(opts)

    #resource path
    path = "/user/{username}".sub('{format}','json').sub('{' + 'username' + '}', escapeString(username))
    
    
    # pull querystring keys from options
    queryopts = options.select do |key,value|
      query_param_keys.include? key
    end
    
    
    headers = nil
    
    
    post_body = nil
    

    
    
    Swagger::Request.new(:DELETE, path, {:params=>queryopts,:headers=>headers, :body=>post_body}).make
    
  
  end

end

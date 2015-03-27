require "uri"

class UserApi
  basePath = "http://petstore.swagger.io/v2"
  # apiInvoker = APIInvoker

  def self.escapeString(string)
    URI.encode(string.to_s)
  end


  # Create user
  # This can only be done by the logged in user.
  # @param body Created user object
  # @return void
  def self.createUser (body, opts={})
    query_param_keys = []

    
    
    # set default values and merge with input
    options = {
      :'body' => body
      
    }.merge(opts)

    #resource path
    path = "/user".sub('{format}','json')
    
    # pull querystring keys from options
    queryopts = options.select do |key,value|
      query_param_keys.include? key
    end

    # header parameters, if any
    headers = {}
    
    
    # http body (model)
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
    
    # form parameters
    form_parameter_hash = {}
    
    
    
    Swagger::Request.new(:POST, path, {:params=>queryopts,:headers=>headers, :body=>post_body, :form_params => form_parameter_hash }).make
    
  
  end

  # Creates list of users with given input array
  # 
  # @param body List of user object
  # @return void
  def self.createUsersWithArrayInput (body, opts={})
    query_param_keys = []

    
    
    # set default values and merge with input
    options = {
      :'body' => body
      
    }.merge(opts)

    #resource path
    path = "/user/createWithArray".sub('{format}','json')
    
    # pull querystring keys from options
    queryopts = options.select do |key,value|
      query_param_keys.include? key
    end

    # header parameters, if any
    headers = {}
    
    
    # http body (model)
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
    
    # form parameters
    form_parameter_hash = {}
    
    
    
    Swagger::Request.new(:POST, path, {:params=>queryopts,:headers=>headers, :body=>post_body, :form_params => form_parameter_hash }).make
    
  
  end

  # Creates list of users with given input array
  # 
  # @param body List of user object
  # @return void
  def self.createUsersWithListInput (body, opts={})
    query_param_keys = []

    
    
    # set default values and merge with input
    options = {
      :'body' => body
      
    }.merge(opts)

    #resource path
    path = "/user/createWithList".sub('{format}','json')
    
    # pull querystring keys from options
    queryopts = options.select do |key,value|
      query_param_keys.include? key
    end

    # header parameters, if any
    headers = {}
    
    
    # http body (model)
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
    
    # form parameters
    form_parameter_hash = {}
    
    
    
    Swagger::Request.new(:POST, path, {:params=>queryopts,:headers=>headers, :body=>post_body, :form_params => form_parameter_hash }).make
    
  
  end

  # Logs user into the system
  # 
  # @param username The user name for login
  # @param password The password for login in clear text
  # @return string
  def self.loginUser (username, password, opts={})
    query_param_keys = [:username,:password]

    
    
    # set default values and merge with input
    options = {
      :'username' => username,
      :'password' => password
      
    }.merge(opts)

    #resource path
    path = "/user/login".sub('{format}','json')
    
    # pull querystring keys from options
    queryopts = options.select do |key,value|
      query_param_keys.include? key
    end

    # header parameters, if any
    headers = {}
    
    
    # http body (model)
    post_body = nil
    
    # form parameters
    form_parameter_hash = {}
    
    
    response = Swagger::Request.new(:GET, path, {:params=>queryopts,:headers=>headers, :body=>post_body, :form_params => form_parameter_hash }).make.body
     string.new(response)
    
    
  
  end

  # Logs out current logged in user session
  # 
  # @return void
  def self.logoutUser (opts={})
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

    # header parameters, if any
    headers = {}
    
    
    # http body (model)
    post_body = nil
    
    # form parameters
    form_parameter_hash = {}
    
    
    
    Swagger::Request.new(:GET, path, {:params=>queryopts,:headers=>headers, :body=>post_body, :form_params => form_parameter_hash }).make
    
  
  end

  # Get user by user name
  # 
  # @param username The name that needs to be fetched. Use user1 for testing. 
  # @return User
  def self.getUserByName (username, opts={})
    query_param_keys = []

    
    
    # set default values and merge with input
    options = {
      :'username' => username
      
    }.merge(opts)

    #resource path
    path = "/user/{username}".sub('{format}','json').sub('{' + 'username' + '}', escapeString(username))
    
    
    # pull querystring keys from options
    queryopts = options.select do |key,value|
      query_param_keys.include? key
    end

    # header parameters, if any
    headers = {}
    
    
    # http body (model)
    post_body = nil
    
    # form parameters
    form_parameter_hash = {}
    
    
    response = Swagger::Request.new(:GET, path, {:params=>queryopts,:headers=>headers, :body=>post_body, :form_params => form_parameter_hash }).make.body
     User.new(response)
    
    
  
  end

  # Updated user
  # This can only be done by the logged in user.
  # @param username name that need to be deleted
  # @param body Updated user object
  # @return void
  def self.updateUser (username, body, opts={})
    query_param_keys = []

    
    
    # set default values and merge with input
    options = {
      :'username' => username,
      :'body' => body
      
    }.merge(opts)

    #resource path
    path = "/user/{username}".sub('{format}','json').sub('{' + 'username' + '}', escapeString(username))
    
    
    # pull querystring keys from options
    queryopts = options.select do |key,value|
      query_param_keys.include? key
    end

    # header parameters, if any
    headers = {}
    
    
    # http body (model)
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
    
    # form parameters
    form_parameter_hash = {}
    
    
    
    Swagger::Request.new(:PUT, path, {:params=>queryopts,:headers=>headers, :body=>post_body, :form_params => form_parameter_hash }).make
    
  
  end

  # Delete user
  # This can only be done by the logged in user.
  # @param username The name that needs to be deleted
  # @return void
  def self.deleteUser (username, opts={})
    query_param_keys = []

    
    
    # set default values and merge with input
    options = {
      :'username' => username
      
    }.merge(opts)

    #resource path
    path = "/user/{username}".sub('{format}','json').sub('{' + 'username' + '}', escapeString(username))
    
    
    # pull querystring keys from options
    queryopts = options.select do |key,value|
      query_param_keys.include? key
    end

    # header parameters, if any
    headers = {}
    
    
    # http body (model)
    post_body = nil
    
    # form parameters
    form_parameter_hash = {}
    
    
    
    Swagger::Request.new(:DELETE, path, {:params=>queryopts,:headers=>headers, :body=>post_body, :form_params => form_parameter_hash }).make
    
  
  end
end

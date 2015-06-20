require "uri"

module SwaggerClient
  class UserApi
    basePath = "http://petstore.swagger.io/v2"
    # apiInvoker = APIInvoker

    # Create user
    # This can only be done by the logged in user.
    # @param [Hash] opts the optional parameters
    # @option opts [User] :body Created user object
    # @return [nil]
    def self.create_user(opts = {})
      if Swagger.configuration.debug
        Swagger.logger.debug "Calling API: UserApi#create_user ..."
      end
      

      # resource path
      path = "/user".sub('{format}','json')

      # query parameters
      query_params = {}

      # header parameters
      header_params = {}

      # HTTP header 'Accept' (if needed)
      _header_accept = ['application/json', 'application/xml']
      _header_accept_result = Swagger::Request.select_header_accept(_header_accept) and header_params['Accept'] = _header_accept_result

      # HTTP header 'Content-Type'
      _header_content_type = []
      header_params['Content-Type'] = Swagger::Request.select_header_content_type(_header_content_type)

      # form parameters
      form_params = {}

      # http body (model)
      post_body = Swagger::Request.object_to_http_body(opts[:'body'])
      

      auth_names = []
      Swagger::Request.new(:POST, path, {:params => query_params, :headers => header_params, :form_params => form_params, :body => post_body, :auth_names => auth_names}).make
      if Swagger.configuration.debug
        Swagger.logger.debug "API called: UserApi#create_user"
      end
      nil
    end

    # Creates list of users with given input array
    # 
    # @param [Hash] opts the optional parameters
    # @option opts [Array<User>] :body List of user object
    # @return [nil]
    def self.create_users_with_array_input(opts = {})
      if Swagger.configuration.debug
        Swagger.logger.debug "Calling API: UserApi#create_users_with_array_input ..."
      end
      

      # resource path
      path = "/user/createWithArray".sub('{format}','json')

      # query parameters
      query_params = {}

      # header parameters
      header_params = {}

      # HTTP header 'Accept' (if needed)
      _header_accept = ['application/json', 'application/xml']
      _header_accept_result = Swagger::Request.select_header_accept(_header_accept) and header_params['Accept'] = _header_accept_result

      # HTTP header 'Content-Type'
      _header_content_type = []
      header_params['Content-Type'] = Swagger::Request.select_header_content_type(_header_content_type)

      # form parameters
      form_params = {}

      # http body (model)
      post_body = Swagger::Request.object_to_http_body(opts[:'body'])
      

      auth_names = []
      Swagger::Request.new(:POST, path, {:params => query_params, :headers => header_params, :form_params => form_params, :body => post_body, :auth_names => auth_names}).make
      if Swagger.configuration.debug
        Swagger.logger.debug "API called: UserApi#create_users_with_array_input"
      end
      nil
    end

    # Creates list of users with given input array
    # 
    # @param [Hash] opts the optional parameters
    # @option opts [Array<User>] :body List of user object
    # @return [nil]
    def self.create_users_with_list_input(opts = {})
      if Swagger.configuration.debug
        Swagger.logger.debug "Calling API: UserApi#create_users_with_list_input ..."
      end
      

      # resource path
      path = "/user/createWithList".sub('{format}','json')

      # query parameters
      query_params = {}

      # header parameters
      header_params = {}

      # HTTP header 'Accept' (if needed)
      _header_accept = ['application/json', 'application/xml']
      _header_accept_result = Swagger::Request.select_header_accept(_header_accept) and header_params['Accept'] = _header_accept_result

      # HTTP header 'Content-Type'
      _header_content_type = []
      header_params['Content-Type'] = Swagger::Request.select_header_content_type(_header_content_type)

      # form parameters
      form_params = {}

      # http body (model)
      post_body = Swagger::Request.object_to_http_body(opts[:'body'])
      

      auth_names = []
      Swagger::Request.new(:POST, path, {:params => query_params, :headers => header_params, :form_params => form_params, :body => post_body, :auth_names => auth_names}).make
      if Swagger.configuration.debug
        Swagger.logger.debug "API called: UserApi#create_users_with_list_input"
      end
      nil
    end

    # Logs user into the system
    # 
    # @param [Hash] opts the optional parameters
    # @option opts [String] :username The user name for login
    # @option opts [String] :password The password for login in clear text
    # @return [String]
    def self.login_user(opts = {})
      if Swagger.configuration.debug
        Swagger.logger.debug "Calling API: UserApi#login_user ..."
      end
      

      # resource path
      path = "/user/login".sub('{format}','json')

      # query parameters
      query_params = {}
      query_params[:'username'] = opts[:'username'] if opts[:'username']
      query_params[:'password'] = opts[:'password'] if opts[:'password']

      # header parameters
      header_params = {}

      # HTTP header 'Accept' (if needed)
      _header_accept = ['application/json', 'application/xml']
      _header_accept_result = Swagger::Request.select_header_accept(_header_accept) and header_params['Accept'] = _header_accept_result

      # HTTP header 'Content-Type'
      _header_content_type = []
      header_params['Content-Type'] = Swagger::Request.select_header_content_type(_header_content_type)

      # form parameters
      form_params = {}

      # http body (model)
      post_body = nil
      

      auth_names = []
      response = Swagger::Request.new(:GET, path, {:params => query_params, :headers => header_params, :form_params => form_params, :body => post_body, :auth_names => auth_names}).make
      result = response.deserialize('String')
      if Swagger.configuration.debug
        Swagger.logger.debug "API called: UserApi#login_user. Result: #{result.inspect}"
      end
      result
    end

    # Logs out current logged in user session
    # 
    # @param [Hash] opts the optional parameters
    # @return [nil]
    def self.logout_user(opts = {})
      if Swagger.configuration.debug
        Swagger.logger.debug "Calling API: UserApi#logout_user ..."
      end
      

      # resource path
      path = "/user/logout".sub('{format}','json')

      # query parameters
      query_params = {}

      # header parameters
      header_params = {}

      # HTTP header 'Accept' (if needed)
      _header_accept = ['application/json', 'application/xml']
      _header_accept_result = Swagger::Request.select_header_accept(_header_accept) and header_params['Accept'] = _header_accept_result

      # HTTP header 'Content-Type'
      _header_content_type = []
      header_params['Content-Type'] = Swagger::Request.select_header_content_type(_header_content_type)

      # form parameters
      form_params = {}

      # http body (model)
      post_body = nil
      

      auth_names = []
      Swagger::Request.new(:GET, path, {:params => query_params, :headers => header_params, :form_params => form_params, :body => post_body, :auth_names => auth_names}).make
      if Swagger.configuration.debug
        Swagger.logger.debug "API called: UserApi#logout_user"
      end
      nil
    end

    # Get user by user name
    # 
    # @param username The name that needs to be fetched. Use user1 for testing. 
    # @param [Hash] opts the optional parameters
    # @return [User]
    def self.get_user_by_name(username, opts = {})
      if Swagger.configuration.debug
        Swagger.logger.debug "Calling API: UserApi#get_user_by_name ..."
      end
      
      # verify the required parameter 'username' is set
      raise "Missing the required parameter 'username' when calling get_user_by_name" if username.nil?
      

      # resource path
      path = "/user/{username}".sub('{format}','json').sub('{' + 'username' + '}', username.to_s)

      # query parameters
      query_params = {}

      # header parameters
      header_params = {}

      # HTTP header 'Accept' (if needed)
      _header_accept = ['application/json', 'application/xml']
      _header_accept_result = Swagger::Request.select_header_accept(_header_accept) and header_params['Accept'] = _header_accept_result

      # HTTP header 'Content-Type'
      _header_content_type = []
      header_params['Content-Type'] = Swagger::Request.select_header_content_type(_header_content_type)

      # form parameters
      form_params = {}

      # http body (model)
      post_body = nil
      

      auth_names = []
      response = Swagger::Request.new(:GET, path, {:params => query_params, :headers => header_params, :form_params => form_params, :body => post_body, :auth_names => auth_names}).make
      result = response.deserialize('User')
      if Swagger.configuration.debug
        Swagger.logger.debug "API called: UserApi#get_user_by_name. Result: #{result.inspect}"
      end
      result
    end

    # Updated user
    # This can only be done by the logged in user.
    # @param username name that need to be deleted
    # @param [Hash] opts the optional parameters
    # @option opts [User] :body Updated user object
    # @return [nil]
    def self.update_user(username, opts = {})
      if Swagger.configuration.debug
        Swagger.logger.debug "Calling API: UserApi#update_user ..."
      end
      
      # verify the required parameter 'username' is set
      raise "Missing the required parameter 'username' when calling update_user" if username.nil?
      

      # resource path
      path = "/user/{username}".sub('{format}','json').sub('{' + 'username' + '}', username.to_s)

      # query parameters
      query_params = {}

      # header parameters
      header_params = {}

      # HTTP header 'Accept' (if needed)
      _header_accept = ['application/json', 'application/xml']
      _header_accept_result = Swagger::Request.select_header_accept(_header_accept) and header_params['Accept'] = _header_accept_result

      # HTTP header 'Content-Type'
      _header_content_type = []
      header_params['Content-Type'] = Swagger::Request.select_header_content_type(_header_content_type)

      # form parameters
      form_params = {}

      # http body (model)
      post_body = Swagger::Request.object_to_http_body(opts[:'body'])
      

      auth_names = []
      Swagger::Request.new(:PUT, path, {:params => query_params, :headers => header_params, :form_params => form_params, :body => post_body, :auth_names => auth_names}).make
      if Swagger.configuration.debug
        Swagger.logger.debug "API called: UserApi#update_user"
      end
      nil
    end

    # Delete user
    # This can only be done by the logged in user.
    # @param username The name that needs to be deleted
    # @param [Hash] opts the optional parameters
    # @return [nil]
    def self.delete_user(username, opts = {})
      if Swagger.configuration.debug
        Swagger.logger.debug "Calling API: UserApi#delete_user ..."
      end
      
      # verify the required parameter 'username' is set
      raise "Missing the required parameter 'username' when calling delete_user" if username.nil?
      

      # resource path
      path = "/user/{username}".sub('{format}','json').sub('{' + 'username' + '}', username.to_s)

      # query parameters
      query_params = {}

      # header parameters
      header_params = {}

      # HTTP header 'Accept' (if needed)
      _header_accept = ['application/json', 'application/xml']
      _header_accept_result = Swagger::Request.select_header_accept(_header_accept) and header_params['Accept'] = _header_accept_result

      # HTTP header 'Content-Type'
      _header_content_type = []
      header_params['Content-Type'] = Swagger::Request.select_header_content_type(_header_content_type)

      # form parameters
      form_params = {}

      # http body (model)
      post_body = nil
      

      auth_names = []
      Swagger::Request.new(:DELETE, path, {:params => query_params, :headers => header_params, :form_params => form_params, :body => post_body, :auth_names => auth_names}).make
      if Swagger.configuration.debug
        Swagger.logger.debug "API called: UserApi#delete_user"
      end
      nil
    end
  end
end

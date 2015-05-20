require "uri"

module SwaggerClient
  class UserApi
    basePath = "http://petstore.swagger.io/v2"
    # apiInvoker = APIInvoker

    # Create user
    # This can only be done by the logged in user.
    # @param [Hash] opts the optional parameters
    # @option opts [User] :body Created user object
    # @return void
    def self.create_user(opts = {})
      # verify existence of params

      # resource path
      path = "/user".sub('{format}','json')

      # query parameters
      query_params = {}

      # header parameters
      header_params = {}

      _header_accept = 'application/json, application/xml'
      header_params['Accept'] = _header_accept if _header_accept != ''

      _header_content_type = []
      header_params['Content-Type'] = _header_content_type.length > 0 ? _header_content_type[0] : 'application/json'

      # form parameters
      form_params = {}

      # http body (model)
      post_body = nil
      _body_param = opts[:'body']
      if _body_param != nil
        if _body_param.is_a?(Array)
          _array = Array.new
          _body_param.each do |item|
            if item.respond_to?(:to_body)
              _array.push item.to_body
            else
              _array.push item
            end
          end
          post_body = _array
        else 
          if _body_param.respond_to?(:to_body)
            post_body = _body_param.to_body
          else
            post_body = _body_param
          end
        end
      end

      Swagger::Request.new(:POST, path, {:params => query_params,:headers => header_params, :form_params => form_params, :body => post_body}).make
    end

    # Creates list of users with given input array
    # 
    # @param [Hash] opts the optional parameters
    # @option opts [array[User]] :body List of user object
    # @return void
    def self.create_users_with_array_input(opts = {})
      # verify existence of params

      # resource path
      path = "/user/createWithArray".sub('{format}','json')

      # query parameters
      query_params = {}

      # header parameters
      header_params = {}

      _header_accept = 'application/json, application/xml'
      header_params['Accept'] = _header_accept if _header_accept != ''

      _header_content_type = []
      header_params['Content-Type'] = _header_content_type.length > 0 ? _header_content_type[0] : 'application/json'

      # form parameters
      form_params = {}

      # http body (model)
      post_body = nil
      _body_param = opts[:'body']
      if _body_param != nil
        if _body_param.is_a?(Array)
          _array = Array.new
          _body_param.each do |item|
            if item.respond_to?(:to_body)
              _array.push item.to_body
            else
              _array.push item
            end
          end
          post_body = _array
        else 
          if _body_param.respond_to?(:to_body)
            post_body = _body_param.to_body
          else
            post_body = _body_param
          end
        end
      end

      Swagger::Request.new(:POST, path, {:params => query_params,:headers => header_params, :form_params => form_params, :body => post_body}).make
    end

    # Creates list of users with given input array
    # 
    # @param [Hash] opts the optional parameters
    # @option opts [array[User]] :body List of user object
    # @return void
    def self.create_users_with_list_input(opts = {})
      # verify existence of params

      # resource path
      path = "/user/createWithList".sub('{format}','json')

      # query parameters
      query_params = {}

      # header parameters
      header_params = {}

      _header_accept = 'application/json, application/xml'
      header_params['Accept'] = _header_accept if _header_accept != ''

      _header_content_type = []
      header_params['Content-Type'] = _header_content_type.length > 0 ? _header_content_type[0] : 'application/json'

      # form parameters
      form_params = {}

      # http body (model)
      post_body = nil
      _body_param = opts[:'body']
      if _body_param != nil
        if _body_param.is_a?(Array)
          _array = Array.new
          _body_param.each do |item|
            if item.respond_to?(:to_body)
              _array.push item.to_body
            else
              _array.push item
            end
          end
          post_body = _array
        else 
          if _body_param.respond_to?(:to_body)
            post_body = _body_param.to_body
          else
            post_body = _body_param
          end
        end
      end

      Swagger::Request.new(:POST, path, {:params => query_params,:headers => header_params, :form_params => form_params, :body => post_body}).make
    end

    # Logs user into the system
    # 
    # @param [Hash] opts the optional parameters
    # @option opts [string] :username The user name for login
    # @option opts [string] :password The password for login in clear text
    # @return string
    def self.login_user(opts = {})
      # verify existence of params

      # resource path
      path = "/user/login".sub('{format}','json')

      # query parameters
      query_params = {}
      query_params[:'username'] = opts[:'username'] if opts[:'username']
      query_params[:'password'] = opts[:'password'] if opts[:'password']

      # header parameters
      header_params = {}

      _header_accept = 'application/json, application/xml'
      header_params['Accept'] = _header_accept if _header_accept != ''

      _header_content_type = []
      header_params['Content-Type'] = _header_content_type.length > 0 ? _header_content_type[0] : 'application/json'

      # form parameters
      form_params = {}

      # http body (model)
      post_body = nil

      response = Swagger::Request.new(:GET, path, {:params => query_params, :headers => header_params, :form_params => form_params, :body => post_body}).make.body
      string.new(response)
    end

    # Logs out current logged in user session
    # 
    # @param [Hash] opts the optional parameters
    # @return void
    def self.logout_user(opts = {})
      # verify existence of params

      # resource path
      path = "/user/logout".sub('{format}','json')

      # query parameters
      query_params = {}

      # header parameters
      header_params = {}

      _header_accept = 'application/json, application/xml'
      header_params['Accept'] = _header_accept if _header_accept != ''

      _header_content_type = []
      header_params['Content-Type'] = _header_content_type.length > 0 ? _header_content_type[0] : 'application/json'

      # form parameters
      form_params = {}

      # http body (model)
      post_body = nil

      Swagger::Request.new(:GET, path, {:params => query_params,:headers => header_params, :form_params => form_params, :body => post_body}).make
    end

    # Get user by user name
    # 
    # @param username The name that needs to be fetched. Use user1 for testing. 
    # @param [Hash] opts the optional parameters
    # @return User
    def self.get_user_by_name(username, opts = {})
      # verify existence of params
      raise "username is required" if username.nil?

      # resource path
      path = "/user/{username}".sub('{format}','json').sub('{' + 'username' + '}', username.to_s)

      # query parameters
      query_params = {}

      # header parameters
      header_params = {}

      _header_accept = 'application/json, application/xml'
      header_params['Accept'] = _header_accept if _header_accept != ''

      _header_content_type = []
      header_params['Content-Type'] = _header_content_type.length > 0 ? _header_content_type[0] : 'application/json'

      # form parameters
      form_params = {}

      # http body (model)
      post_body = nil

      response = Swagger::Request.new(:GET, path, {:params => query_params, :headers => header_params, :form_params => form_params, :body => post_body}).make.body
      User.new(response)
    end

    # Updated user
    # This can only be done by the logged in user.
    # @param username name that need to be deleted
    # @param [Hash] opts the optional parameters
    # @option opts [User] :body Updated user object
    # @return void
    def self.update_user(username, opts = {})
      # verify existence of params
      raise "username is required" if username.nil?

      # resource path
      path = "/user/{username}".sub('{format}','json').sub('{' + 'username' + '}', username.to_s)

      # query parameters
      query_params = {}

      # header parameters
      header_params = {}

      _header_accept = 'application/json, application/xml'
      header_params['Accept'] = _header_accept if _header_accept != ''

      _header_content_type = []
      header_params['Content-Type'] = _header_content_type.length > 0 ? _header_content_type[0] : 'application/json'

      # form parameters
      form_params = {}

      # http body (model)
      post_body = nil
      _body_param = opts[:'body']
      if _body_param != nil
        if _body_param.is_a?(Array)
          _array = Array.new
          _body_param.each do |item|
            if item.respond_to?(:to_body)
              _array.push item.to_body
            else
              _array.push item
            end
          end
          post_body = _array
        else 
          if _body_param.respond_to?(:to_body)
            post_body = _body_param.to_body
          else
            post_body = _body_param
          end
        end
      end

      Swagger::Request.new(:PUT, path, {:params => query_params,:headers => header_params, :form_params => form_params, :body => post_body}).make
    end

    # Delete user
    # This can only be done by the logged in user.
    # @param username The name that needs to be deleted
    # @param [Hash] opts the optional parameters
    # @return void
    def self.delete_user(username, opts = {})
      # verify existence of params
      raise "username is required" if username.nil?

      # resource path
      path = "/user/{username}".sub('{format}','json').sub('{' + 'username' + '}', username.to_s)

      # query parameters
      query_params = {}

      # header parameters
      header_params = {}

      _header_accept = 'application/json, application/xml'
      header_params['Accept'] = _header_accept if _header_accept != ''

      _header_content_type = []
      header_params['Content-Type'] = _header_content_type.length > 0 ? _header_content_type[0] : 'application/json'

      # form parameters
      form_params = {}

      # http body (model)
      post_body = nil

      Swagger::Request.new(:DELETE, path, {:params => query_params,:headers => header_params, :form_params => form_params, :body => post_body}).make
    end
  end
end
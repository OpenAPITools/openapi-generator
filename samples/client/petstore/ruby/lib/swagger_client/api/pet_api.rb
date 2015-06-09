require "uri"

module SwaggerClient
  class PetApi
    basePath = "http://petstore.swagger.io/v2"
    # apiInvoker = APIInvoker

    # Update an existing pet
    # 
    # @param [Hash] opts the optional parameters
    # @option opts [Pet] :body Pet object that needs to be added to the store
    # @return [nil]
    def self.update_pet(opts = {})
      

      # resource path
      path = "/pet".sub('{format}','json')

      # query parameters
      query_params = {}

      # header parameters
      header_params = {}

      # HTTP header 'Accept' (if needed)
      _header_accept = ['application/json', 'application/xml']
      _header_accept_result = Swagger::Request.select_header_accept(_header_accept) and header_params['Accept'] = _header_accept_result

      # HTTP header 'Content-Type'
      _header_content_type = ['application/json', 'application/xml']
      header_params['Content-Type'] = Swagger::Request.select_header_content_type(_header_content_type)

      # form parameters
      form_params = {}

      # http body (model)
      post_body = Swagger::Request.object_to_http_body(opts[:'body'])
      

      auth_names = ['petstore_auth']
      Swagger::Request.new(:PUT, path, {:params => query_params,:headers => header_params, :form_params => form_params, :body => post_body, :auth_names => auth_names}).make
      nil
    end

    # Add a new pet to the store
    # 
    # @param [Hash] opts the optional parameters
    # @option opts [Pet] :body Pet object that needs to be added to the store
    # @return [nil]
    def self.add_pet(opts = {})
      

      # resource path
      path = "/pet".sub('{format}','json')

      # query parameters
      query_params = {}

      # header parameters
      header_params = {}

      # HTTP header 'Accept' (if needed)
      _header_accept = ['application/json', 'application/xml']
      _header_accept_result = Swagger::Request.select_header_accept(_header_accept) and header_params['Accept'] = _header_accept_result

      # HTTP header 'Content-Type'
      _header_content_type = ['application/json', 'application/xml']
      header_params['Content-Type'] = Swagger::Request.select_header_content_type(_header_content_type)

      # form parameters
      form_params = {}

      # http body (model)
      post_body = Swagger::Request.object_to_http_body(opts[:'body'])
      

      auth_names = ['petstore_auth']
      Swagger::Request.new(:POST, path, {:params => query_params,:headers => header_params, :form_params => form_params, :body => post_body, :auth_names => auth_names}).make
      nil
    end

    # Finds Pets by status
    # Multiple status values can be provided with comma seperated strings
    # @param [Hash] opts the optional parameters
    # @option opts [array[string]] :status Status values that need to be considered for filter
    # @return [array[Pet]]
    def self.find_pets_by_status(opts = {})
      

      # resource path
      path = "/pet/findByStatus".sub('{format}','json')

      # query parameters
      query_params = {}
      query_params[:'status'] = opts[:'status'] if opts[:'status']

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
      

      auth_names = ['petstore_auth']
      response = Swagger::Request.new(:GET, path, {:params => query_params, :headers => header_params, :form_params => form_params, :body => post_body, :auth_names => auth_names}).make.body
      response.map {|response| obj = Pet.new() and obj.build_from_hash(response) }
    end

    # Finds Pets by tags
    # Muliple tags can be provided with comma seperated strings. Use tag1, tag2, tag3 for testing.
    # @param [Hash] opts the optional parameters
    # @option opts [array[string]] :tags Tags to filter by
    # @return [array[Pet]]
    def self.find_pets_by_tags(opts = {})
      

      # resource path
      path = "/pet/findByTags".sub('{format}','json')

      # query parameters
      query_params = {}
      query_params[:'tags'] = opts[:'tags'] if opts[:'tags']

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
      

      auth_names = ['petstore_auth']
      response = Swagger::Request.new(:GET, path, {:params => query_params, :headers => header_params, :form_params => form_params, :body => post_body, :auth_names => auth_names}).make.body
      response.map {|response| obj = Pet.new() and obj.build_from_hash(response) }
    end

    # Find pet by ID
    # Returns a pet when ID &lt; 10.  ID &gt; 10 or nonintegers will simulate API error conditions
    # @param pet_id ID of pet that needs to be fetched
    # @param [Hash] opts the optional parameters
    # @return [Pet]
    def self.get_pet_by_id(pet_id, opts = {})
      
      # verify the required parameter 'pet_id' is set
      raise "Missing the required parameter 'pet_id' when calling get_pet_by_id" if pet_id.nil?
      

      # resource path
      path = "/pet/{petId}".sub('{format}','json').sub('{' + 'petId' + '}', pet_id.to_s)

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
      

      auth_names = ['petstore_auth', 'api_key']
      response = Swagger::Request.new(:GET, path, {:params => query_params, :headers => header_params, :form_params => form_params, :body => post_body, :auth_names => auth_names}).make.body
      obj = Pet.new() and obj.build_from_hash(response)
    end

    # Updates a pet in the store with form data
    # 
    # @param pet_id ID of pet that needs to be updated
    # @param [Hash] opts the optional parameters
    # @option opts [string] :name Updated name of the pet
    # @option opts [string] :status Updated status of the pet
    # @return [nil]
    def self.update_pet_with_form(pet_id, opts = {})
      
      # verify the required parameter 'pet_id' is set
      raise "Missing the required parameter 'pet_id' when calling update_pet_with_form" if pet_id.nil?
      

      # resource path
      path = "/pet/{petId}".sub('{format}','json').sub('{' + 'petId' + '}', pet_id.to_s)

      # query parameters
      query_params = {}

      # header parameters
      header_params = {}

      # HTTP header 'Accept' (if needed)
      _header_accept = ['application/json', 'application/xml']
      _header_accept_result = Swagger::Request.select_header_accept(_header_accept) and header_params['Accept'] = _header_accept_result

      # HTTP header 'Content-Type'
      _header_content_type = ['application/x-www-form-urlencoded']
      header_params['Content-Type'] = Swagger::Request.select_header_content_type(_header_content_type)

      # form parameters
      form_params = {}
      form_params["name"] = opts[:'name'] if opts[:'name']
      form_params["status"] = opts[:'status'] if opts[:'status']

      # http body (model)
      post_body = nil
      

      auth_names = ['petstore_auth']
      Swagger::Request.new(:POST, path, {:params => query_params,:headers => header_params, :form_params => form_params, :body => post_body, :auth_names => auth_names}).make
      nil
    end

    # Deletes a pet
    # 
    # @param pet_id Pet id to delete
    # @param [Hash] opts the optional parameters
    # @option opts [string] :api_key 
    # @return [nil]
    def self.delete_pet(pet_id, opts = {})
      
      # verify the required parameter 'pet_id' is set
      raise "Missing the required parameter 'pet_id' when calling delete_pet" if pet_id.nil?
      

      # resource path
      path = "/pet/{petId}".sub('{format}','json').sub('{' + 'petId' + '}', pet_id.to_s)

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
      header_params[:'api_key'] = opts[:'api_key'] if opts[:'api_key']

      # form parameters
      form_params = {}

      # http body (model)
      post_body = nil
      

      auth_names = ['petstore_auth']
      Swagger::Request.new(:DELETE, path, {:params => query_params,:headers => header_params, :form_params => form_params, :body => post_body, :auth_names => auth_names}).make
      nil
    end

    # uploads an image
    # 
    # @param pet_id ID of pet to update
    # @param [Hash] opts the optional parameters
    # @option opts [string] :additional_metadata Additional data to pass to server
    # @option opts [file] :file file to upload
    # @return [nil]
    def self.upload_file(pet_id, opts = {})
      
      # verify the required parameter 'pet_id' is set
      raise "Missing the required parameter 'pet_id' when calling upload_file" if pet_id.nil?
      

      # resource path
      path = "/pet/{petId}/uploadImage".sub('{format}','json').sub('{' + 'petId' + '}', pet_id.to_s)

      # query parameters
      query_params = {}

      # header parameters
      header_params = {}

      # HTTP header 'Accept' (if needed)
      _header_accept = ['application/json', 'application/xml']
      _header_accept_result = Swagger::Request.select_header_accept(_header_accept) and header_params['Accept'] = _header_accept_result

      # HTTP header 'Content-Type'
      _header_content_type = ['multipart/form-data']
      header_params['Content-Type'] = Swagger::Request.select_header_content_type(_header_content_type)

      # form parameters
      form_params = {}
      form_params["additionalMetadata"] = opts[:'additional_metadata'] if opts[:'additional_metadata']
      form_params["file"] = opts[:'file'] if opts[:'file']

      # http body (model)
      post_body = nil
      

      auth_names = ['petstore_auth']
      Swagger::Request.new(:POST, path, {:params => query_params,:headers => header_params, :form_params => form_params, :body => post_body, :auth_names => auth_names}).make
      nil
    end
  end
end

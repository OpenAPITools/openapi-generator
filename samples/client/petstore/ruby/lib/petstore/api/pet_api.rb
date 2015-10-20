require "uri"

module Petstore
  class PetApi
    attr_accessor :api_client

    def initialize(api_client = nil)
      @api_client = api_client || Configuration.api_client
    end

    # Update an existing pet
    # 
    # @param [Hash] opts the optional parameters
    # @option opts [Pet] :body Pet object that needs to be added to the store
    # @return [nil]
    def update_pet(opts = {})
      if Configuration.debugging
        Configuration.logger.debug "Calling API: PetApi#update_pet ..."
      end
      
      # resource path
      path = "/pet".sub('{format}','json')

      # query parameters
      query_params = {}

      # header parameters
      header_params = {}

      # HTTP header 'Accept' (if needed)
      _header_accept = ['application/json', 'application/xml']
      _header_accept_result = @api_client.select_header_accept(_header_accept) and header_params['Accept'] = _header_accept_result

      # HTTP header 'Content-Type'
      _header_content_type = ['application/json', 'application/xml']
      header_params['Content-Type'] = @api_client.select_header_content_type(_header_content_type)

      # form parameters
      form_params = {}

      # http body (model)
      post_body = @api_client.object_to_http_body(opts[:'body'])
      

      auth_names = ['petstore_auth']
      @api_client.call_api(:PUT, path,
        :header_params => header_params,
        :query_params => query_params,
        :form_params => form_params,
        :body => post_body,
        :auth_names => auth_names)
      if Configuration.debugging
        Configuration.logger.debug "API called: PetApi#update_pet"
      end
      return nil
    end

    # Add a new pet to the store
    # 
    # @param [Hash] opts the optional parameters
    # @option opts [Pet] :body Pet object that needs to be added to the store
    # @return [nil]
    def add_pet(opts = {})
      if Configuration.debugging
        Configuration.logger.debug "Calling API: PetApi#add_pet ..."
      end
      
      # resource path
      path = "/pet".sub('{format}','json')

      # query parameters
      query_params = {}

      # header parameters
      header_params = {}

      # HTTP header 'Accept' (if needed)
      _header_accept = ['application/json', 'application/xml']
      _header_accept_result = @api_client.select_header_accept(_header_accept) and header_params['Accept'] = _header_accept_result

      # HTTP header 'Content-Type'
      _header_content_type = ['application/json', 'application/xml']
      header_params['Content-Type'] = @api_client.select_header_content_type(_header_content_type)

      # form parameters
      form_params = {}

      # http body (model)
      post_body = @api_client.object_to_http_body(opts[:'body'])
      

      auth_names = ['petstore_auth']
      @api_client.call_api(:POST, path,
        :header_params => header_params,
        :query_params => query_params,
        :form_params => form_params,
        :body => post_body,
        :auth_names => auth_names)
      if Configuration.debugging
        Configuration.logger.debug "API called: PetApi#add_pet"
      end
      return nil
    end

    # Finds Pets by status
    # Multiple status values can be provided with comma seperated strings
    # @param [Hash] opts the optional parameters
    # @option opts [Array<String>] :status Status values that need to be considered for filter
    # @return [Array<Pet>]
    def find_pets_by_status(opts = {})
      if Configuration.debugging
        Configuration.logger.debug "Calling API: PetApi#find_pets_by_status ..."
      end
      
      # resource path
      path = "/pet/findByStatus".sub('{format}','json')

      # query parameters
      query_params = {}
      query_params[:'status'] = opts[:'status'] if opts[:'status']

      # header parameters
      header_params = {}

      # HTTP header 'Accept' (if needed)
      _header_accept = ['application/json', 'application/xml']
      _header_accept_result = @api_client.select_header_accept(_header_accept) and header_params['Accept'] = _header_accept_result

      # HTTP header 'Content-Type'
      _header_content_type = []
      header_params['Content-Type'] = @api_client.select_header_content_type(_header_content_type)

      # form parameters
      form_params = {}

      # http body (model)
      post_body = nil
      

      auth_names = ['petstore_auth']
      result = @api_client.call_api(:GET, path,
        :header_params => header_params,
        :query_params => query_params,
        :form_params => form_params,
        :body => post_body,
        :auth_names => auth_names,
        :return_type => 'Array<Pet>')
      if Configuration.debugging
        Configuration.logger.debug "API called: PetApi#find_pets_by_status. Result: #{result.inspect}"
      end
      return result
    end

    # Finds Pets by tags
    # Muliple tags can be provided with comma seperated strings. Use tag1, tag2, tag3 for testing.
    # @param [Hash] opts the optional parameters
    # @option opts [Array<String>] :tags Tags to filter by
    # @return [Array<Pet>]
    def find_pets_by_tags(opts = {})
      if Configuration.debugging
        Configuration.logger.debug "Calling API: PetApi#find_pets_by_tags ..."
      end
      
      # resource path
      path = "/pet/findByTags".sub('{format}','json')

      # query parameters
      query_params = {}
      query_params[:'tags'] = opts[:'tags'] if opts[:'tags']

      # header parameters
      header_params = {}

      # HTTP header 'Accept' (if needed)
      _header_accept = ['application/json', 'application/xml']
      _header_accept_result = @api_client.select_header_accept(_header_accept) and header_params['Accept'] = _header_accept_result

      # HTTP header 'Content-Type'
      _header_content_type = []
      header_params['Content-Type'] = @api_client.select_header_content_type(_header_content_type)

      # form parameters
      form_params = {}

      # http body (model)
      post_body = nil
      

      auth_names = ['petstore_auth']
      result = @api_client.call_api(:GET, path,
        :header_params => header_params,
        :query_params => query_params,
        :form_params => form_params,
        :body => post_body,
        :auth_names => auth_names,
        :return_type => 'Array<Pet>')
      if Configuration.debugging
        Configuration.logger.debug "API called: PetApi#find_pets_by_tags. Result: #{result.inspect}"
      end
      return result
    end

    # Find pet by ID
    # Returns a pet when ID &lt; 10.  ID &gt; 10 or nonintegers will simulate API error conditions
    # @param pet_id ID of pet that needs to be fetched
    # @param [Hash] opts the optional parameters
    # @return [Pet]
    def get_pet_by_id(pet_id, opts = {})
      if Configuration.debugging
        Configuration.logger.debug "Calling API: PetApi#get_pet_by_id ..."
      end
      
      # verify the required parameter 'pet_id' is set
      fail "Missing the required parameter 'pet_id' when calling get_pet_by_id" if pet_id.nil?
      
      # resource path
      path = "/pet/{petId}".sub('{format}','json').sub('{' + 'petId' + '}', pet_id.to_s)

      # query parameters
      query_params = {}

      # header parameters
      header_params = {}

      # HTTP header 'Accept' (if needed)
      _header_accept = ['application/json', 'application/xml']
      _header_accept_result = @api_client.select_header_accept(_header_accept) and header_params['Accept'] = _header_accept_result

      # HTTP header 'Content-Type'
      _header_content_type = []
      header_params['Content-Type'] = @api_client.select_header_content_type(_header_content_type)

      # form parameters
      form_params = {}

      # http body (model)
      post_body = nil
      

      auth_names = ['api_key', 'petstore_auth']
      result = @api_client.call_api(:GET, path,
        :header_params => header_params,
        :query_params => query_params,
        :form_params => form_params,
        :body => post_body,
        :auth_names => auth_names,
        :return_type => 'Pet')
      if Configuration.debugging
        Configuration.logger.debug "API called: PetApi#get_pet_by_id. Result: #{result.inspect}"
      end
      return result
    end

    # Updates a pet in the store with form data
    # 
    # @param pet_id ID of pet that needs to be updated
    # @param [Hash] opts the optional parameters
    # @option opts [String] :name Updated name of the pet
    # @option opts [String] :status Updated status of the pet
    # @return [nil]
    def update_pet_with_form(pet_id, opts = {})
      if Configuration.debugging
        Configuration.logger.debug "Calling API: PetApi#update_pet_with_form ..."
      end
      
      # verify the required parameter 'pet_id' is set
      fail "Missing the required parameter 'pet_id' when calling update_pet_with_form" if pet_id.nil?
      
      # resource path
      path = "/pet/{petId}".sub('{format}','json').sub('{' + 'petId' + '}', pet_id.to_s)

      # query parameters
      query_params = {}

      # header parameters
      header_params = {}

      # HTTP header 'Accept' (if needed)
      _header_accept = ['application/json', 'application/xml']
      _header_accept_result = @api_client.select_header_accept(_header_accept) and header_params['Accept'] = _header_accept_result

      # HTTP header 'Content-Type'
      _header_content_type = ['application/x-www-form-urlencoded']
      header_params['Content-Type'] = @api_client.select_header_content_type(_header_content_type)

      # form parameters
      form_params = {}
      form_params["name"] = opts[:'name'] if opts[:'name']
      form_params["status"] = opts[:'status'] if opts[:'status']

      # http body (model)
      post_body = nil
      

      auth_names = ['petstore_auth']
      @api_client.call_api(:POST, path,
        :header_params => header_params,
        :query_params => query_params,
        :form_params => form_params,
        :body => post_body,
        :auth_names => auth_names)
      if Configuration.debugging
        Configuration.logger.debug "API called: PetApi#update_pet_with_form"
      end
      return nil
    end

    # Deletes a pet
    # 
    # @param pet_id Pet id to delete
    # @param [Hash] opts the optional parameters
    # @option opts [String] :api_key 
    # @return [nil]
    def delete_pet(pet_id, opts = {})
      if Configuration.debugging
        Configuration.logger.debug "Calling API: PetApi#delete_pet ..."
      end
      
      # verify the required parameter 'pet_id' is set
      fail "Missing the required parameter 'pet_id' when calling delete_pet" if pet_id.nil?
      
      # resource path
      path = "/pet/{petId}".sub('{format}','json').sub('{' + 'petId' + '}', pet_id.to_s)

      # query parameters
      query_params = {}

      # header parameters
      header_params = {}

      # HTTP header 'Accept' (if needed)
      _header_accept = ['application/json', 'application/xml']
      _header_accept_result = @api_client.select_header_accept(_header_accept) and header_params['Accept'] = _header_accept_result

      # HTTP header 'Content-Type'
      _header_content_type = []
      header_params['Content-Type'] = @api_client.select_header_content_type(_header_content_type)
      header_params[:'api_key'] = opts[:'api_key'] if opts[:'api_key']

      # form parameters
      form_params = {}

      # http body (model)
      post_body = nil
      

      auth_names = ['petstore_auth']
      @api_client.call_api(:DELETE, path,
        :header_params => header_params,
        :query_params => query_params,
        :form_params => form_params,
        :body => post_body,
        :auth_names => auth_names)
      if Configuration.debugging
        Configuration.logger.debug "API called: PetApi#delete_pet"
      end
      return nil
    end

    # uploads an image
    # 
    # @param pet_id ID of pet to update
    # @param [Hash] opts the optional parameters
    # @option opts [String] :additional_metadata Additional data to pass to server
    # @option opts [File] :file file to upload
    # @return [nil]
    def upload_file(pet_id, opts = {})
      if Configuration.debugging
        Configuration.logger.debug "Calling API: PetApi#upload_file ..."
      end
      
      # verify the required parameter 'pet_id' is set
      fail "Missing the required parameter 'pet_id' when calling upload_file" if pet_id.nil?
      
      # resource path
      path = "/pet/{petId}/uploadImage".sub('{format}','json').sub('{' + 'petId' + '}', pet_id.to_s)

      # query parameters
      query_params = {}

      # header parameters
      header_params = {}

      # HTTP header 'Accept' (if needed)
      _header_accept = ['application/json', 'application/xml']
      _header_accept_result = @api_client.select_header_accept(_header_accept) and header_params['Accept'] = _header_accept_result

      # HTTP header 'Content-Type'
      _header_content_type = ['multipart/form-data']
      header_params['Content-Type'] = @api_client.select_header_content_type(_header_content_type)

      # form parameters
      form_params = {}
      form_params["additionalMetadata"] = opts[:'additional_metadata'] if opts[:'additional_metadata']
      form_params["file"] = opts[:'file'] if opts[:'file']

      # http body (model)
      post_body = nil
      

      auth_names = ['petstore_auth']
      @api_client.call_api(:POST, path,
        :header_params => header_params,
        :query_params => query_params,
        :form_params => form_params,
        :body => post_body,
        :auth_names => auth_names)
      if Configuration.debugging
        Configuration.logger.debug "API called: PetApi#upload_file"
      end
      return nil
    end
  end
end





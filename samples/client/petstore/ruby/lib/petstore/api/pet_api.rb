require "uri"

module Petstore
  class PetApi
    attr_accessor :api_client

    def initialize(api_client = ApiClient.default)
      @api_client = api_client
    end

    # Update an existing pet
    # 
    # @param [Hash] opts the optional parameters
    # @option opts [Pet] :body Pet object that needs to be added to the store
    # @return [nil]
    def update_pet(opts = {})
      update_pet_with_http_info(opts)
      return nil
    end

    # Update an existing pet
    # 
    # @param [Hash] opts the optional parameters
    # @option opts [Pet] :body Pet object that needs to be added to the store
    # @return [Array<(nil, Fixnum, Hash)>] nil, response status code and response headers
    def update_pet_with_http_info(opts = {})
      if @api_client.config.debugging
        @api_client.config.logger.debug "Calling API: PetApi#update_pet ..."
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
      data, status_code, headers = @api_client.call_api(:PUT, path,
        :header_params => header_params,
        :query_params => query_params,
        :form_params => form_params,
        :body => post_body,
        :auth_names => auth_names)
      if @api_client.config.debugging
        @api_client.config.logger.debug "API called: PetApi#update_pet\nData: #{data.inspect}\nStatus code: #{status_code}\nHeaders: #{headers}"
      end
      return data, status_code, headers
    end

    # Add a new pet to the store
    # 
    # @param [Hash] opts the optional parameters
    # @option opts [Pet] :body Pet object that needs to be added to the store
    # @return [nil]
    def add_pet(opts = {})
      add_pet_with_http_info(opts)
      return nil
    end

    # Add a new pet to the store
    # 
    # @param [Hash] opts the optional parameters
    # @option opts [Pet] :body Pet object that needs to be added to the store
    # @return [Array<(nil, Fixnum, Hash)>] nil, response status code and response headers
    def add_pet_with_http_info(opts = {})
      if @api_client.config.debugging
        @api_client.config.logger.debug "Calling API: PetApi#add_pet ..."
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
      data, status_code, headers = @api_client.call_api(:POST, path,
        :header_params => header_params,
        :query_params => query_params,
        :form_params => form_params,
        :body => post_body,
        :auth_names => auth_names)
      if @api_client.config.debugging
        @api_client.config.logger.debug "API called: PetApi#add_pet\nData: #{data.inspect}\nStatus code: #{status_code}\nHeaders: #{headers}"
      end
      return data, status_code, headers
    end

    # Finds Pets by status
    # Multiple status values can be provided with comma seperated strings
    # @param [Hash] opts the optional parameters
    # @option opts [Array<String>] :status Status values that need to be considered for filter
    # @return [Array<Pet>]
    def find_pets_by_status(opts = {})
      data, status_code, headers = find_pets_by_status_with_http_info(opts)
      return data
    end

    # Finds Pets by status
    # Multiple status values can be provided with comma seperated strings
    # @param [Hash] opts the optional parameters
    # @option opts [Array<String>] :status Status values that need to be considered for filter
    # @return [Array<(Array<Pet>, Fixnum, Hash)>] Array<Pet> data, response status code and response headers
    def find_pets_by_status_with_http_info(opts = {})
      if @api_client.config.debugging
        @api_client.config.logger.debug "Calling API: PetApi#find_pets_by_status ..."
      end
      
      # resource path
      path = "/pet/findByStatus".sub('{format}','json')

      # query parameters
      query_params = {}
      query_params[:'status'] = @api_client.build_collection_param(opts[:'status'], :multi) if opts[:'status']

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
      data, status_code, headers = @api_client.call_api(:GET, path,
        :header_params => header_params,
        :query_params => query_params,
        :form_params => form_params,
        :body => post_body,
        :auth_names => auth_names,
        :return_type => 'Array<Pet>')
      if @api_client.config.debugging
        @api_client.config.logger.debug "API called: PetApi#find_pets_by_status\nData: #{data.inspect}\nStatus code: #{status_code}\nHeaders: #{headers}"
      end
      return data, status_code, headers
    end

    # Finds Pets by tags
    # Muliple tags can be provided with comma seperated strings. Use tag1, tag2, tag3 for testing.
    # @param [Hash] opts the optional parameters
    # @option opts [Array<String>] :tags Tags to filter by
    # @return [Array<Pet>]
    def find_pets_by_tags(opts = {})
      data, status_code, headers = find_pets_by_tags_with_http_info(opts)
      return data
    end

    # Finds Pets by tags
    # Muliple tags can be provided with comma seperated strings. Use tag1, tag2, tag3 for testing.
    # @param [Hash] opts the optional parameters
    # @option opts [Array<String>] :tags Tags to filter by
    # @return [Array<(Array<Pet>, Fixnum, Hash)>] Array<Pet> data, response status code and response headers
    def find_pets_by_tags_with_http_info(opts = {})
      if @api_client.config.debugging
        @api_client.config.logger.debug "Calling API: PetApi#find_pets_by_tags ..."
      end
      
      # resource path
      path = "/pet/findByTags".sub('{format}','json')

      # query parameters
      query_params = {}
      query_params[:'tags'] = @api_client.build_collection_param(opts[:'tags'], :multi) if opts[:'tags']

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
      data, status_code, headers = @api_client.call_api(:GET, path,
        :header_params => header_params,
        :query_params => query_params,
        :form_params => form_params,
        :body => post_body,
        :auth_names => auth_names,
        :return_type => 'Array<Pet>')
      if @api_client.config.debugging
        @api_client.config.logger.debug "API called: PetApi#find_pets_by_tags\nData: #{data.inspect}\nStatus code: #{status_code}\nHeaders: #{headers}"
      end
      return data, status_code, headers
    end

    # Find pet by ID
    # Returns a pet when ID &lt; 10.  ID &gt; 10 or nonintegers will simulate API error conditions
    # @param pet_id ID of pet that needs to be fetched
    # @param [Hash] opts the optional parameters
    # @return [Pet]
    def get_pet_by_id(pet_id, opts = {})
      data, status_code, headers = get_pet_by_id_with_http_info(pet_id, opts)
      return data
    end

    # Find pet by ID
    # Returns a pet when ID &lt; 10.  ID &gt; 10 or nonintegers will simulate API error conditions
    # @param pet_id ID of pet that needs to be fetched
    # @param [Hash] opts the optional parameters
    # @return [Array<(Pet, Fixnum, Hash)>] Pet data, response status code and response headers
    def get_pet_by_id_with_http_info(pet_id, opts = {})
      if @api_client.config.debugging
        @api_client.config.logger.debug "Calling API: PetApi#get_pet_by_id ..."
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
      

      auth_names = ['api_key']
      data, status_code, headers = @api_client.call_api(:GET, path,
        :header_params => header_params,
        :query_params => query_params,
        :form_params => form_params,
        :body => post_body,
        :auth_names => auth_names,
        :return_type => 'Pet')
      if @api_client.config.debugging
        @api_client.config.logger.debug "API called: PetApi#get_pet_by_id\nData: #{data.inspect}\nStatus code: #{status_code}\nHeaders: #{headers}"
      end
      return data, status_code, headers
    end

    # Updates a pet in the store with form data
    # 
    # @param pet_id ID of pet that needs to be updated
    # @param [Hash] opts the optional parameters
    # @option opts [String] :name Updated name of the pet
    # @option opts [String] :status Updated status of the pet
    # @return [nil]
    def update_pet_with_form(pet_id, opts = {})
      update_pet_with_form_with_http_info(pet_id, opts)
      return nil
    end

    # Updates a pet in the store with form data
    # 
    # @param pet_id ID of pet that needs to be updated
    # @param [Hash] opts the optional parameters
    # @option opts [String] :name Updated name of the pet
    # @option opts [String] :status Updated status of the pet
    # @return [Array<(nil, Fixnum, Hash)>] nil, response status code and response headers
    def update_pet_with_form_with_http_info(pet_id, opts = {})
      if @api_client.config.debugging
        @api_client.config.logger.debug "Calling API: PetApi#update_pet_with_form ..."
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
      data, status_code, headers = @api_client.call_api(:POST, path,
        :header_params => header_params,
        :query_params => query_params,
        :form_params => form_params,
        :body => post_body,
        :auth_names => auth_names)
      if @api_client.config.debugging
        @api_client.config.logger.debug "API called: PetApi#update_pet_with_form\nData: #{data.inspect}\nStatus code: #{status_code}\nHeaders: #{headers}"
      end
      return data, status_code, headers
    end

    # Deletes a pet
    # 
    # @param pet_id Pet id to delete
    # @param [Hash] opts the optional parameters
    # @option opts [String] :api_key 
    # @return [nil]
    def delete_pet(pet_id, opts = {})
      delete_pet_with_http_info(pet_id, opts)
      return nil
    end

    # Deletes a pet
    # 
    # @param pet_id Pet id to delete
    # @param [Hash] opts the optional parameters
    # @option opts [String] :api_key 
    # @return [Array<(nil, Fixnum, Hash)>] nil, response status code and response headers
    def delete_pet_with_http_info(pet_id, opts = {})
      if @api_client.config.debugging
        @api_client.config.logger.debug "Calling API: PetApi#delete_pet ..."
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
      data, status_code, headers = @api_client.call_api(:DELETE, path,
        :header_params => header_params,
        :query_params => query_params,
        :form_params => form_params,
        :body => post_body,
        :auth_names => auth_names)
      if @api_client.config.debugging
        @api_client.config.logger.debug "API called: PetApi#delete_pet\nData: #{data.inspect}\nStatus code: #{status_code}\nHeaders: #{headers}"
      end
      return data, status_code, headers
    end

    # uploads an image
    # 
    # @param pet_id ID of pet to update
    # @param [Hash] opts the optional parameters
    # @option opts [String] :additional_metadata Additional data to pass to server
    # @option opts [File] :file file to upload
    # @return [nil]
    def upload_file(pet_id, opts = {})
      upload_file_with_http_info(pet_id, opts)
      return nil
    end

    # uploads an image
    # 
    # @param pet_id ID of pet to update
    # @param [Hash] opts the optional parameters
    # @option opts [String] :additional_metadata Additional data to pass to server
    # @option opts [File] :file file to upload
    # @return [Array<(nil, Fixnum, Hash)>] nil, response status code and response headers
    def upload_file_with_http_info(pet_id, opts = {})
      if @api_client.config.debugging
        @api_client.config.logger.debug "Calling API: PetApi#upload_file ..."
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
      data, status_code, headers = @api_client.call_api(:POST, path,
        :header_params => header_params,
        :query_params => query_params,
        :form_params => form_params,
        :body => post_body,
        :auth_names => auth_names)
      if @api_client.config.debugging
        @api_client.config.logger.debug "API called: PetApi#upload_file\nData: #{data.inspect}\nStatus code: #{status_code}\nHeaders: #{headers}"
      end
      return data, status_code, headers
    end
  end
end





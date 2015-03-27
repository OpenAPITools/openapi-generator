require "uri"

class PetApi
  basePath = "http://petstore.swagger.io/v2"
  # apiInvoker = APIInvoker

  def self.escapeString(string)
    URI.encode(string.to_s)
  end


  # Update an existing pet
  # 
  # @param body Pet object that needs to be added to the store
  # @return void
  def self.updatePet (body, opts={})
    query_param_keys = []

    
    
    # set default values and merge with input
    options = {
      :'body' => body
      
    }.merge(opts)

    #resource path
    path = "/pet".sub('{format}','json')
    
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

  # Add a new pet to the store
  # 
  # @param body Pet object that needs to be added to the store
  # @return void
  def self.addPet (body, opts={})
    query_param_keys = []

    
    
    # set default values and merge with input
    options = {
      :'body' => body
      
    }.merge(opts)

    #resource path
    path = "/pet".sub('{format}','json')
    
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

  # Finds Pets by status
  # Multiple status values can be provided with comma seperated strings
  # @param status Status values that need to be considered for filter
  # @return array[Pet]
  def self.findPetsByStatus (status, opts={})
    query_param_keys = [:status]

    
    
    # set default values and merge with input
    options = {
      :'status' => status
      
    }.merge(opts)

    #resource path
    path = "/pet/findByStatus".sub('{format}','json')
    
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
    
    response.map {|response| Pet.new(response) }
    
    
  
  end

  # Finds Pets by tags
  # Muliple tags can be provided with comma seperated strings. Use tag1, tag2, tag3 for testing.
  # @param tags Tags to filter by
  # @return array[Pet]
  def self.findPetsByTags (tags, opts={})
    query_param_keys = [:tags]

    
    
    # set default values and merge with input
    options = {
      :'tags' => tags
      
    }.merge(opts)

    #resource path
    path = "/pet/findByTags".sub('{format}','json')
    
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
    
    response.map {|response| Pet.new(response) }
    
    
  
  end

  # Find pet by ID
  # Returns a pet when ID &lt; 10.  ID &gt; 10 or nonintegers will simulate API error conditions
  # @param pet_id ID of pet that needs to be fetched
  # @return Pet
  def self.getPetById (pet_id, opts={})
    query_param_keys = []

    
    
    # set default values and merge with input
    options = {
      :'pet_id' => pet_id
      
    }.merge(opts)

    #resource path
    path = "/pet/{petId}".sub('{format}','json').sub('{' + 'petId' + '}', escapeString(pet_id))
    
    
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
     Pet.new(response)
    
    
  
  end

  # Updates a pet in the store with form data
  # 
  # @param pet_id ID of pet that needs to be updated
  # @param name Updated name of the pet
  # @param status Updated status of the pet
  # @return void
  def self.updatePetWithForm (pet_id, name, status, opts={})
    query_param_keys = []

    
    
    # set default values and merge with input
    options = {
      :'pet_id' => pet_id,
      :'name' => name,
      :'status' => status
      
    }.merge(opts)

    #resource path
    path = "/pet/{petId}".sub('{format}','json').sub('{' + 'petId' + '}', escapeString(pet_id))
    
    
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
    
    form_parameter_hash["name"] = name
    form_parameter_hash["status"] = status
    
    
    Swagger::Request.new(:POST, path, {:params=>queryopts,:headers=>headers, :body=>post_body, :form_params => form_parameter_hash }).make
    
  
  end

  # Deletes a pet
  # 
  # @param api_key 
  # @param pet_id Pet id to delete
  # @return void
  def self.deletePet (api_key, pet_id, opts={})
    query_param_keys = []

    
    
    # set default values and merge with input
    options = {
      :'api_key' => api_key,
      :'pet_id' => pet_id
      
    }.merge(opts)

    #resource path
    path = "/pet/{petId}".sub('{format}','json').sub('{' + 'petId' + '}', escapeString(pet_id))
    
    
    # pull querystring keys from options
    queryopts = options.select do |key,value|
      query_param_keys.include? key
    end

    # header parameters, if any
    headers = {}
    
    headers[:'api_key'] = api_key
    # http body (model)
    post_body = nil
    
    # form parameters
    form_parameter_hash = {}
    
    
    
    Swagger::Request.new(:DELETE, path, {:params=>queryopts,:headers=>headers, :body=>post_body, :form_params => form_parameter_hash }).make
    
  
  end

  # uploads an image
  # 
  # @param pet_id ID of pet to update
  # @param additional_metadata Additional data to pass to server
  # @param file file to upload
  # @return void
  def self.uploadFile (pet_id, additional_metadata, file, opts={})
    query_param_keys = []

    
    
    # set default values and merge with input
    options = {
      :'pet_id' => pet_id,
      :'additional_metadata' => additional_metadata,
      :'file' => file
      
    }.merge(opts)

    #resource path
    path = "/pet/{petId}/uploadImage".sub('{format}','json').sub('{' + 'petId' + '}', escapeString(pet_id))
    
    
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
    
    form_parameter_hash["additionalMetadata"] = additional_metadata
    form_parameter_hash["file"] = file
    
    
    Swagger::Request.new(:POST, path, {:params=>queryopts,:headers=>headers, :body=>post_body, :form_params => form_parameter_hash }).make
    
  
  end
end

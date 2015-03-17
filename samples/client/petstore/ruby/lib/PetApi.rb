require "uri"

class PetApi
  basePath = "http://petstore.swagger.io/v2"
  # apiInvoker = APIInvoker

  def self.escapeString(string)
    URI.encode(string.to_s)
  end

  
  def self.updatePet (body, opts={})
    query_param_keys = []

    
    
    # set default values and merge with input
    options = {
    
      :body => body
    
    }.merge(opts)

    #resource path
    path = "/pet".sub('{format}','json')
    
    # pull querystring keys from options
    queryopts = options.select do |key,value|
      query_param_keys.include? key
    end

    
    headers = nil
    

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


  def self.addPet (body, opts={})
    query_param_keys = []

    
    
    # set default values and merge with input
    options = {
    
      :body => body
    
    }.merge(opts)

    #resource path
    path = "/pet".sub('{format}','json')
    
    # pull querystring keys from options
    queryopts = options.select do |key,value|
      query_param_keys.include? key
    end

    
    headers = nil
    

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


  def self.findPetsByStatus (status, opts={})
    query_param_keys = [:status]

    
    
    # set default values and merge with input
    options = {
    
      :status => status
    
    }.merge(opts)

    #resource path
    path = "/pet/findByStatus".sub('{format}','json')
    
    # pull querystring keys from options
    queryopts = options.select do |key,value|
      query_param_keys.include? key
    end

    
    headers = nil
    

    # http body (model)
    post_body = nil
    

    # form parameters
    form_parameter_hash = {}
    

    
    response = Swagger::Request.new(:GET, path, {:params=>queryopts,:headers=>headers, :body=>post_body, :form_params => form_parameter_hash }).make.body
    
    response.map {|response| Pet.new(response) }
    
    
  
  end


  def self.findPetsByTags (tags, opts={})
    query_param_keys = [:tags]

    
    
    # set default values and merge with input
    options = {
    
      :tags => tags
    
    }.merge(opts)

    #resource path
    path = "/pet/findByTags".sub('{format}','json')
    
    # pull querystring keys from options
    queryopts = options.select do |key,value|
      query_param_keys.include? key
    end

    
    headers = nil
    

    # http body (model)
    post_body = nil
    

    # form parameters
    form_parameter_hash = {}
    

    
    response = Swagger::Request.new(:GET, path, {:params=>queryopts,:headers=>headers, :body=>post_body, :form_params => form_parameter_hash }).make.body
    
    response.map {|response| Pet.new(response) }
    
    
  
  end


  def self.getPetById (petId, opts={})
    query_param_keys = []

    
    
    # set default values and merge with input
    options = {
    
      :petId => petId
    
    }.merge(opts)

    #resource path
    path = "/pet/{petId}".sub('{format}','json').sub('{' + 'petId' + '}', escapeString(petId))
    
    
    # pull querystring keys from options
    queryopts = options.select do |key,value|
      query_param_keys.include? key
    end

    
    headers = nil
    

    # http body (model)
    post_body = nil
    

    # form parameters
    form_parameter_hash = {}
    

    
    response = Swagger::Request.new(:GET, path, {:params=>queryopts,:headers=>headers, :body=>post_body, :form_params => form_parameter_hash }).make.body
     Pet.new(response)
    
    
  
  end


  def self.updatePetWithForm (petId,name,status, opts={})
    query_param_keys = []

    
    
    # set default values and merge with input
    options = {
    
      :petId => petId,
      
    
      :name => name,
      
    
      :status => status
    
    }.merge(opts)

    #resource path
    path = "/pet/{petId}".sub('{format}','json').sub('{' + 'petId' + '}', escapeString(petId))
    
    
    # pull querystring keys from options
    queryopts = options.select do |key,value|
      query_param_keys.include? key
    end

    
    headers = nil
    

    # http body (model)
    post_body = nil
    

    # form parameters
    form_parameter_hash = {}
    
    form_parameter_hash["name"] = name
    form_parameter_hash["status"] = status

    
    
    Swagger::Request.new(:POST, path, {:params=>queryopts,:headers=>headers, :body=>post_body, :form_params => form_parameter_hash }).make
    
  
  end


  def self.deletePet (api_key,petId, opts={})
    query_param_keys = []

    
    
    # set default values and merge with input
    options = {
    
      :api_key => api_key,
      
    
      :petId => petId
    
    }.merge(opts)

    #resource path
    path = "/pet/{petId}".sub('{format}','json').sub('{' + 'petId' + '}', escapeString(petId))
    
    
    # pull querystring keys from options
    queryopts = options.select do |key,value|
      query_param_keys.include? key
    end

    headers = {
      api_key: api_key,
    }
    
    

    # http body (model)
    post_body = nil
    

    # form parameters
    form_parameter_hash = {}
    

    
    
    Swagger::Request.new(:DELETE, path, {:params=>queryopts,:headers=>headers, :body=>post_body, :form_params => form_parameter_hash }).make
    
  
  end


  def self.uploadFile (petId,additionalMetadata,file, opts={})
    query_param_keys = []

    
    
    # set default values and merge with input
    options = {
    
      :petId => petId,
      
    
      :additionalMetadata => additionalMetadata,
      
    
      :file => file
    
    }.merge(opts)

    #resource path
    path = "/pet/{petId}/uploadImage".sub('{format}','json').sub('{' + 'petId' + '}', escapeString(petId))
    
    
    # pull querystring keys from options
    queryopts = options.select do |key,value|
      query_param_keys.include? key
    end

    
    headers = nil
    

    # http body (model)
    post_body = nil
    

    # form parameters
    form_parameter_hash = {}
    
    form_parameter_hash["additionalMetadata"] = additionalMetadata
    form_parameter_hash["file"] = file

    
    
    Swagger::Request.new(:POST, path, {:params=>queryopts,:headers=>headers, :body=>post_body, :form_params => form_parameter_hash }).make
    
  
  end

end

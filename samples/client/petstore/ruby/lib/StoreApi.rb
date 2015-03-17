require "uri"

class StoreApi
  basePath = "http://petstore.swagger.io/v2"
  # apiInvoker = APIInvoker

  def self.escapeString(string)
    URI.encode(string.to_s)
  end

  
  def self.getInventory ( opts={})
    query_param_keys = []

    
    
    # set default values and merge with input
    options = {
    
    }.merge(opts)

    #resource path
    path = "/store/inventory".sub('{format}','json')
    
    # pull querystring keys from options
    queryopts = options.select do |key,value|
      query_param_keys.include? key
    end
    
    
    headers = nil
    
    
    post_body = nil
    

    
    response = Swagger::Request.new(:GET, path, {:params=>queryopts,:headers=>headers, :body=>post_body }).make.body
    
    response.map {|response| map.new(response) }
    
    
  
  end


  def self.placeOrder (body, opts={})
    query_param_keys = []

    
    
    # set default values and merge with input
    options = {
    
      :body => body
    
    }.merge(opts)

    #resource path
    path = "/store/order".sub('{format}','json')
    
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
    

    
    response = Swagger::Request.new(:POST, path, {:params=>queryopts,:headers=>headers, :body=>post_body }).make.body
     Order.new(response)
    
    
  
  end


  def self.getOrderById (orderId, opts={})
    query_param_keys = []

    
    
    # set default values and merge with input
    options = {
    
      :orderId => orderId
    
    }.merge(opts)

    #resource path
    path = "/store/order/{orderId}".sub('{format}','json').sub('{' + 'orderId' + '}', escapeString(orderId))
    
    
    # pull querystring keys from options
    queryopts = options.select do |key,value|
      query_param_keys.include? key
    end
    
    
    headers = nil
    
    
    post_body = nil
    

    
    response = Swagger::Request.new(:GET, path, {:params=>queryopts,:headers=>headers, :body=>post_body }).make.body
     Order.new(response)
    
    
  
  end


  def self.deleteOrder (orderId, opts={})
    query_param_keys = []

    
    
    # set default values and merge with input
    options = {
    
      :orderId => orderId
    
    }.merge(opts)

    #resource path
    path = "/store/order/{orderId}".sub('{format}','json').sub('{' + 'orderId' + '}', escapeString(orderId))
    
    
    # pull querystring keys from options
    queryopts = options.select do |key,value|
      query_param_keys.include? key
    end
    
    
    headers = nil
    
    
    post_body = nil
    

    
    
    Swagger::Request.new(:DELETE, path, {:params=>queryopts,:headers=>headers, :body=>post_body}).make
    
  
  end

end

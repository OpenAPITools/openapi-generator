require "uri"

class StoreApi
  basePath = "http://petstore.swagger.io/v2"
  # apiInvoker = APIInvoker

  def self.escapeString(string)
    URI.encode(string.to_s)
  end


  # Returns pet inventories by status
  # Returns a map of status codes to quantities
  # @return map[string,int]
  def self.getInventory (opts={})
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

    # header parameters, if any
    headers = {}
    
    
    # http body (model)
    post_body = nil
    
    # form parameters
    form_parameter_hash = {}
    
    
    response = Swagger::Request.new(:GET, path, {:params=>queryopts,:headers=>headers, :body=>post_body, :form_params => form_parameter_hash }).make.body
    
    response.map {|response| map.new(response) }
    
    
  
  end

  # Place an order for a pet
  # 
  # @param body order placed for purchasing the pet
  # @return Order
  def self.placeOrder (body, opts={})
    query_param_keys = []

    
    
    # set default values and merge with input
    options = {
      :'body' => body
      
    }.merge(opts)

    #resource path
    path = "/store/order".sub('{format}','json')
    
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
    
    
    response = Swagger::Request.new(:POST, path, {:params=>queryopts,:headers=>headers, :body=>post_body, :form_params => form_parameter_hash }).make.body
     Order.new(response)
    
    
  
  end

  # Find purchase order by ID
  # For valid response try integer IDs with value &lt;= 5 or &gt; 10. Other values will generated exceptions
  # @param order_id ID of pet that needs to be fetched
  # @return Order
  def self.getOrderById (order_id, opts={})
    query_param_keys = []

    
    
    # set default values and merge with input
    options = {
      :'order_id' => order_id
      
    }.merge(opts)

    #resource path
    path = "/store/order/{orderId}".sub('{format}','json').sub('{' + 'orderId' + '}', escapeString(order_id))
    
    
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
     Order.new(response)
    
    
  
  end

  # Delete purchase order by ID
  # For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
  # @param order_id ID of the order that needs to be deleted
  # @return void
  def self.deleteOrder (order_id, opts={})
    query_param_keys = []

    
    
    # set default values and merge with input
    options = {
      :'order_id' => order_id
      
    }.merge(opts)

    #resource path
    path = "/store/order/{orderId}".sub('{format}','json').sub('{' + 'orderId' + '}', escapeString(order_id))
    
    
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

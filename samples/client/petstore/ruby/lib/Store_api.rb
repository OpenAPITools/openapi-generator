require "uri"

class Store_api
  basePath = "http://petstore.swagger.wordnik.com/api"
  # apiInvoker = APIInvoker

  def self.escapeString(string)
    URI.encode(string.to_s)
  end

  def self.get_order_by_id (order_id,opts={})
    query_param_keys = []

    # verify existence of params
    raise "order_id is required" if order_id.nil?
    # set default values and merge with input
    options = { :order_id => order_id}.merge(opts)

    #resource path
    path = "/store.{format}/order/{orderId}".sub('{format}','json').sub('{' + 'orderId' + '}', escapeString(order_id))
    
    
    # pull querystring keys from options
    queryopts = options.select do |key,value|
      query_param_keys.include? key
    end
    
    headers = nil
    post_body = nil
    response = Swagger::Request.new(:GET, path, {:params=>queryopts,:headers=>headers, :body=>post_body }).make.body
    Order.new(response)
  end

def self.delete_order (order_id,opts={})
    query_param_keys = []

    # verify existence of params
    raise "order_id is required" if order_id.nil?
    # set default values and merge with input
    options = { :order_id => order_id}.merge(opts)

    #resource path
    path = "/store.{format}/order/{orderId}".sub('{format}','json').sub('{' + 'orderId' + '}', escapeString(order_id))
    
    
    # pull querystring keys from options
    queryopts = options.select do |key,value|
      query_param_keys.include? key
    end
    
    headers = nil
    post_body = nil
    Swagger::Request.new(:DELETE, path, {:params=>queryopts,:headers=>headers, :body=>post_body}).make
    
  end

def self.place_order (body,opts={})
    query_param_keys = []

    # verify existence of params
    raise "body is required" if body.nil?
    # set default values and merge with input
    options = { :body => body}.merge(opts)

    #resource path
    path = "/store.{format}/order".sub('{format}','json')
    
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
    Swagger::Request.new(:POST, path, {:params=>queryopts,:headers=>headers, :body=>post_body}).make
    
  end

end


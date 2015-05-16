require "uri"

class StoreApi
  basePath = "http://petstore.swagger.io/v2"
  # apiInvoker = APIInvoker

  # Returns pet inventories by status
  # Returns a map of status codes to quantities
  # @param [Hash] opts the optional parameters
  # @return map[string,int]
  def self.get_inventory(opts = {})
    

    # resource path
    path = "/store/inventory".sub('{format}','json')

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

    response = Swagger::Request.new(:GET, path, {:params => query_params, :headers => header_params, :form_params => form_params, :body => post_body}).make.body
    response.map {|response| map.new(response) }
  end

  # Place an order for a pet
  # 
  # @param [Hash] opts the optional parameters
  # @option opts [Order] :body order placed for purchasing the pet
  # @return Order
  def self.place_order(opts = {})
    

    # resource path
    path = "/store/order".sub('{format}','json')

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

    response = Swagger::Request.new(:POST, path, {:params => query_params, :headers => header_params, :form_params => form_params, :body => post_body}).make.body
    Order.new(response)
  end

  # Find purchase order by ID
  # For valid response try integer IDs with value &lt;= 5 or &gt; 10. Other values will generated exceptions
  # @param order_id ID of pet that needs to be fetched
  # @param [Hash] opts the optional parameters
  # @return Order
  def self.get_order_by_id(order_id, opts = {})
    
    # verify the required parameter 'order_id' is set
    raise "Missing the required parameter 'order_id' when calling get_order_by_id" if order_id.nil?
    

    # resource path
    path = "/store/order/{orderId}".sub('{format}','json').sub('{' + 'orderId' + '}', order_id.to_s)

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

    response = Swagger::Request.new(:GET, path, {:params => query_params, :headers => header_params, :form_params => form_params, :body => post_body}).make.body
    Order.new(response)
  end

  # Delete purchase order by ID
  # For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
  # @param order_id ID of the order that needs to be deleted
  # @param [Hash] opts the optional parameters
  # @return void
  def self.delete_order(order_id, opts = {})
    
    # verify the required parameter 'order_id' is set
    raise "Missing the required parameter 'order_id' when calling delete_order" if order_id.nil?
    

    # resource path
    path = "/store/order/{orderId}".sub('{format}','json').sub('{' + 'orderId' + '}', order_id.to_s)

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

    Swagger::Request.new(:DELETE, path, {:params => query_params,:headers => header_params, :form_params => form_params, :body => post_body}).make
  end
end

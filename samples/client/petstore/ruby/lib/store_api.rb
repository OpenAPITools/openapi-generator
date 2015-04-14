require "uri"

class StoreApi
  basePath = "http://petstore.swagger.io/v2"
  # apiInvoker = APIInvoker

  # Returns pet inventories by status
  # Returns a map of status codes to quantities
  # @param [Hash] opts the optional parameters
  # @return map[string,int]
  def self.getInventory(opts = {})
    # verify existence of params

    # resource path
    path = "/store/inventory".sub('{format}','json')

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
    response.map {|response| map.new(response) }
  end

  # Place an order for a pet
  # 
  # @param [Hash] opts the optional parameters
  # @option opts [Order] :body order placed for purchasing the pet
  # @return Order
  def self.placeOrder(opts = {})
    # verify existence of params

    # resource path
    path = "/store/order".sub('{format}','json')

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

    response = Swagger::Request.new(:POST, path, {:params => query_params, :headers => header_params, :form_params => form_params, :body => post_body}).make.body
    Order.new(response)
  end

  # Find purchase order by ID
  # For valid response try integer IDs with value &lt;= 5 or &gt; 10. Other values will generated exceptions
  # @param order_id ID of pet that needs to be fetched
  # @param [Hash] opts the optional parameters
  # @return Order
  def self.getOrderById(order_id, opts = {})
    # verify existence of params
    raise "order_id is required" if order_id.nil?

    # resource path
    path = "/store/order/{orderId}".sub('{format}','json').sub('{' + 'orderId' + '}', order_id.to_s)

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
    Order.new(response)
  end

  # Delete purchase order by ID
  # For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
  # @param order_id ID of the order that needs to be deleted
  # @param [Hash] opts the optional parameters
  # @return void
  def self.deleteOrder(order_id, opts = {})
    # verify existence of params
    raise "order_id is required" if order_id.nil?

    # resource path
    path = "/store/order/{orderId}".sub('{format}','json').sub('{' + 'orderId' + '}', order_id.to_s)

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

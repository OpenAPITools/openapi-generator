require "uri"

module SwaggerClient
  class StoreApi
    basePath = "http://petstore.swagger.io/v2"
    # apiInvoker = APIInvoker

    # Returns pet inventories by status
    # Returns a map of status codes to quantities
    # @param [Hash] opts the optional parameters
    # @return [Hash<String, Integer>]
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
      

      auth_names = ['api_key']
      response = Swagger::Request.new(:GET, path, {:params => query_params, :headers => header_params, :form_params => form_params, :body => post_body, :auth_names => auth_names}).make
      response.deserialize('Hash<String, Integer>')
    end

    # Place an order for a pet
    # 
    # @param [Hash] opts the optional parameters
    # @option opts [Order] :body order placed for purchasing the pet
    # @return [Order]
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
      post_body = Swagger::Request.object_to_http_body(opts[:'body'])
      

      auth_names = []
      response = Swagger::Request.new(:POST, path, {:params => query_params, :headers => header_params, :form_params => form_params, :body => post_body, :auth_names => auth_names}).make
      response.deserialize('Order')
    end

    # Find purchase order by ID
    # For valid response try integer IDs with value &lt;= 5 or &gt; 10. Other values will generated exceptions
    # @param order_id ID of pet that needs to be fetched
    # @param [Hash] opts the optional parameters
    # @return [Order]
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
      

      auth_names = []
      response = Swagger::Request.new(:GET, path, {:params => query_params, :headers => header_params, :form_params => form_params, :body => post_body, :auth_names => auth_names}).make
      response.deserialize('Order')
    end

    # Delete purchase order by ID
    # For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
    # @param order_id ID of the order that needs to be deleted
    # @param [Hash] opts the optional parameters
    # @return [nil]
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
      

      auth_names = []
      Swagger::Request.new(:DELETE, path, {:params => query_params,:headers => header_params, :form_params => form_params, :body => post_body, :auth_names => auth_names}).make
      nil
    end
  end
end

require "uri"

module Petstore
  class StoreApi
    attr_accessor :api_client

    def initialize(api_client = nil)
      @api_client = api_client || Configuration.api_client
    end

    # Returns pet inventories by status
    # Returns a map of status codes to quantities
    # @param [Hash] opts the optional parameters
    # @return [Hash<String, Integer>]
    def get_inventory(opts = {})
      status_code, headers, data = get_inventory_with_http_info(opts)
      return data
    end

    # Returns pet inventories by status
    # Returns a map of status codes to quantities
    # @param [Hash] opts the optional parameters
    # @return [Array<Fixnum, Hash, Hash<String, Integer>>] response status code, response headers and Hash<String, Integer> data
    def get_inventory_with_http_info(opts = {})
      if Configuration.debugging
        Configuration.logger.debug "Calling API: StoreApi#get_inventory ..."
      end
      
      # resource path
      path = "/store/inventory".sub('{format}','json')

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
      status_code, headers, data = @api_client.call_api(:GET, path,
        :header_params => header_params,
        :query_params => query_params,
        :form_params => form_params,
        :body => post_body,
        :auth_names => auth_names,
        :return_type => 'Hash<String, Integer>')
      if Configuration.debugging
        Configuration.logger.debug "API called: StoreApi#get_inventory\nStatus code: #{status_code}\nHeaders: #{headers}\nData: #{data.inspect}"
      end
      return status_code, headers, data
    end

    # Place an order for a pet
    # 
    # @param [Hash] opts the optional parameters
    # @option opts [Order] :body order placed for purchasing the pet
    # @return [Order]
    def place_order(opts = {})
      status_code, headers, data = place_order_with_http_info(opts)
      return data
    end

    # Place an order for a pet
    # 
    # @param [Hash] opts the optional parameters
    # @option opts [Order] :body order placed for purchasing the pet
    # @return [Array<Fixnum, Hash, Order>] response status code, response headers and Order data
    def place_order_with_http_info(opts = {})
      if Configuration.debugging
        Configuration.logger.debug "Calling API: StoreApi#place_order ..."
      end
      
      # resource path
      path = "/store/order".sub('{format}','json')

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
      post_body = @api_client.object_to_http_body(opts[:'body'])
      

      auth_names = []
      status_code, headers, data = @api_client.call_api(:POST, path,
        :header_params => header_params,
        :query_params => query_params,
        :form_params => form_params,
        :body => post_body,
        :auth_names => auth_names,
        :return_type => 'Order')
      if Configuration.debugging
        Configuration.logger.debug "API called: StoreApi#place_order\nStatus code: #{status_code}\nHeaders: #{headers}\nData: #{data.inspect}"
      end
      return status_code, headers, data
    end

    # Find purchase order by ID
    # For valid response try integer IDs with value &lt;= 5 or &gt; 10. Other values will generated exceptions
    # @param order_id ID of pet that needs to be fetched
    # @param [Hash] opts the optional parameters
    # @return [Order]
    def get_order_by_id(order_id, opts = {})
      status_code, headers, data = get_order_by_id_with_http_info(order_id, opts)
      return data
    end

    # Find purchase order by ID
    # For valid response try integer IDs with value &lt;= 5 or &gt; 10. Other values will generated exceptions
    # @param order_id ID of pet that needs to be fetched
    # @param [Hash] opts the optional parameters
    # @return [Array<Fixnum, Hash, Order>] response status code, response headers and Order data
    def get_order_by_id_with_http_info(order_id, opts = {})
      if Configuration.debugging
        Configuration.logger.debug "Calling API: StoreApi#get_order_by_id ..."
      end
      
      # verify the required parameter 'order_id' is set
      fail "Missing the required parameter 'order_id' when calling get_order_by_id" if order_id.nil?
      
      # resource path
      path = "/store/order/{orderId}".sub('{format}','json').sub('{' + 'orderId' + '}', order_id.to_s)

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
      

      auth_names = []
      status_code, headers, data = @api_client.call_api(:GET, path,
        :header_params => header_params,
        :query_params => query_params,
        :form_params => form_params,
        :body => post_body,
        :auth_names => auth_names,
        :return_type => 'Order')
      if Configuration.debugging
        Configuration.logger.debug "API called: StoreApi#get_order_by_id\nStatus code: #{status_code}\nHeaders: #{headers}\nData: #{data.inspect}"
      end
      return status_code, headers, data
    end

    # Delete purchase order by ID
    # For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
    # @param order_id ID of the order that needs to be deleted
    # @param [Hash] opts the optional parameters
    # @return [nil]
    def delete_order(order_id, opts = {})
      delete_order_with_http_info(order_id, opts)
      return nil
    end

    # Delete purchase order by ID
    # For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
    # @param order_id ID of the order that needs to be deleted
    # @param [Hash] opts the optional parameters
    # @return [Array<Fixnum, Hash, nil>] response status code, response headers and nil
    def delete_order_with_http_info(order_id, opts = {})
      if Configuration.debugging
        Configuration.logger.debug "Calling API: StoreApi#delete_order ..."
      end
      
      # verify the required parameter 'order_id' is set
      fail "Missing the required parameter 'order_id' when calling delete_order" if order_id.nil?
      
      # resource path
      path = "/store/order/{orderId}".sub('{format}','json').sub('{' + 'orderId' + '}', order_id.to_s)

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
      

      auth_names = []
      status_code, headers, data = @api_client.call_api(:DELETE, path,
        :header_params => header_params,
        :query_params => query_params,
        :form_params => form_params,
        :body => post_body,
        :auth_names => auth_names)
      if Configuration.debugging
        Configuration.logger.debug "API called: StoreApi#delete_order\nStatus code: #{status_code}\nHeaders: #{headers}\nData: #{data.inspect}"
      end
      return status_code, headers, data
    end
  end
end





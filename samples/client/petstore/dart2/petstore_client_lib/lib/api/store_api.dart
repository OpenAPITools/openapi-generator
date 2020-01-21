part of openapi.api;



class StoreApi {
  final ApiClient apiClient;

  StoreApi([ApiClient apiClient]) : apiClient = apiClient ?? defaultApiClient;

  /// Delete purchase order by ID with HTTP info returned
  ///
  /// For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
  Future deleteOrderWithHttpInfo(String orderId) async {
    Object postBody;

    // verify required params are set
    if(orderId == null) {
     throw ApiException(400, "Missing required param: orderId");
    }

    // create path and map variables
    String path = "/store/order/{orderId}".replaceAll("{format}","json").replaceAll("{" + "orderId" + "}", orderId.toString());

    // query params
    List<QueryParam> queryParams = [];
    Map<String, String> headerParams = {};
    Map<String, String> formParams = {};

    List<String> contentTypes = [];

    String contentType = contentTypes.isNotEmpty ? contentTypes[0] : "application/json";
    List<String> authNames = [];

    if(contentType.startsWith("multipart/form-data")) {
      bool hasFields = false;
      MultipartRequest mp = MultipartRequest(null, null);
      if(hasFields)
        postBody = mp;
    }
    else {
    }

    var response = await apiClient.invokeAPI(path,
                                             'DELETE',
                                             queryParams,
                                             postBody,
                                             headerParams,
                                             formParams,
                                             contentType,
                                             authNames);
    return response;
  }

  /// Delete purchase order by ID
  ///
  /// For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
  Future deleteOrder(String orderId) async {
    Response response = await deleteOrderWithHttpInfo(orderId);
    if(response.statusCode >= 400) {
      throw ApiException(response.statusCode, _decodeBodyBytes(response));
    } else if(response.body != null) {
    } else {
      return;
    }
  }

  /// Returns pet inventories by status with HTTP info returned
  ///
  /// Returns a map of status codes to quantities
  Future<Response> getInventoryWithHttpInfo() async {
    Object postBody;

    // verify required params are set

    // create path and map variables
    String path = "/store/inventory".replaceAll("{format}","json");

    // query params
    List<QueryParam> queryParams = [];
    Map<String, String> headerParams = {};
    Map<String, String> formParams = {};

    List<String> contentTypes = [];

    String contentType = contentTypes.isNotEmpty ? contentTypes[0] : "application/json";
    List<String> authNames = ["api_key"];

    if(contentType.startsWith("multipart/form-data")) {
      bool hasFields = false;
      MultipartRequest mp = MultipartRequest(null, null);
      if(hasFields)
        postBody = mp;
    }
    else {
    }

    var response = await apiClient.invokeAPI(path,
                                             'GET',
                                             queryParams,
                                             postBody,
                                             headerParams,
                                             formParams,
                                             contentType,
                                             authNames);
    return response;
  }

  /// Returns pet inventories by status
  ///
  /// Returns a map of status codes to quantities
  Future<Map<String, int>> getInventory() async {
    Response response = await getInventoryWithHttpInfo();
    if(response.statusCode >= 400) {
      throw ApiException(response.statusCode, _decodeBodyBytes(response));
    } else if(response.body != null) {
      return Map<String, int>.from(apiClient.deserialize(_decodeBodyBytes(response), 'Map<String, int>'));
          ;
    } else {
      return null;
    }
  }

  /// Find purchase order by ID with HTTP info returned
  ///
  /// For valid response try integer IDs with value &lt;&#x3D; 5 or &gt; 10. Other values will generated exceptions
  Future<Response> getOrderByIdWithHttpInfo(int orderId) async {
    Object postBody;

    // verify required params are set
    if(orderId == null) {
     throw ApiException(400, "Missing required param: orderId");
    }

    // create path and map variables
    String path = "/store/order/{orderId}".replaceAll("{format}","json").replaceAll("{" + "orderId" + "}", orderId.toString());

    // query params
    List<QueryParam> queryParams = [];
    Map<String, String> headerParams = {};
    Map<String, String> formParams = {};

    List<String> contentTypes = [];

    String contentType = contentTypes.isNotEmpty ? contentTypes[0] : "application/json";
    List<String> authNames = [];

    if(contentType.startsWith("multipart/form-data")) {
      bool hasFields = false;
      MultipartRequest mp = MultipartRequest(null, null);
      if(hasFields)
        postBody = mp;
    }
    else {
    }

    var response = await apiClient.invokeAPI(path,
                                             'GET',
                                             queryParams,
                                             postBody,
                                             headerParams,
                                             formParams,
                                             contentType,
                                             authNames);
    return response;
  }

  /// Find purchase order by ID
  ///
  /// For valid response try integer IDs with value &lt;&#x3D; 5 or &gt; 10. Other values will generated exceptions
  Future<Order> getOrderById(int orderId) async {
    Response response = await getOrderByIdWithHttpInfo(orderId);
    if(response.statusCode >= 400) {
      throw ApiException(response.statusCode, _decodeBodyBytes(response));
    } else if(response.body != null) {
      return apiClient.deserialize(_decodeBodyBytes(response), 'Order') as Order;
    } else {
      return null;
    }
  }

  /// Place an order for a pet with HTTP info returned
  ///
  /// 
  Future<Response> placeOrderWithHttpInfo(Order body) async {
    Object postBody = body;

    // verify required params are set
    if(body == null) {
     throw ApiException(400, "Missing required param: body");
    }

    // create path and map variables
    String path = "/store/order".replaceAll("{format}","json");

    // query params
    List<QueryParam> queryParams = [];
    Map<String, String> headerParams = {};
    Map<String, String> formParams = {};

    List<String> contentTypes = [];

    String contentType = contentTypes.isNotEmpty ? contentTypes[0] : "application/json";
    List<String> authNames = [];

    if(contentType.startsWith("multipart/form-data")) {
      bool hasFields = false;
      MultipartRequest mp = MultipartRequest(null, null);
      if(hasFields)
        postBody = mp;
    }
    else {
    }

    var response = await apiClient.invokeAPI(path,
                                             'POST',
                                             queryParams,
                                             postBody,
                                             headerParams,
                                             formParams,
                                             contentType,
                                             authNames);
    return response;
  }

  /// Place an order for a pet
  ///
  /// 
  Future<Order> placeOrder(Order body) async {
    Response response = await placeOrderWithHttpInfo(body);
    if(response.statusCode >= 400) {
      throw ApiException(response.statusCode, _decodeBodyBytes(response));
    } else if(response.body != null) {
      return apiClient.deserialize(_decodeBodyBytes(response), 'Order') as Order;
    } else {
      return null;
    }
  }

}

part of swagger.api;



class StoreApi {
  final ApiClient apiClient;

  StoreApi([ApiClient apiClient]) : apiClient = apiClient ?? defaultApiClient;

  /// Delete purchase order by ID
  ///
  /// For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
  Future deleteOrder(String orderId) async {
    Object postBody = null;

    // verify required params are set
    if(orderId == null) {
     throw new ApiException(400, "Missing required param: orderId");
    }

    // create path and map variables
    String path = "/store/order/{orderId}".replaceAll("{format}","json").replaceAll("{" + "orderId" + "}", orderId.toString());

    // query params
    List<QueryParam> queryParams = [];
    Map<String, String> headerParams = {};
    Map<String, String> formParams = {};
    
    List<String> contentTypes = [];

    String contentType = contentTypes.length > 0 ? contentTypes[0] : "application/json";
    List<String> authNames = [];

    if(contentType.startsWith("multipart/form-data")) {
      bool hasFields = false;
      MultipartRequest mp = new MultipartRequest(null, null);
      
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

    if(response.statusCode >= 400) {
      throw new ApiException(response.statusCode, response.body);
    } else if(response.body != null) {
      return ;
    } else {
      return ;
    }
  }
  /// Returns pet inventories by status
  ///
  /// Returns a map of status codes to quantities
  Future<Map<String, int>> getInventory() async {
    Object postBody = null;

    // verify required params are set

    // create path and map variables
    String path = "/store/inventory".replaceAll("{format}","json");

    // query params
    List<QueryParam> queryParams = [];
    Map<String, String> headerParams = {};
    Map<String, String> formParams = {};
    
    List<String> contentTypes = [];

    String contentType = contentTypes.length > 0 ? contentTypes[0] : "application/json";
    List<String> authNames = ["api_key"];

    if(contentType.startsWith("multipart/form-data")) {
      bool hasFields = false;
      MultipartRequest mp = new MultipartRequest(null, null);
      
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

    if(response.statusCode >= 400) {
      throw new ApiException(response.statusCode, response.body);
    } else if(response.body != null) {
      return apiClient.deserialize(response.body, 'Map<String, int>') as Map<String, int> ;
    } else {
      return null;
    }
  }
  /// Find purchase order by ID
  ///
  /// For valid response try integer IDs with value &lt;&#x3D; 5 or &gt; 10. Other values will generated exceptions
  Future<Order> getOrderById(int orderId) async {
    Object postBody = null;

    // verify required params are set
    if(orderId == null) {
     throw new ApiException(400, "Missing required param: orderId");
    }

    // create path and map variables
    String path = "/store/order/{orderId}".replaceAll("{format}","json").replaceAll("{" + "orderId" + "}", orderId.toString());

    // query params
    List<QueryParam> queryParams = [];
    Map<String, String> headerParams = {};
    Map<String, String> formParams = {};
    
    List<String> contentTypes = [];

    String contentType = contentTypes.length > 0 ? contentTypes[0] : "application/json";
    List<String> authNames = [];

    if(contentType.startsWith("multipart/form-data")) {
      bool hasFields = false;
      MultipartRequest mp = new MultipartRequest(null, null);
      
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

    if(response.statusCode >= 400) {
      throw new ApiException(response.statusCode, response.body);
    } else if(response.body != null) {
      return apiClient.deserialize(response.body, 'Order') as Order ;
    } else {
      return null;
    }
  }
  /// Place an order for a pet
  ///
  /// 
  Future<Order> placeOrder(Order body) async {
    Object postBody = body;

    // verify required params are set
    if(body == null) {
     throw new ApiException(400, "Missing required param: body");
    }

    // create path and map variables
    String path = "/store/order".replaceAll("{format}","json");

    // query params
    List<QueryParam> queryParams = [];
    Map<String, String> headerParams = {};
    Map<String, String> formParams = {};
    
    List<String> contentTypes = [];

    String contentType = contentTypes.length > 0 ? contentTypes[0] : "application/json";
    List<String> authNames = [];

    if(contentType.startsWith("multipart/form-data")) {
      bool hasFields = false;
      MultipartRequest mp = new MultipartRequest(null, null);
      
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

    if(response.statusCode >= 400) {
      throw new ApiException(response.statusCode, response.body);
    } else if(response.body != null) {
      return apiClient.deserialize(response.body, 'Order') as Order ;
    } else {
      return null;
    }
  }
}

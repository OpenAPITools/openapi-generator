//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: avoid_init_to_null, lines_longer_than_80_chars
// ignore_for_file: prefer_single_quotes

part of openapi.api;


class StoreApi {
  StoreApi([ApiClient apiClient]) : apiClient = apiClient ?? defaultApiClient;

  final ApiClient apiClient;

  /// Delete purchase order by ID with HTTP info returned
  ///
  /// For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors
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

    String nullableContentType = contentTypes.isNotEmpty ? contentTypes[0] : null;
    List<String> authNames = [];

    if(nullableContentType != null && nullableContentType.startsWith("multipart/form-data")) {
      bool hasFields = false;
      MultipartRequest mp = MultipartRequest(null, null);
      if(hasFields) {
        postBody = mp;
      }
    } else {
    }

    return await apiClient.invokeAPI(
      path,
      "DELETE",
      queryParams,
      postBody,
      headerParams,
      formParams,
      nullableContentType,
      authNames,
    );
  }

  /// Delete purchase order by ID
  ///
  ///String orderId  (required):
  ///     ID of the order that needs to be deleted
  /// For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors
  Future deleteOrder(String orderId) async {
    final response = await deleteOrderWithHttpInfo(orderId);
    if(response.statusCode >= 400) {
      throw ApiException(response.statusCode, _decodeBodyBytes(response));
    }
    if(response.body != null) {
    }
    return;
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

    String nullableContentType = contentTypes.isNotEmpty ? contentTypes[0] : null;
    List<String> authNames = ["api_key"];

    if(nullableContentType != null && nullableContentType.startsWith("multipart/form-data")) {
      bool hasFields = false;
      MultipartRequest mp = MultipartRequest(null, null);
      if(hasFields) {
        postBody = mp;
      }
    } else {
    }

    return await apiClient.invokeAPI(
      path,
      "GET",
      queryParams,
      postBody,
      headerParams,
      formParams,
      nullableContentType,
      authNames,
    );
  }

  /// Returns pet inventories by status
  ///
  /// Returns a map of status codes to quantities
  Future<Map<String, int>> getInventory() async {
    final response = await getInventoryWithHttpInfo();
    if(response.statusCode >= 400) {
      throw ApiException(response.statusCode, _decodeBodyBytes(response));
    }
    if(response.body != null) {
      return Map<String, int>.from(apiClient.deserialize(_decodeBodyBytes(response), "Map<String, int>"));
          ;
    }
    return null;
  }

  /// Find purchase order by ID with HTTP info returned
  ///
  /// For valid response try integer IDs with value <= 5 or > 10. Other values will generated exceptions
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

    String nullableContentType = contentTypes.isNotEmpty ? contentTypes[0] : null;
    List<String> authNames = [];

    if(nullableContentType != null && nullableContentType.startsWith("multipart/form-data")) {
      bool hasFields = false;
      MultipartRequest mp = MultipartRequest(null, null);
      if(hasFields) {
        postBody = mp;
      }
    } else {
    }

    return await apiClient.invokeAPI(
      path,
      "GET",
      queryParams,
      postBody,
      headerParams,
      formParams,
      nullableContentType,
      authNames,
    );
  }

  /// Find purchase order by ID
  ///
  ///int orderId  (required):
  ///     ID of pet that needs to be fetched
  /// For valid response try integer IDs with value <= 5 or > 10. Other values will generated exceptions
  Future<Order> getOrderById(int orderId) async {
    final response = await getOrderByIdWithHttpInfo(orderId);
    if(response.statusCode >= 400) {
      throw ApiException(response.statusCode, _decodeBodyBytes(response));
    }
    if(response.body != null) {
      return apiClient.deserialize(_decodeBodyBytes(response), "Order") as Order;
    }
    return null;
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

    String nullableContentType = contentTypes.isNotEmpty ? contentTypes[0] : null;
    List<String> authNames = [];

    if(nullableContentType != null && nullableContentType.startsWith("multipart/form-data")) {
      bool hasFields = false;
      MultipartRequest mp = MultipartRequest(null, null);
      if(hasFields) {
        postBody = mp;
      }
    } else {
    }

    return await apiClient.invokeAPI(
      path,
      "POST",
      queryParams,
      postBody,
      headerParams,
      formParams,
      nullableContentType,
      authNames,
    );
  }

  /// Place an order for a pet
  ///
  ///Order body  (required):
  ///     order placed for purchasing the pet
  /// 
  Future<Order> placeOrder(Order body) async {
    final response = await placeOrderWithHttpInfo(body);
    if(response.statusCode >= 400) {
      throw ApiException(response.statusCode, _decodeBodyBytes(response));
    }
    if(response.body != null) {
      return apiClient.deserialize(_decodeBodyBytes(response), "Order") as Order;
    }
    return null;
  }
}

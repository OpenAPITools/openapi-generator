//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;


class StoreApi {
  StoreApi([ApiClient apiClient]) : apiClient = apiClient ?? defaultApiClient;

  final ApiClient apiClient;

  /// Delete purchase order by ID
  ///
  /// For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors
  ///
  /// Note: This method returns the HTTP [Response].
  ///
  /// Parameters:
  ///
  /// * [String] orderId (required):
  ///   ID of the order that needs to be deleted
  Future<Response> deleteOrderWithHttpInfo(String orderId) async {
    // Verify required params are set.
    if (orderId == null) {
     throw ApiException(HttpStatus.badRequest, 'Missing required param: orderId');
    }

    final path = r'/store/order/{order_id}'
      .replaceAll('{' + 'order_id' + '}', orderId.toString());

    Object postBody;

    final queryParams = <QueryParam>[];
    final headerParams = <String, String>{};
    final formParams = <String, String>{};

    final contentTypes = <String>[];
    final nullableContentType = contentTypes.isNotEmpty ? contentTypes[0] : null;
    final authNames = <String>[];


    return await apiClient.invokeAPI(
      path,
      'DELETE',
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
  /// For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors
  ///
  /// Parameters:
  ///
  /// * [String] orderId (required):
  ///   ID of the order that needs to be deleted
  Future<void> deleteOrder(String orderId) async {
    final response = await deleteOrderWithHttpInfo(orderId);
    if (response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(response.statusCode, await _decodeBodyBytes(response));
    }
  }

  /// Returns pet inventories by status
  ///
  /// Returns a map of status codes to quantities
  ///
  /// Note: This method returns the HTTP [Response].
  Future<Response> getInventoryWithHttpInfo() async {
    final path = r'/store/inventory';

    Object postBody;

    final queryParams = <QueryParam>[];
    final headerParams = <String, String>{};
    final formParams = <String, String>{};

    final contentTypes = <String>[];
    final nullableContentType = contentTypes.isNotEmpty ? contentTypes[0] : null;
    final authNames = <String>['api_key'];


    return await apiClient.invokeAPI(
      path,
      'GET',
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
    if (response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(response.statusCode, await _decodeBodyBytes(response));
    }
    // When a remote server returns no body with a status of 204, we shall not decode it.
    // At the time of writing this, `dart:convert` will throw an "Unexpected end of input"
    // FormatException when trying to decode an empty string.
    if (response.body != null && response.statusCode != HttpStatus.noContent) {
      return Map<String, int>.from(await apiClient.deserializeAsync(await _decodeBodyBytes(response), 'Map<String, int>'),);
    }
    return Future<Map<String, int>>.value(null);
  }

  /// Find purchase order by ID
  ///
  /// For valid response try integer IDs with value <= 5 or > 10. Other values will generated exceptions
  ///
  /// Note: This method returns the HTTP [Response].
  ///
  /// Parameters:
  ///
  /// * [int] orderId (required):
  ///   ID of pet that needs to be fetched
  Future<Response> getOrderByIdWithHttpInfo(int orderId) async {
    // Verify required params are set.
    if (orderId == null) {
     throw ApiException(HttpStatus.badRequest, 'Missing required param: orderId');
    }

    final path = r'/store/order/{order_id}'
      .replaceAll('{' + 'order_id' + '}', orderId.toString());

    Object postBody;

    final queryParams = <QueryParam>[];
    final headerParams = <String, String>{};
    final formParams = <String, String>{};

    final contentTypes = <String>[];
    final nullableContentType = contentTypes.isNotEmpty ? contentTypes[0] : null;
    final authNames = <String>[];


    return await apiClient.invokeAPI(
      path,
      'GET',
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
  /// For valid response try integer IDs with value <= 5 or > 10. Other values will generated exceptions
  ///
  /// Parameters:
  ///
  /// * [int] orderId (required):
  ///   ID of pet that needs to be fetched
  Future<Order> getOrderById(int orderId) async {
    final response = await getOrderByIdWithHttpInfo(orderId);
    if (response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(response.statusCode, await _decodeBodyBytes(response));
    }
    // When a remote server returns no body with a status of 204, we shall not decode it.
    // At the time of writing this, `dart:convert` will throw an "Unexpected end of input"
    // FormatException when trying to decode an empty string.
    if (response.body != null && response.statusCode != HttpStatus.noContent) {
      return await apiClient.deserializeAsync(await _decodeBodyBytes(response), 'Order',) as Order;
        }
    return Future<Order>.value(null);
  }

  /// Place an order for a pet
  ///
  /// Note: This method returns the HTTP [Response].
  ///
  /// Parameters:
  ///
  /// * [Order] order (required):
  ///   order placed for purchasing the pet
  Future<Response> placeOrderWithHttpInfo(Order order) async {
    // Verify required params are set.
    if (order == null) {
     throw ApiException(HttpStatus.badRequest, 'Missing required param: order');
    }

    final path = r'/store/order';

    Object postBody = order;

    final queryParams = <QueryParam>[];
    final headerParams = <String, String>{};
    final formParams = <String, String>{};

    final contentTypes = <String>['application/json'];
    final nullableContentType = contentTypes.isNotEmpty ? contentTypes[0] : null;
    final authNames = <String>[];


    return await apiClient.invokeAPI(
      path,
      'POST',
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
  /// Parameters:
  ///
  /// * [Order] order (required):
  ///   order placed for purchasing the pet
  Future<Order> placeOrder(Order order) async {
    final response = await placeOrderWithHttpInfo(order);
    if (response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(response.statusCode, await _decodeBodyBytes(response));
    }
    // When a remote server returns no body with a status of 204, we shall not decode it.
    // At the time of writing this, `dart:convert` will throw an "Unexpected end of input"
    // FormatException when trying to decode an empty string.
    if (response.body != null && response.statusCode != HttpStatus.noContent) {
      return await apiClient.deserializeAsync(await _decodeBodyBytes(response), 'Order',) as Order;
        }
    return Future<Order>.value(null);
  }
}

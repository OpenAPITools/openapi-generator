//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.12

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;


class StoreApi {
  StoreApi([ApiClient? apiClient]) : apiClient = apiClient ?? defaultApiClient;

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
  Future<Response> deleteOrderWithHttpInfo(String orderId,) async {
    // ignore: prefer_const_declarations
    final path = r'/store/order/{order_id}'
      .replaceAll('{order_id}', orderId);

    // ignore: prefer_final_locals
    Object? postBody;

    final queryParams = <QueryParam>[];
    final headerParams = <String, String>{};
    final formParams = <String, String>{};

    const authNames = <String>[];
    const contentTypes = <String>[];


    return apiClient.invokeAPI(
      path,
      'DELETE',
      queryParams,
      postBody,
      headerParams,
      formParams,
      contentTypes.isEmpty ? null : contentTypes.first,
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
  Future<void> deleteOrder(String orderId,) async {
    final response = await deleteOrderWithHttpInfo(orderId,);
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
    // ignore: prefer_const_declarations
    final path = r'/store/inventory';

    // ignore: prefer_final_locals
    Object? postBody;

    final queryParams = <QueryParam>[];
    final headerParams = <String, String>{};
    final formParams = <String, String>{};

    const authNames = <String>['api_key'];
    const contentTypes = <String>[];


    return apiClient.invokeAPI(
      path,
      'GET',
      queryParams,
      postBody,
      headerParams,
      formParams,
      contentTypes.isEmpty ? null : contentTypes.first,
      authNames,
    );
  }

  /// Returns pet inventories by status
  ///
  /// Returns a map of status codes to quantities
  Future<Map<String, int>?> getInventory() async {
    final response = await getInventoryWithHttpInfo();
    if (response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(response.statusCode, await _decodeBodyBytes(response));
    }
    // When a remote server returns no body with a status of 204, we shall not decode it.
    // At the time of writing this, `dart:convert` will throw an "Unexpected end of input"
    // FormatException when trying to decode an empty string.
    if (response.body.isNotEmpty && response.statusCode != HttpStatus.noContent) {
      return Map<String, int>.from(await apiClient.deserializeAsync(await _decodeBodyBytes(response), 'Map<String, int>'),);

    }
    return null;
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
  Future<Response> getOrderByIdWithHttpInfo(int orderId,) async {
    // ignore: prefer_const_declarations
    final path = r'/store/order/{order_id}'
      .replaceAll('{order_id}', orderId.toString());

    // ignore: prefer_final_locals
    Object? postBody;

    final queryParams = <QueryParam>[];
    final headerParams = <String, String>{};
    final formParams = <String, String>{};

    const authNames = <String>[];
    const contentTypes = <String>[];


    return apiClient.invokeAPI(
      path,
      'GET',
      queryParams,
      postBody,
      headerParams,
      formParams,
      contentTypes.isEmpty ? null : contentTypes.first,
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
  Future<Order?> getOrderById(int orderId,) async {
    final response = await getOrderByIdWithHttpInfo(orderId,);
    if (response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(response.statusCode, await _decodeBodyBytes(response));
    }
    // When a remote server returns no body with a status of 204, we shall not decode it.
    // At the time of writing this, `dart:convert` will throw an "Unexpected end of input"
    // FormatException when trying to decode an empty string.
    if (response.body.isNotEmpty && response.statusCode != HttpStatus.noContent) {
      return await apiClient.deserializeAsync(await _decodeBodyBytes(response), 'Order',) as Order;
    
    }
    return null;
  }

  /// Place an order for a pet
  ///
  /// 
  ///
  /// Note: This method returns the HTTP [Response].
  ///
  /// Parameters:
  ///
  /// * [Order] order (required):
  ///   order placed for purchasing the pet
  Future<Response> placeOrderWithHttpInfo(Order order,) async {
    // ignore: prefer_const_declarations
    final path = r'/store/order';

    // ignore: prefer_final_locals
    Object? postBody = order;

    final queryParams = <QueryParam>[];
    final headerParams = <String, String>{};
    final formParams = <String, String>{};

    const authNames = <String>[];
    const contentTypes = <String>['application/json'];


    return apiClient.invokeAPI(
      path,
      'POST',
      queryParams,
      postBody,
      headerParams,
      formParams,
      contentTypes.isEmpty ? null : contentTypes.first,
      authNames,
    );
  }

  /// Place an order for a pet
  ///
  /// 
  ///
  /// Parameters:
  ///
  /// * [Order] order (required):
  ///   order placed for purchasing the pet
  Future<Order?> placeOrder(Order order,) async {
    final response = await placeOrderWithHttpInfo(order,);
    if (response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(response.statusCode, await _decodeBodyBytes(response));
    }
    // When a remote server returns no body with a status of 204, we shall not decode it.
    // At the time of writing this, `dart:convert` will throw an "Unexpected end of input"
    // FormatException when trying to decode an empty string.
    if (response.body.isNotEmpty && response.statusCode != HttpStatus.noContent) {
      return await apiClient.deserializeAsync(await _decodeBodyBytes(response), 'Order',) as Order;
    
    }
    return null;
  }
}

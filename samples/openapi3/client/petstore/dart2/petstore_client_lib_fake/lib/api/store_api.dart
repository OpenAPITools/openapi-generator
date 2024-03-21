//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.18

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
    final _path = r'/store/order/{order_id}'
      .replaceAll('{order_id}', orderId);

    // ignore: prefer_final_locals
    Object? _postBody;

    final __queryParams = <QueryParam>[];
    final _headerParams = <String, String>{};
    final _formParams = <String, String>{};

    const _contentTypes = <String>[];


    return apiClient.invokeAPI(
      _path,
      'DELETE',
      __queryParams,
      _postBody,
      _headerParams,
      _formParams,
      _contentTypes.isEmpty ? null : _contentTypes.first,
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
    final _response = await deleteOrderWithHttpInfo(orderId,);
    if (_response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(_response.statusCode, await _decodeBodyBytes(_response));
    }
  }

  /// Returns pet inventories by status
  ///
  /// Returns a map of status codes to quantities
  ///
  /// Note: This method returns the HTTP [Response].
  Future<Response> getInventoryWithHttpInfo() async {
    // ignore: prefer_const_declarations
    final _path = r'/store/inventory';

    // ignore: prefer_final_locals
    Object? _postBody;

    final __queryParams = <QueryParam>[];
    final _headerParams = <String, String>{};
    final _formParams = <String, String>{};

    const _contentTypes = <String>[];


    return apiClient.invokeAPI(
      _path,
      'GET',
      __queryParams,
      _postBody,
      _headerParams,
      _formParams,
      _contentTypes.isEmpty ? null : _contentTypes.first,
    );
  }

  /// Returns pet inventories by status
  ///
  /// Returns a map of status codes to quantities
  Future<Map<String, int>?> getInventory() async {
    final _response = await getInventoryWithHttpInfo();
    if (_response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(_response.statusCode, await _decodeBodyBytes(_response));
    }
    // When a remote server returns no body with a status of 204, we shall not decode it.
    // At the time of writing this, `dart:convert` will throw an "Unexpected end of input"
    // FormatException when trying to decode an empty string.
    if (_response.bodyBytes.isNotEmpty && _response.statusCode != HttpStatus.noContent) {
      return Map<String, int>.from(await apiClient.deserializeAsync(await _decodeBodyBytes(_response), 'Map<String, int>'),);

    }
    return null;
  }

  /// Find purchase order by ID
  ///
  /// For valid response try integer IDs with value <= 5 or > 10. Other values will generate exceptions
  ///
  /// Note: This method returns the HTTP [Response].
  ///
  /// Parameters:
  ///
  /// * [int] orderId (required):
  ///   ID of pet that needs to be fetched
  Future<Response> getOrderByIdWithHttpInfo(int orderId,) async {
    // ignore: prefer_const_declarations
    final _path = r'/store/order/{order_id}'
      .replaceAll('{order_id}', orderId.toString());

    // ignore: prefer_final_locals
    Object? _postBody;

    final __queryParams = <QueryParam>[];
    final _headerParams = <String, String>{};
    final _formParams = <String, String>{};

    const _contentTypes = <String>[];


    return apiClient.invokeAPI(
      _path,
      'GET',
      __queryParams,
      _postBody,
      _headerParams,
      _formParams,
      _contentTypes.isEmpty ? null : _contentTypes.first,
    );
  }

  /// Find purchase order by ID
  ///
  /// For valid response try integer IDs with value <= 5 or > 10. Other values will generate exceptions
  ///
  /// Parameters:
  ///
  /// * [int] orderId (required):
  ///   ID of pet that needs to be fetched
  Future<Order?> getOrderById(int orderId,) async {
    final _response = await getOrderByIdWithHttpInfo(orderId,);
    if (_response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(_response.statusCode, await _decodeBodyBytes(_response));
    }
    // When a remote server returns no body with a status of 204, we shall not decode it.
    // At the time of writing this, `dart:convert` will throw an "Unexpected end of input"
    // FormatException when trying to decode an empty string.
    if (_response.bodyBytes.isNotEmpty && _response.statusCode != HttpStatus.noContent) {
      return await apiClient.deserializeAsync(await _decodeBodyBytes(_response), 'Order',) as Order;
    
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
    final _path = r'/store/order';

    // ignore: prefer_final_locals
    Object? _postBody = order;

    final __queryParams = <QueryParam>[];
    final _headerParams = <String, String>{};
    final _formParams = <String, String>{};

    const _contentTypes = <String>['application/json'];


    return apiClient.invokeAPI(
      _path,
      'POST',
      __queryParams,
      _postBody,
      _headerParams,
      _formParams,
      _contentTypes.isEmpty ? null : _contentTypes.first,
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
    final _response = await placeOrderWithHttpInfo(order,);
    if (_response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(_response.statusCode, await _decodeBodyBytes(_response));
    }
    // When a remote server returns no body with a status of 204, we shall not decode it.
    // At the time of writing this, `dart:convert` will throw an "Unexpected end of input"
    // FormatException when trying to decode an empty string.
    if (_response.bodyBytes.isNotEmpty && _response.statusCode != HttpStatus.noContent) {
      return await apiClient.deserializeAsync(await _decodeBodyBytes(_response), 'Order',) as Order;
    
    }
    return null;
  }
}

//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

import 'dart:async';
import 'package:dio/dio.dart';
import 'package:openapi/src/repository_base.dart';
import 'package:openapi/src/api_util.dart';
import 'package:openapi/models.dart';
import 'package:openapi/src/model/order.dart';

class StoreApi {
  final StoreApiRaw rawApi;
  final SerializationRepositoryBase _repository;

  const StoreApi(this.rawApi, this._repository);

  /// Delete purchase order by ID
  /// For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
  ///
  /// Parameters:
  /// * [orderId] - ID of the order that needs to be deleted
  /// * [cancelToken] - A [CancelToken] that can be used to cancel the operation
  /// * [headers] - Can be used to add additional headers to the request
  /// * [extras] - Can be used to add flags to the request
  /// * [validateStatus] - A [ValidateStatus] callback that can be used to determine request success based on the HTTP status of the response
  /// * [onSendProgress] - A [ProgressCallback] that can be used to get the send progress
  /// * [onReceiveProgress] - A [ProgressCallback] that can be used to get the receive progress
  ///
  /// Returns a [Future]
<<<<<<< HEAD:samples/openapi3/client/petstore/dart-dio/dio/json_serializable/lib/src/api/store_api.dart
  /// Throws [DioError] if API call or serialization fails
  Future<Response<void>> deleteOrder({
    required String orderId,
=======
  /// Throws [DioException] if API call or serialization fails
  Future<Response<void>> createUser({ 
    required User user,
>>>>>>> 95cefaeecdae21a43a453f07ed510c420abaa461:samples/openapi3/client/petstore/dart-dio/petstore_client_lib_fake-json_serializable/lib/src/api/user_api.dart
    CancelToken? cancelToken,
    Map<String, dynamic>? headers,
    Map<String, dynamic>? extra,
    ValidateStatus? validateStatus,
    ProgressCallback? onSendProgress,
    ProgressCallback? onReceiveProgress,
  }) async {
<<<<<<< HEAD:samples/openapi3/client/petstore/dart-dio/dio/json_serializable/lib/src/api/store_api.dart
    final _response = await rawApi.deleteOrder(
      orderId:
          _repository.serialize(orderId, const TypeInfo(String)).toString(),
=======
    final _path = r'/user';
    final _options = Options(
      method: r'POST',
      headers: <String, dynamic>{
        ...?headers,
      },
      extra: <String, dynamic>{
        'secure': <Map<String, String>>[],
        ...?extra,
      },
      contentType: 'application/json',
      validateStatus: validateStatus,
    );

    dynamic _bodyData;

    try {
_bodyData=jsonEncode(user);
    } catch(error, stackTrace) {
      throw DioException(
         requestOptions: _options.compose(
          _dio.options,
          _path,
        ),
        type: DioExceptionType.unknown,
        error: error,
        stackTrace: stackTrace,
      );
    }

    final _response = await _dio.request<Object>(
      _path,
      data: _bodyData,
      options: _options,
>>>>>>> 95cefaeecdae21a43a453f07ed510c420abaa461:samples/openapi3/client/petstore/dart-dio/petstore_client_lib_fake-json_serializable/lib/src/api/user_api.dart
      cancelToken: cancelToken,
      headers: headers,
      extra: extra,
      validateStatus: validateStatus,
      onSendProgress: onSendProgress,
      onReceiveProgress: onReceiveProgress,
    );

    return _response;
  }

  /// Returns pet inventories by status
  /// Returns a map of status codes to quantities
  ///
  /// Parameters:
  /// * [cancelToken] - A [CancelToken] that can be used to cancel the operation
  /// * [headers] - Can be used to add additional headers to the request
  /// * [extras] - Can be used to add flags to the request
  /// * [validateStatus] - A [ValidateStatus] callback that can be used to determine request success based on the HTTP status of the response
  /// * [onSendProgress] - A [ProgressCallback] that can be used to get the send progress
  /// * [onReceiveProgress] - A [ProgressCallback] that can be used to get the receive progress
  ///
<<<<<<< HEAD:samples/openapi3/client/petstore/dart-dio/dio/json_serializable/lib/src/api/store_api.dart
  /// Returns a [Future] containing a [Response] with a [Map<String, int>] as data
  /// Throws [DioError] if API call or serialization fails
  Future<Response<Map<String, int>>> getInventory({
=======
  /// Returns a [Future]
  /// Throws [DioException] if API call or serialization fails
  Future<Response<void>> createUsersWithArrayInput({ 
    required List<User> user,
>>>>>>> 95cefaeecdae21a43a453f07ed510c420abaa461:samples/openapi3/client/petstore/dart-dio/petstore_client_lib_fake-json_serializable/lib/src/api/user_api.dart
    CancelToken? cancelToken,
    Map<String, dynamic>? headers,
    Map<String, dynamic>? extra,
    ValidateStatus? validateStatus,
    ProgressCallback? onSendProgress,
    ProgressCallback? onReceiveProgress,
  }) async {
    final _response = await rawApi.getInventory(
      cancelToken: cancelToken,
      headers: headers,
      extra: extra,
      validateStatus: validateStatus,
      onSendProgress: onSendProgress,
      onReceiveProgress: onReceiveProgress,
    );

    Map<String, int>? _responseData;

    try {
<<<<<<< HEAD:samples/openapi3/client/petstore/dart-dio/dio/json_serializable/lib/src/api/store_api.dart
      final rawResponse = _response.data;
      _responseData = rawResponse == null
          ? null
          : _repository.deserialize(
              rawResponse,
              const TypeInfo(Map, [
                TypeInfo(String),
                const TypeInfo(int),
              ]),
            );
    } catch (error, stackTrace) {
      throw DioError(
        requestOptions: _response.requestOptions,
        response: _response,
        type: DioErrorType.unknown,
=======
_bodyData=jsonEncode(user);
    } catch(error, stackTrace) {
      throw DioException(
         requestOptions: _options.compose(
          _dio.options,
          _path,
        ),
        type: DioExceptionType.unknown,
>>>>>>> 95cefaeecdae21a43a453f07ed510c420abaa461:samples/openapi3/client/petstore/dart-dio/petstore_client_lib_fake-json_serializable/lib/src/api/user_api.dart
        error: error,
        stackTrace: stackTrace,
      );
    }

    return Response<Map<String, int>>(
      data: _responseData,
      headers: _response.headers,
      isRedirect: _response.isRedirect,
      requestOptions: _response.requestOptions,
      redirects: _response.redirects,
      statusCode: _response.statusCode,
      statusMessage: _response.statusMessage,
      extra: _response.extra,
    );
  }

  /// Find purchase order by ID
  /// For valid response try integer IDs with value &lt;&#x3D; 5 or &gt; 10. Other values will generate exceptions
  ///
  /// Parameters:
  /// * [orderId] - ID of pet that needs to be fetched
  /// * [cancelToken] - A [CancelToken] that can be used to cancel the operation
  /// * [headers] - Can be used to add additional headers to the request
  /// * [extras] - Can be used to add flags to the request
  /// * [validateStatus] - A [ValidateStatus] callback that can be used to determine request success based on the HTTP status of the response
  /// * [onSendProgress] - A [ProgressCallback] that can be used to get the send progress
  /// * [onReceiveProgress] - A [ProgressCallback] that can be used to get the receive progress
  ///
<<<<<<< HEAD:samples/openapi3/client/petstore/dart-dio/dio/json_serializable/lib/src/api/store_api.dart
  /// Returns a [Future] containing a [Response] with a [Order] as data
  /// Throws [DioError] if API call or serialization fails
  Future<Response<Order>> getOrderById({
    required int orderId,
=======
  /// Returns a [Future]
  /// Throws [DioException] if API call or serialization fails
  Future<Response<void>> createUsersWithListInput({ 
    required List<User> user,
>>>>>>> 95cefaeecdae21a43a453f07ed510c420abaa461:samples/openapi3/client/petstore/dart-dio/petstore_client_lib_fake-json_serializable/lib/src/api/user_api.dart
    CancelToken? cancelToken,
    Map<String, dynamic>? headers,
    Map<String, dynamic>? extra,
    ValidateStatus? validateStatus,
    ProgressCallback? onSendProgress,
    ProgressCallback? onReceiveProgress,
  }) async {
    final _response = await rawApi.getOrderById(
      orderId: _repository.serialize(orderId, const TypeInfo(int)).toString(),
      cancelToken: cancelToken,
      headers: headers,
      extra: extra,
      validateStatus: validateStatus,
      onSendProgress: onSendProgress,
      onReceiveProgress: onReceiveProgress,
    );

    Order? _responseData;

    try {
<<<<<<< HEAD:samples/openapi3/client/petstore/dart-dio/dio/json_serializable/lib/src/api/store_api.dart
      final rawResponse = _response.data;
      _responseData = rawResponse == null
          ? null
          : _repository.deserialize(
              rawResponse,
              const TypeInfo(Order),
            );
    } catch (error, stackTrace) {
      throw DioError(
        requestOptions: _response.requestOptions,
        response: _response,
        type: DioErrorType.unknown,
=======
_bodyData=jsonEncode(user);
    } catch(error, stackTrace) {
      throw DioException(
         requestOptions: _options.compose(
          _dio.options,
          _path,
        ),
        type: DioExceptionType.unknown,
>>>>>>> 95cefaeecdae21a43a453f07ed510c420abaa461:samples/openapi3/client/petstore/dart-dio/petstore_client_lib_fake-json_serializable/lib/src/api/user_api.dart
        error: error,
        stackTrace: stackTrace,
      );
    }

    return Response<Order>(
      data: _responseData,
      headers: _response.headers,
      isRedirect: _response.isRedirect,
      requestOptions: _response.requestOptions,
      redirects: _response.redirects,
      statusCode: _response.statusCode,
      statusMessage: _response.statusMessage,
      extra: _response.extra,
    );
  }

  /// Place an order for a pet
  ///
  ///
  /// Parameters:
  /// * [order] - order placed for purchasing the pet
  /// * [cancelToken] - A [CancelToken] that can be used to cancel the operation
  /// * [headers] - Can be used to add additional headers to the request
  /// * [extras] - Can be used to add flags to the request
  /// * [validateStatus] - A [ValidateStatus] callback that can be used to determine request success based on the HTTP status of the response
  /// * [onSendProgress] - A [ProgressCallback] that can be used to get the send progress
  /// * [onReceiveProgress] - A [ProgressCallback] that can be used to get the receive progress
  ///
<<<<<<< HEAD:samples/openapi3/client/petstore/dart-dio/dio/json_serializable/lib/src/api/store_api.dart
  /// Returns a [Future] containing a [Response] with a [Order] as data
  /// Throws [DioError] if API call or serialization fails
  Future<Response<Order>> placeOrder({
    required Order order,
=======
  /// Returns a [Future]
  /// Throws [DioException] if API call or serialization fails
  Future<Response<void>> deleteUser({ 
    required String username,
>>>>>>> 95cefaeecdae21a43a453f07ed510c420abaa461:samples/openapi3/client/petstore/dart-dio/petstore_client_lib_fake-json_serializable/lib/src/api/user_api.dart
    CancelToken? cancelToken,
    Map<String, dynamic>? headers,
    Map<String, dynamic>? extra,
    ValidateStatus? validateStatus,
    ProgressCallback? onSendProgress,
    ProgressCallback? onReceiveProgress,
  }) async {
    Object? _bodyData;
    _bodyData = _repository.serialize(order, const TypeInfo(Order));

    final _response = await rawApi.placeOrder(
      body: _bodyData,
      requestContentType: 'application/json',
      cancelToken: cancelToken,
      headers: headers,
      extra: extra,
      validateStatus: validateStatus,
      onSendProgress: onSendProgress,
      onReceiveProgress: onReceiveProgress,
    );

    Order? _responseData;

    try {
      final rawResponse = _response.data;
      _responseData = rawResponse == null
          ? null
          : _repository.deserialize(
              rawResponse,
              const TypeInfo(Order),
            );
    } catch (error, stackTrace) {
      throw DioError(
        requestOptions: _response.requestOptions,
        response: _response,
        type: DioErrorType.unknown,
        error: error,
        stackTrace: stackTrace,
      );
    }

    return Response<Order>(
      data: _responseData,
      headers: _response.headers,
      isRedirect: _response.isRedirect,
      requestOptions: _response.requestOptions,
      redirects: _response.redirects,
      statusCode: _response.statusCode,
      statusMessage: _response.statusMessage,
      extra: _response.extra,
    );
  }
}

class StoreApiRaw {
  final Dio _dio;

  const StoreApiRaw(this._dio);

  /// Delete purchase order by ID
  /// For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
  ///
  /// Parameters:
  /// * [orderId] - ID of the order that needs to be deleted
  /// * [cancelToken] - A [CancelToken] that can be used to cancel the operation
  /// * [headers] - Can be used to add additional headers to the request
  /// * [extras] - Can be used to add flags to the request
  /// * [validateStatus] - A [ValidateStatus] callback that can be used to determine request success based on the HTTP status of the response
  /// * [onSendProgress] - A [ProgressCallback] that can be used to get the send progress
  /// * [onReceiveProgress] - A [ProgressCallback] that can be used to get the receive progress
  ///
<<<<<<< HEAD:samples/openapi3/client/petstore/dart-dio/dio/json_serializable/lib/src/api/store_api.dart
  /// Returns a [Future]
  /// Throws [DioError] if API call or serialization fails
  Future<Response<void>> deleteOrder({
    required String orderId,
    Object? body,
    String? requestContentType,
    String? acceptContentType,
    ResponseType? responseType,
=======
  /// Returns a [Future] containing a [Response] with a [User] as data
  /// Throws [DioException] if API call or serialization fails
  Future<Response<User>> getUserByName({ 
    required String username,
>>>>>>> 95cefaeecdae21a43a453f07ed510c420abaa461:samples/openapi3/client/petstore/dart-dio/petstore_client_lib_fake-json_serializable/lib/src/api/user_api.dart
    CancelToken? cancelToken,
    Map<String, dynamic>? headers,
    Map<String, dynamic>? extra,
    ValidateStatus? validateStatus,
    ProgressCallback? onSendProgress,
    ProgressCallback? onReceiveProgress,
  }) async {
    final _path = r'/store/order/{order_id}'
        .replaceAll('{' r'order_id' '}', orderId.toString());
    final _options = Options(
      method: r'DELETE',
      headers: <String, dynamic>{
        if (acceptContentType != null) 'Accept': acceptContentType,
        ...?headers,
      },
      extra: <String, dynamic>{
        'secure': <Map<String, String>>[],
        ...?extra,
      },
      contentType: requestContentType,
      responseType: responseType,
      validateStatus: validateStatus,
    );

    return await _dio.request<Object>(
      _path,
      data: body,
      options: _options,
      cancelToken: cancelToken,
      onSendProgress: onSendProgress,
      onReceiveProgress: onReceiveProgress,
    );
<<<<<<< HEAD:samples/openapi3/client/petstore/dart-dio/dio/json_serializable/lib/src/api/store_api.dart
=======

    User? _responseData;

    try {
final rawData = _response.data;
_responseData = rawData == null ? null : deserialize<User, User>(rawData, 'User', growable: true);
    } catch (error, stackTrace) {
      throw DioException(
        requestOptions: _response.requestOptions,
        response: _response,
        type: DioExceptionType.unknown,
        error: error,
        stackTrace: stackTrace,
      );
    }

    return Response<User>(
      data: _responseData,
      headers: _response.headers,
      isRedirect: _response.isRedirect,
      requestOptions: _response.requestOptions,
      redirects: _response.redirects,
      statusCode: _response.statusCode,
      statusMessage: _response.statusMessage,
      extra: _response.extra,
    );
>>>>>>> 95cefaeecdae21a43a453f07ed510c420abaa461:samples/openapi3/client/petstore/dart-dio/petstore_client_lib_fake-json_serializable/lib/src/api/user_api.dart
  }

  /// Returns pet inventories by status
  /// Returns a map of status codes to quantities
  ///
  /// Parameters:
  /// * [cancelToken] - A [CancelToken] that can be used to cancel the operation
  /// * [headers] - Can be used to add additional headers to the request
  /// * [extras] - Can be used to add flags to the request
  /// * [validateStatus] - A [ValidateStatus] callback that can be used to determine request success based on the HTTP status of the response
  /// * [onSendProgress] - A [ProgressCallback] that can be used to get the send progress
  /// * [onReceiveProgress] - A [ProgressCallback] that can be used to get the receive progress
  ///
<<<<<<< HEAD:samples/openapi3/client/petstore/dart-dio/dio/json_serializable/lib/src/api/store_api.dart
  /// Returns a [Future] containing a [Response] with a [Map<String, int>] as data
  /// Throws [DioError] if API call or serialization fails
  Future<Response<Object>> getInventory({
    Object? body,
    String? requestContentType,
    String? acceptContentType,
    ResponseType? responseType,
=======
  /// Returns a [Future] containing a [Response] with a [String] as data
  /// Throws [DioException] if API call or serialization fails
  Future<Response<String>> loginUser({ 
    required String username,
    required String password,
>>>>>>> 95cefaeecdae21a43a453f07ed510c420abaa461:samples/openapi3/client/petstore/dart-dio/petstore_client_lib_fake-json_serializable/lib/src/api/user_api.dart
    CancelToken? cancelToken,
    Map<String, dynamic>? headers,
    Map<String, dynamic>? extra,
    ValidateStatus? validateStatus,
    ProgressCallback? onSendProgress,
    ProgressCallback? onReceiveProgress,
  }) async {
    final _path = r'/store/inventory';
    final _options = Options(
      method: r'GET',
      headers: <String, dynamic>{
        if (acceptContentType != null) 'Accept': acceptContentType,
        ...?headers,
      },
      extra: <String, dynamic>{
        'secure': <Map<String, String>>[
          {
            'type': 'apiKey',
            'name': 'api_key',
            'keyName': 'api_key',
            'where': 'header',
          },
        ],
        ...?extra,
      },
      contentType: requestContentType,
      responseType: responseType,
      validateStatus: validateStatus,
    );

    return await _dio.request<Object>(
      _path,
      data: body,
      options: _options,
      cancelToken: cancelToken,
      onSendProgress: onSendProgress,
      onReceiveProgress: onReceiveProgress,
    );
<<<<<<< HEAD:samples/openapi3/client/petstore/dart-dio/dio/json_serializable/lib/src/api/store_api.dart
=======

    String? _responseData;

    try {
final rawData = _response.data;
_responseData = rawData == null ? null : deserialize<String, String>(rawData, 'String', growable: true);
    } catch (error, stackTrace) {
      throw DioException(
        requestOptions: _response.requestOptions,
        response: _response,
        type: DioExceptionType.unknown,
        error: error,
        stackTrace: stackTrace,
      );
    }

    return Response<String>(
      data: _responseData,
      headers: _response.headers,
      isRedirect: _response.isRedirect,
      requestOptions: _response.requestOptions,
      redirects: _response.redirects,
      statusCode: _response.statusCode,
      statusMessage: _response.statusMessage,
      extra: _response.extra,
    );
>>>>>>> 95cefaeecdae21a43a453f07ed510c420abaa461:samples/openapi3/client/petstore/dart-dio/petstore_client_lib_fake-json_serializable/lib/src/api/user_api.dart
  }

  /// Find purchase order by ID
  /// For valid response try integer IDs with value &lt;&#x3D; 5 or &gt; 10. Other values will generate exceptions
  ///
  /// Parameters:
  /// * [orderId] - ID of pet that needs to be fetched
  /// * [cancelToken] - A [CancelToken] that can be used to cancel the operation
  /// * [headers] - Can be used to add additional headers to the request
  /// * [extras] - Can be used to add flags to the request
  /// * [validateStatus] - A [ValidateStatus] callback that can be used to determine request success based on the HTTP status of the response
  /// * [onSendProgress] - A [ProgressCallback] that can be used to get the send progress
  /// * [onReceiveProgress] - A [ProgressCallback] that can be used to get the receive progress
  ///
<<<<<<< HEAD:samples/openapi3/client/petstore/dart-dio/dio/json_serializable/lib/src/api/store_api.dart
  /// Returns a [Future] containing a [Response] with a [Order] as data
  /// Throws [DioError] if API call or serialization fails
  Future<Response<Object>> getOrderById({
    required String orderId,
    Object? body,
    String? requestContentType,
    String? acceptContentType,
    ResponseType? responseType,
=======
  /// Returns a [Future]
  /// Throws [DioException] if API call or serialization fails
  Future<Response<void>> logoutUser({ 
>>>>>>> 95cefaeecdae21a43a453f07ed510c420abaa461:samples/openapi3/client/petstore/dart-dio/petstore_client_lib_fake-json_serializable/lib/src/api/user_api.dart
    CancelToken? cancelToken,
    Map<String, dynamic>? headers,
    Map<String, dynamic>? extra,
    ValidateStatus? validateStatus,
    ProgressCallback? onSendProgress,
    ProgressCallback? onReceiveProgress,
  }) async {
    final _path = r'/store/order/{order_id}'
        .replaceAll('{' r'order_id' '}', orderId.toString());
    final _options = Options(
      method: r'GET',
      headers: <String, dynamic>{
        if (acceptContentType != null) 'Accept': acceptContentType,
        ...?headers,
      },
      extra: <String, dynamic>{
        'secure': <Map<String, String>>[],
        ...?extra,
      },
      contentType: requestContentType,
      responseType: responseType,
      validateStatus: validateStatus,
    );

    return await _dio.request<Object>(
      _path,
      data: body,
      options: _options,
      cancelToken: cancelToken,
      onSendProgress: onSendProgress,
      onReceiveProgress: onReceiveProgress,
    );
  }

  /// Place an order for a pet
  ///
  ///
  /// Parameters:
  /// * [order] - order placed for purchasing the pet
  /// * [cancelToken] - A [CancelToken] that can be used to cancel the operation
  /// * [headers] - Can be used to add additional headers to the request
  /// * [extras] - Can be used to add flags to the request
  /// * [validateStatus] - A [ValidateStatus] callback that can be used to determine request success based on the HTTP status of the response
  /// * [onSendProgress] - A [ProgressCallback] that can be used to get the send progress
  /// * [onReceiveProgress] - A [ProgressCallback] that can be used to get the receive progress
  ///
<<<<<<< HEAD:samples/openapi3/client/petstore/dart-dio/dio/json_serializable/lib/src/api/store_api.dart
  /// Returns a [Future] containing a [Response] with a [Order] as data
  /// Throws [DioError] if API call or serialization fails
  Future<Response<Object>> placeOrder({
    Object? body,
    String? requestContentType,
    String? acceptContentType,
    ResponseType? responseType,
=======
  /// Returns a [Future]
  /// Throws [DioException] if API call or serialization fails
  Future<Response<void>> updateUser({ 
    required String username,
    required User user,
>>>>>>> 95cefaeecdae21a43a453f07ed510c420abaa461:samples/openapi3/client/petstore/dart-dio/petstore_client_lib_fake-json_serializable/lib/src/api/user_api.dart
    CancelToken? cancelToken,
    Map<String, dynamic>? headers,
    Map<String, dynamic>? extra,
    ValidateStatus? validateStatus,
    ProgressCallback? onSendProgress,
    ProgressCallback? onReceiveProgress,
  }) async {
    final _path = r'/store/order';
    final _options = Options(
      method: r'POST',
      headers: <String, dynamic>{
        if (acceptContentType != null) 'Accept': acceptContentType,
        ...?headers,
      },
      extra: <String, dynamic>{
        'secure': <Map<String, String>>[],
        ...?extra,
      },
      contentType: requestContentType,
      responseType: responseType,
      validateStatus: validateStatus,
    );

<<<<<<< HEAD:samples/openapi3/client/petstore/dart-dio/dio/json_serializable/lib/src/api/store_api.dart
    return await _dio.request<Object>(
=======
    dynamic _bodyData;

    try {
_bodyData=jsonEncode(user);
    } catch(error, stackTrace) {
      throw DioException(
         requestOptions: _options.compose(
          _dio.options,
          _path,
        ),
        type: DioExceptionType.unknown,
        error: error,
        stackTrace: stackTrace,
      );
    }

    final _response = await _dio.request<Object>(
>>>>>>> 95cefaeecdae21a43a453f07ed510c420abaa461:samples/openapi3/client/petstore/dart-dio/petstore_client_lib_fake-json_serializable/lib/src/api/user_api.dart
      _path,
      data: body,
      options: _options,
      cancelToken: cancelToken,
      onSendProgress: onSendProgress,
      onReceiveProgress: onReceiveProgress,
    );
  }
}

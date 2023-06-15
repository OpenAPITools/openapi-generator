//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

import 'dart:async';
import 'package:dio/dio.dart';
import 'package:openapi/src/repository_base.dart';
import 'package:openapi/src/api_util.dart';
import 'package:openapi/models.dart';
import 'package:built_collection/built_collection.dart';
import 'package:openapi/src/model/foo.dart';
import 'package:openapi/src/model/foo_ref_or_value.dart';

class FooApi {
  final FooApiRaw rawApi;
  final SerializationRepositoryBase _repository;

  const FooApi(this.rawApi, this._repository);

  /// Create a Foo
  ///
  ///
  /// Parameters:
  /// * [foo] - The Foo to be created
  /// * [cancelToken] - A [CancelToken] that can be used to cancel the operation
  /// * [headers] - Can be used to add additional headers to the request
  /// * [extras] - Can be used to add flags to the request
  /// * [validateStatus] - A [ValidateStatus] callback that can be used to determine request success based on the HTTP status of the response
  /// * [onSendProgress] - A [ProgressCallback] that can be used to get the send progress
  /// * [onReceiveProgress] - A [ProgressCallback] that can be used to get the receive progress
  ///
<<<<<<< HEAD:samples/openapi3/client/petstore/dart-dio/dio/built_value/lib/src/api/foo_api.dart
  /// Returns a [Future] containing a [Response] with a [FooRefOrValue] as data
  /// Throws [DioError] if API call or serialization fails
  Future<Response<FooRefOrValue>> createFoo({
    Foo? foo,
=======
  /// Returns a [Future]
  /// Throws [DioException] if API call or serialization fails
  Future<Response<void>> deleteOrder({ 
    required String orderId,
>>>>>>> 95cefaeecdae21a43a453f07ed510c420abaa461:samples/openapi3/client/petstore/dart-dio/petstore_client_lib_fake-json_serializable/lib/src/api/store_api.dart
    CancelToken? cancelToken,
    Map<String, dynamic>? headers,
    Map<String, dynamic>? extra,
    ValidateStatus? validateStatus,
    ProgressCallback? onSendProgress,
    ProgressCallback? onReceiveProgress,
  }) async {
    Object? _bodyData;
    _bodyData =
        foo == null ? null : _repository.serialize(foo, const TypeInfo(Foo));

    final _response = await rawApi.createFoo(
      body: _bodyData,
      requestContentType: 'application/json;charset=utf-8',
      cancelToken: cancelToken,
      headers: headers,
      extra: extra,
      validateStatus: validateStatus,
      onSendProgress: onSendProgress,
      onReceiveProgress: onReceiveProgress,
    );

    FooRefOrValue? _responseData;

    try {
      final rawResponse = _response.data;
      _responseData = rawResponse == null
          ? null
          : _repository.deserialize(
              rawResponse,
              const TypeInfo(FooRefOrValue),
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

    return Response<FooRefOrValue>(
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

  /// GET all Foos
  ///
  ///
  /// Parameters:
  /// * [cancelToken] - A [CancelToken] that can be used to cancel the operation
  /// * [headers] - Can be used to add additional headers to the request
  /// * [extras] - Can be used to add flags to the request
  /// * [validateStatus] - A [ValidateStatus] callback that can be used to determine request success based on the HTTP status of the response
  /// * [onSendProgress] - A [ProgressCallback] that can be used to get the send progress
  /// * [onReceiveProgress] - A [ProgressCallback] that can be used to get the receive progress
  ///
<<<<<<< HEAD:samples/openapi3/client/petstore/dart-dio/dio/built_value/lib/src/api/foo_api.dart
  /// Returns a [Future] containing a [Response] with a [BuiltList<FooRefOrValue>] as data
  /// Throws [DioError] if API call or serialization fails
  Future<Response<BuiltList<FooRefOrValue>>> getAllFoos({
=======
  /// Returns a [Future] containing a [Response] with a [Map<String, int>] as data
  /// Throws [DioException] if API call or serialization fails
  Future<Response<Map<String, int>>> getInventory({ 
>>>>>>> 95cefaeecdae21a43a453f07ed510c420abaa461:samples/openapi3/client/petstore/dart-dio/petstore_client_lib_fake-json_serializable/lib/src/api/store_api.dart
    CancelToken? cancelToken,
    Map<String, dynamic>? headers,
    Map<String, dynamic>? extra,
    ValidateStatus? validateStatus,
    ProgressCallback? onSendProgress,
    ProgressCallback? onReceiveProgress,
  }) async {
    final _response = await rawApi.getAllFoos(
      cancelToken: cancelToken,
      headers: headers,
      extra: extra,
      validateStatus: validateStatus,
      onSendProgress: onSendProgress,
      onReceiveProgress: onReceiveProgress,
    );

    BuiltList<FooRefOrValue>? _responseData;

    try {
      final rawResponse = _response.data;
      _responseData = rawResponse == null
          ? null
          : _repository.deserialize(
              rawResponse,
              const TypeInfo(BuiltList, [
                const TypeInfo(FooRefOrValue),
              ]),
            );
    } catch (error, stackTrace) {
      throw DioException(
        requestOptions: _response.requestOptions,
        response: _response,
        type: DioExceptionType.unknown,
        error: error,
        stackTrace: stackTrace,
      );
    }

    return Response<BuiltList<FooRefOrValue>>(
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

class FooApiRaw {
  final Dio _dio;

  const FooApiRaw(this._dio);

  /// Create a Foo
  ///
  ///
  /// Parameters:
  /// * [foo] - The Foo to be created
  /// * [cancelToken] - A [CancelToken] that can be used to cancel the operation
  /// * [headers] - Can be used to add additional headers to the request
  /// * [extras] - Can be used to add flags to the request
  /// * [validateStatus] - A [ValidateStatus] callback that can be used to determine request success based on the HTTP status of the response
  /// * [onSendProgress] - A [ProgressCallback] that can be used to get the send progress
  /// * [onReceiveProgress] - A [ProgressCallback] that can be used to get the receive progress
  ///
<<<<<<< HEAD:samples/openapi3/client/petstore/dart-dio/dio/built_value/lib/src/api/foo_api.dart
  /// Returns a [Future] containing a [Response] with a [FooRefOrValue] as data
  /// Throws [DioError] if API call or serialization fails
  Future<Response<Object>> createFoo({
    Object? body,
    String? requestContentType,
    String? acceptContentType,
    ResponseType? responseType,
=======
  /// Returns a [Future] containing a [Response] with a [Order] as data
  /// Throws [DioException] if API call or serialization fails
  Future<Response<Order>> getOrderById({ 
    required int orderId,
>>>>>>> 95cefaeecdae21a43a453f07ed510c420abaa461:samples/openapi3/client/petstore/dart-dio/petstore_client_lib_fake-json_serializable/lib/src/api/store_api.dart
    CancelToken? cancelToken,
    Map<String, dynamic>? headers,
    Map<String, dynamic>? extra,
    ValidateStatus? validateStatus,
    ProgressCallback? onSendProgress,
    ProgressCallback? onReceiveProgress,
  }) async {
    final _path = r'/foo';
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

    return await _dio.request<Object>(
      _path,
      data: body,
      options: _options,
      cancelToken: cancelToken,
      onSendProgress: onSendProgress,
      onReceiveProgress: onReceiveProgress,
    );
<<<<<<< HEAD:samples/openapi3/client/petstore/dart-dio/dio/built_value/lib/src/api/foo_api.dart
=======

    Order? _responseData;

    try {
final rawData = _response.data;
_responseData = rawData == null ? null : deserialize<Order, Order>(rawData, 'Order', growable: true);
    } catch (error, stackTrace) {
      throw DioException(
        requestOptions: _response.requestOptions,
        response: _response,
        type: DioExceptionType.unknown,
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
>>>>>>> 95cefaeecdae21a43a453f07ed510c420abaa461:samples/openapi3/client/petstore/dart-dio/petstore_client_lib_fake-json_serializable/lib/src/api/store_api.dart
  }

  /// GET all Foos
  ///
  ///
  /// Parameters:
  /// * [cancelToken] - A [CancelToken] that can be used to cancel the operation
  /// * [headers] - Can be used to add additional headers to the request
  /// * [extras] - Can be used to add flags to the request
  /// * [validateStatus] - A [ValidateStatus] callback that can be used to determine request success based on the HTTP status of the response
  /// * [onSendProgress] - A [ProgressCallback] that can be used to get the send progress
  /// * [onReceiveProgress] - A [ProgressCallback] that can be used to get the receive progress
  ///
<<<<<<< HEAD:samples/openapi3/client/petstore/dart-dio/dio/built_value/lib/src/api/foo_api.dart
  /// Returns a [Future] containing a [Response] with a [BuiltList<FooRefOrValue>] as data
  /// Throws [DioError] if API call or serialization fails
  Future<Response<Object>> getAllFoos({
    Object? body,
    String? requestContentType,
    String? acceptContentType,
    ResponseType? responseType,
=======
  /// Returns a [Future] containing a [Response] with a [Order] as data
  /// Throws [DioException] if API call or serialization fails
  Future<Response<Order>> placeOrder({ 
    required Order order,
>>>>>>> 95cefaeecdae21a43a453f07ed510c420abaa461:samples/openapi3/client/petstore/dart-dio/petstore_client_lib_fake-json_serializable/lib/src/api/store_api.dart
    CancelToken? cancelToken,
    Map<String, dynamic>? headers,
    Map<String, dynamic>? extra,
    ValidateStatus? validateStatus,
    ProgressCallback? onSendProgress,
    ProgressCallback? onReceiveProgress,
  }) async {
    final _path = r'/foo';
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

<<<<<<< HEAD:samples/openapi3/client/petstore/dart-dio/dio/built_value/lib/src/api/foo_api.dart
    return await _dio.request<Object>(
=======
    dynamic _bodyData;

    try {
_bodyData=jsonEncode(order);
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
>>>>>>> 95cefaeecdae21a43a453f07ed510c420abaa461:samples/openapi3/client/petstore/dart-dio/petstore_client_lib_fake-json_serializable/lib/src/api/store_api.dart
      _path,
      data: body,
      options: _options,
      cancelToken: cancelToken,
      onSendProgress: onSendProgress,
      onReceiveProgress: onReceiveProgress,
    );
<<<<<<< HEAD:samples/openapi3/client/petstore/dart-dio/dio/built_value/lib/src/api/foo_api.dart
=======

    Order? _responseData;

    try {
final rawData = _response.data;
_responseData = rawData == null ? null : deserialize<Order, Order>(rawData, 'Order', growable: true);
    } catch (error, stackTrace) {
      throw DioException(
        requestOptions: _response.requestOptions,
        response: _response,
        type: DioExceptionType.unknown,
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
>>>>>>> 95cefaeecdae21a43a453f07ed510c420abaa461:samples/openapi3/client/petstore/dart-dio/petstore_client_lib_fake-json_serializable/lib/src/api/store_api.dart
  }
}

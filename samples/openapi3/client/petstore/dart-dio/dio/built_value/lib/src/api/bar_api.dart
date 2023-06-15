//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

import 'dart:async';
import 'package:dio/dio.dart';
import 'package:openapi/src/repository_base.dart';
import 'package:openapi/src/api_util.dart';
import 'package:openapi/models.dart';
import 'package:openapi/src/model/bar.dart';
import 'package:openapi/src/model/bar_create.dart';

class BarApi {
  final BarApiRaw rawApi;
  final SerializationRepositoryBase _repository;

  const BarApi(this.rawApi, this._repository);

  /// Create a Bar
  ///
  ///
  /// Parameters:
  /// * [barCreate]
  /// * [cancelToken] - A [CancelToken] that can be used to cancel the operation
  /// * [headers] - Can be used to add additional headers to the request
  /// * [extras] - Can be used to add flags to the request
  /// * [validateStatus] - A [ValidateStatus] callback that can be used to determine request success based on the HTTP status of the response
  /// * [onSendProgress] - A [ProgressCallback] that can be used to get the send progress
  /// * [onReceiveProgress] - A [ProgressCallback] that can be used to get the receive progress
  ///
<<<<<<< HEAD:samples/openapi3/client/petstore/dart-dio/dio/built_value/lib/src/api/bar_api.dart
  /// Returns a [Future] containing a [Response] with a [Bar] as data
  /// Throws [DioError] if API call or serialization fails
  Future<Response<Bar>> createBar({
    required BarCreate barCreate,
=======
  /// Returns a [Future] containing a [Response] with a [FooRefOrValue] as data
  /// Throws [DioException] if API call or serialization fails
  Future<Response<FooRefOrValue>> createFoo({ 
    Foo? foo,
>>>>>>> 95cefaeecdae21a43a453f07ed510c420abaa461:samples/openapi3/client/petstore/dart-dio/oneof_polymorphism_and_inheritance/lib/src/api/foo_api.dart
    CancelToken? cancelToken,
    Map<String, dynamic>? headers,
    Map<String, dynamic>? extra,
    ValidateStatus? validateStatus,
    ProgressCallback? onSendProgress,
    ProgressCallback? onReceiveProgress,
  }) async {
    Object? _bodyData;
    _bodyData = _repository.serialize(barCreate, const TypeInfo(BarCreate));

<<<<<<< HEAD:samples/openapi3/client/petstore/dart-dio/dio/built_value/lib/src/api/bar_api.dart
    final _response = await rawApi.createBar(
      body: _bodyData,
      requestContentType: 'application/json',
=======
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
>>>>>>> 95cefaeecdae21a43a453f07ed510c420abaa461:samples/openapi3/client/petstore/dart-dio/oneof_polymorphism_and_inheritance/lib/src/api/foo_api.dart
      cancelToken: cancelToken,
      headers: headers,
      extra: extra,
      validateStatus: validateStatus,
      onSendProgress: onSendProgress,
      onReceiveProgress: onReceiveProgress,
    );

    Bar? _responseData;

    try {
      final rawResponse = _response.data;
      _responseData = rawResponse == null
          ? null
          : _repository.deserialize(
              rawResponse,
              const TypeInfo(Bar),
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

    return Response<Bar>(
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

class BarApiRaw {
  final Dio _dio;

  const BarApiRaw(this._dio);

  /// Create a Bar
  ///
  ///
  /// Parameters:
  /// * [barCreate]
  /// * [cancelToken] - A [CancelToken] that can be used to cancel the operation
  /// * [headers] - Can be used to add additional headers to the request
  /// * [extras] - Can be used to add flags to the request
  /// * [validateStatus] - A [ValidateStatus] callback that can be used to determine request success based on the HTTP status of the response
  /// * [onSendProgress] - A [ProgressCallback] that can be used to get the send progress
  /// * [onReceiveProgress] - A [ProgressCallback] that can be used to get the receive progress
  ///
<<<<<<< HEAD:samples/openapi3/client/petstore/dart-dio/dio/built_value/lib/src/api/bar_api.dart
  /// Returns a [Future] containing a [Response] with a [Bar] as data
  /// Throws [DioError] if API call or serialization fails
  Future<Response<Object>> createBar({
    Object? body,
    String? requestContentType,
    String? acceptContentType,
    ResponseType? responseType,
=======
  /// Returns a [Future] containing a [Response] with a [BuiltList<FooRefOrValue>] as data
  /// Throws [DioException] if API call or serialization fails
  Future<Response<BuiltList<FooRefOrValue>>> getAllFoos({ 
>>>>>>> 95cefaeecdae21a43a453f07ed510c420abaa461:samples/openapi3/client/petstore/dart-dio/oneof_polymorphism_and_inheritance/lib/src/api/foo_api.dart
    CancelToken? cancelToken,
    Map<String, dynamic>? headers,
    Map<String, dynamic>? extra,
    ValidateStatus? validateStatus,
    ProgressCallback? onSendProgress,
    ProgressCallback? onReceiveProgress,
  }) async {
    final _path = r'/bar';
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
<<<<<<< HEAD:samples/openapi3/client/petstore/dart-dio/dio/built_value/lib/src/api/bar_api.dart
=======

    BuiltList<FooRefOrValue>? _responseData;

    try {
      final rawResponse = _response.data;
      _responseData = rawResponse == null ? null : _serializers.deserialize(
        rawResponse,
        specifiedType: const FullType(BuiltList, [FullType(FooRefOrValue)]),
      ) as BuiltList<FooRefOrValue>;

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
>>>>>>> 95cefaeecdae21a43a453f07ed510c420abaa461:samples/openapi3/client/petstore/dart-dio/oneof_polymorphism_and_inheritance/lib/src/api/foo_api.dart
  }
}

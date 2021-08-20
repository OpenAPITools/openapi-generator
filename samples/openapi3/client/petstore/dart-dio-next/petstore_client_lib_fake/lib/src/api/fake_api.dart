//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

import 'dart:async';

import 'package:built_value/serializer.dart';
import 'package:dio/dio.dart';

import 'package:built_collection/built_collection.dart';
import 'package:built_value/built_value.dart';
import 'package:openapi/src/api_util.dart';
import 'package:openapi/src/model/date.dart';
import 'package:openapi/src/model/file_schema_test_class.dart';
import 'package:openapi/src/model/health_check_result.dart';
import 'package:openapi/src/model/model_client.dart';
import 'package:openapi/src/model/outer_composite.dart';
import 'package:openapi/src/model/outer_object_with_enum_property.dart';
import 'package:openapi/src/model/pet.dart';
import 'package:openapi/src/model/user.dart';
part 'fake_api.g.dart';

class FakeApi {

  final Dio _dio;

  final Serializers _serializers;

  const FakeApi(this._dio, this._serializers);

  /// Health check endpoint
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
  /// Returns a [Future] containing a [Response] with a [HealthCheckResult] as data
  /// Throws [DioError] if API call or serialization fails
  Future<Response<HealthCheckResult>> fakeHealthGet({ 
    CancelToken? cancelToken,
    Map<String, dynamic>? headers,
    Map<String, dynamic>? extra,
    ValidateStatus? validateStatus,
    ProgressCallback? onSendProgress,
    ProgressCallback? onReceiveProgress,
  }) async {
    final _path = r'/fake/health';
    final _options = Options(
      method: r'GET',
      headers: <String, dynamic>{
        ...?headers,
      },
      extra: <String, dynamic>{
        'secure': <Map<String, String>>[],
        ...?extra,
      },
      validateStatus: validateStatus,
    );

    final _response = await _dio.request<Object>(
      _path,
      options: _options,
      cancelToken: cancelToken,
      onSendProgress: onSendProgress,
      onReceiveProgress: onReceiveProgress,
    );

    HealthCheckResult _responseData;

    try {
      const _responseType = FullType(HealthCheckResult);
      _responseData = _serializers.deserialize(
        _response.data!,
        specifiedType: _responseType,
      ) as HealthCheckResult;

    } catch (error, stackTrace) {
      throw DioError(
        requestOptions: _response.requestOptions,
        response: _response,
        type: DioErrorType.other,
        error: error,
      )..stackTrace = stackTrace;
    }

    return Response<HealthCheckResult>(
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

  /// test http signature authentication
  /// 
  ///
  /// Parameters:
  /// * [pet] - Pet object that needs to be added to the store
  /// * [query1] - query parameter
  /// * [header1] - header parameter
  /// * [cancelToken] - A [CancelToken] that can be used to cancel the operation
  /// * [headers] - Can be used to add additional headers to the request
  /// * [extras] - Can be used to add flags to the request
  /// * [validateStatus] - A [ValidateStatus] callback that can be used to determine request success based on the HTTP status of the response
  /// * [onSendProgress] - A [ProgressCallback] that can be used to get the send progress
  /// * [onReceiveProgress] - A [ProgressCallback] that can be used to get the receive progress
  ///
  /// Returns a [Future]
  /// Throws [DioError] if API call or serialization fails
  Future<Response<void>> fakeHttpSignatureTest({ 
    required Pet pet,
    String? query1,
    String? header1,
    CancelToken? cancelToken,
    Map<String, dynamic>? headers,
    Map<String, dynamic>? extra,
    ValidateStatus? validateStatus,
    ProgressCallback? onSendProgress,
    ProgressCallback? onReceiveProgress,
  }) async {
    final _path = r'/fake/http-signature-test';
    final _options = Options(
      method: r'GET',
      headers: <String, dynamic>{
        if (header1 != null) r'header_1': header1,
        ...?headers,
      },
      extra: <String, dynamic>{
        'secure': <Map<String, String>>[
          {
            'type': 'http',
            'scheme': 'signature',
            'name': 'http_signature_test',
          },
        ],
        ...?extra,
      },
      contentType: 'application/json',
      validateStatus: validateStatus,
    );

    final _queryParameters = <String, dynamic>{
      if (query1 != null) r'query_1': encodeQueryParameter(_serializers, query1, const FullType(String)),
    };

    dynamic _bodyData;

    try {
      const _type = FullType(Pet);
      _bodyData = _serializers.serialize(pet, specifiedType: _type);

    } catch(error, stackTrace) {
      throw DioError(
         requestOptions: _options.compose(
          _dio.options,
          _path,
          queryParameters: _queryParameters,
        ),
        type: DioErrorType.other,
        error: error,
      )..stackTrace = stackTrace;
    }

    final _response = await _dio.request<Object>(
      _path,
      data: _bodyData,
      options: _options,
      queryParameters: _queryParameters,
      cancelToken: cancelToken,
      onSendProgress: onSendProgress,
      onReceiveProgress: onReceiveProgress,
    );

    return _response;
  }

  /// fakeOuterBooleanSerialize
  /// Test serialization of outer boolean types
  ///
  /// Parameters:
  /// * [body] - Input boolean as post body
  /// * [cancelToken] - A [CancelToken] that can be used to cancel the operation
  /// * [headers] - Can be used to add additional headers to the request
  /// * [extras] - Can be used to add flags to the request
  /// * [validateStatus] - A [ValidateStatus] callback that can be used to determine request success based on the HTTP status of the response
  /// * [onSendProgress] - A [ProgressCallback] that can be used to get the send progress
  /// * [onReceiveProgress] - A [ProgressCallback] that can be used to get the receive progress
  ///
  /// Returns a [Future] containing a [Response] with a [bool] as data
  /// Throws [DioError] if API call or serialization fails
  Future<Response<bool>> fakeOuterBooleanSerialize({ 
    bool? body,
    CancelToken? cancelToken,
    Map<String, dynamic>? headers,
    Map<String, dynamic>? extra,
    ValidateStatus? validateStatus,
    ProgressCallback? onSendProgress,
    ProgressCallback? onReceiveProgress,
  }) async {
    final _path = r'/fake/outer/boolean';
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
      _bodyData = body;

    } catch(error, stackTrace) {
      throw DioError(
         requestOptions: _options.compose(
          _dio.options,
          _path,
        ),
        type: DioErrorType.other,
        error: error,
      )..stackTrace = stackTrace;
    }

    final _response = await _dio.request<Object>(
      _path,
      data: _bodyData,
      options: _options,
      cancelToken: cancelToken,
      onSendProgress: onSendProgress,
      onReceiveProgress: onReceiveProgress,
    );

    bool _responseData;

    try {
      _responseData = _response.data as bool;

    } catch (error, stackTrace) {
      throw DioError(
        requestOptions: _response.requestOptions,
        response: _response,
        type: DioErrorType.other,
        error: error,
      )..stackTrace = stackTrace;
    }

    return Response<bool>(
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

  /// fakeOuterCompositeSerialize
  /// Test serialization of object with outer number type
  ///
  /// Parameters:
  /// * [outerComposite] - Input composite as post body
  /// * [cancelToken] - A [CancelToken] that can be used to cancel the operation
  /// * [headers] - Can be used to add additional headers to the request
  /// * [extras] - Can be used to add flags to the request
  /// * [validateStatus] - A [ValidateStatus] callback that can be used to determine request success based on the HTTP status of the response
  /// * [onSendProgress] - A [ProgressCallback] that can be used to get the send progress
  /// * [onReceiveProgress] - A [ProgressCallback] that can be used to get the receive progress
  ///
  /// Returns a [Future] containing a [Response] with a [OuterComposite] as data
  /// Throws [DioError] if API call or serialization fails
  Future<Response<OuterComposite>> fakeOuterCompositeSerialize({ 
    OuterComposite? outerComposite,
    CancelToken? cancelToken,
    Map<String, dynamic>? headers,
    Map<String, dynamic>? extra,
    ValidateStatus? validateStatus,
    ProgressCallback? onSendProgress,
    ProgressCallback? onReceiveProgress,
  }) async {
    final _path = r'/fake/outer/composite';
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
      const _type = FullType(OuterComposite);
      _bodyData = outerComposite == null ? null : _serializers.serialize(outerComposite, specifiedType: _type);

    } catch(error, stackTrace) {
      throw DioError(
         requestOptions: _options.compose(
          _dio.options,
          _path,
        ),
        type: DioErrorType.other,
        error: error,
      )..stackTrace = stackTrace;
    }

    final _response = await _dio.request<Object>(
      _path,
      data: _bodyData,
      options: _options,
      cancelToken: cancelToken,
      onSendProgress: onSendProgress,
      onReceiveProgress: onReceiveProgress,
    );

    OuterComposite _responseData;

    try {
      const _responseType = FullType(OuterComposite);
      _responseData = _serializers.deserialize(
        _response.data!,
        specifiedType: _responseType,
      ) as OuterComposite;

    } catch (error, stackTrace) {
      throw DioError(
        requestOptions: _response.requestOptions,
        response: _response,
        type: DioErrorType.other,
        error: error,
      )..stackTrace = stackTrace;
    }

    return Response<OuterComposite>(
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

  /// fakeOuterNumberSerialize
  /// Test serialization of outer number types
  ///
  /// Parameters:
  /// * [body] - Input number as post body
  /// * [cancelToken] - A [CancelToken] that can be used to cancel the operation
  /// * [headers] - Can be used to add additional headers to the request
  /// * [extras] - Can be used to add flags to the request
  /// * [validateStatus] - A [ValidateStatus] callback that can be used to determine request success based on the HTTP status of the response
  /// * [onSendProgress] - A [ProgressCallback] that can be used to get the send progress
  /// * [onReceiveProgress] - A [ProgressCallback] that can be used to get the receive progress
  ///
  /// Returns a [Future] containing a [Response] with a [num] as data
  /// Throws [DioError] if API call or serialization fails
  Future<Response<num>> fakeOuterNumberSerialize({ 
    num? body,
    CancelToken? cancelToken,
    Map<String, dynamic>? headers,
    Map<String, dynamic>? extra,
    ValidateStatus? validateStatus,
    ProgressCallback? onSendProgress,
    ProgressCallback? onReceiveProgress,
  }) async {
    final _path = r'/fake/outer/number';
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
      _bodyData = body;

    } catch(error, stackTrace) {
      throw DioError(
         requestOptions: _options.compose(
          _dio.options,
          _path,
        ),
        type: DioErrorType.other,
        error: error,
      )..stackTrace = stackTrace;
    }

    final _response = await _dio.request<Object>(
      _path,
      data: _bodyData,
      options: _options,
      cancelToken: cancelToken,
      onSendProgress: onSendProgress,
      onReceiveProgress: onReceiveProgress,
    );

    num _responseData;

    try {
      _responseData = _response.data as num;

    } catch (error, stackTrace) {
      throw DioError(
        requestOptions: _response.requestOptions,
        response: _response,
        type: DioErrorType.other,
        error: error,
      )..stackTrace = stackTrace;
    }

    return Response<num>(
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

  /// fakeOuterStringSerialize
  /// Test serialization of outer string types
  ///
  /// Parameters:
  /// * [body] - Input string as post body
  /// * [cancelToken] - A [CancelToken] that can be used to cancel the operation
  /// * [headers] - Can be used to add additional headers to the request
  /// * [extras] - Can be used to add flags to the request
  /// * [validateStatus] - A [ValidateStatus] callback that can be used to determine request success based on the HTTP status of the response
  /// * [onSendProgress] - A [ProgressCallback] that can be used to get the send progress
  /// * [onReceiveProgress] - A [ProgressCallback] that can be used to get the receive progress
  ///
  /// Returns a [Future] containing a [Response] with a [String] as data
  /// Throws [DioError] if API call or serialization fails
  Future<Response<String>> fakeOuterStringSerialize({ 
    String? body,
    CancelToken? cancelToken,
    Map<String, dynamic>? headers,
    Map<String, dynamic>? extra,
    ValidateStatus? validateStatus,
    ProgressCallback? onSendProgress,
    ProgressCallback? onReceiveProgress,
  }) async {
    final _path = r'/fake/outer/string';
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
      _bodyData = body;

    } catch(error, stackTrace) {
      throw DioError(
         requestOptions: _options.compose(
          _dio.options,
          _path,
        ),
        type: DioErrorType.other,
        error: error,
      )..stackTrace = stackTrace;
    }

    final _response = await _dio.request<Object>(
      _path,
      data: _bodyData,
      options: _options,
      cancelToken: cancelToken,
      onSendProgress: onSendProgress,
      onReceiveProgress: onReceiveProgress,
    );

    String _responseData;

    try {
      _responseData = _response.data as String;

    } catch (error, stackTrace) {
      throw DioError(
        requestOptions: _response.requestOptions,
        response: _response,
        type: DioErrorType.other,
        error: error,
      )..stackTrace = stackTrace;
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
  }

  /// fakePropertyEnumIntegerSerialize
  /// Test serialization of enum (int) properties with examples
  ///
  /// Parameters:
  /// * [outerObjectWithEnumProperty] - Input enum (int) as post body
  /// * [cancelToken] - A [CancelToken] that can be used to cancel the operation
  /// * [headers] - Can be used to add additional headers to the request
  /// * [extras] - Can be used to add flags to the request
  /// * [validateStatus] - A [ValidateStatus] callback that can be used to determine request success based on the HTTP status of the response
  /// * [onSendProgress] - A [ProgressCallback] that can be used to get the send progress
  /// * [onReceiveProgress] - A [ProgressCallback] that can be used to get the receive progress
  ///
  /// Returns a [Future] containing a [Response] with a [OuterObjectWithEnumProperty] as data
  /// Throws [DioError] if API call or serialization fails
  Future<Response<OuterObjectWithEnumProperty>> fakePropertyEnumIntegerSerialize({ 
    required OuterObjectWithEnumProperty outerObjectWithEnumProperty,
    CancelToken? cancelToken,
    Map<String, dynamic>? headers,
    Map<String, dynamic>? extra,
    ValidateStatus? validateStatus,
    ProgressCallback? onSendProgress,
    ProgressCallback? onReceiveProgress,
  }) async {
    final _path = r'/fake/property/enum-int';
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
      const _type = FullType(OuterObjectWithEnumProperty);
      _bodyData = _serializers.serialize(outerObjectWithEnumProperty, specifiedType: _type);

    } catch(error, stackTrace) {
      throw DioError(
         requestOptions: _options.compose(
          _dio.options,
          _path,
        ),
        type: DioErrorType.other,
        error: error,
      )..stackTrace = stackTrace;
    }

    final _response = await _dio.request<Object>(
      _path,
      data: _bodyData,
      options: _options,
      cancelToken: cancelToken,
      onSendProgress: onSendProgress,
      onReceiveProgress: onReceiveProgress,
    );

    OuterObjectWithEnumProperty _responseData;

    try {
      const _responseType = FullType(OuterObjectWithEnumProperty);
      _responseData = _serializers.deserialize(
        _response.data!,
        specifiedType: _responseType,
      ) as OuterObjectWithEnumProperty;

    } catch (error, stackTrace) {
      throw DioError(
        requestOptions: _response.requestOptions,
        response: _response,
        type: DioErrorType.other,
        error: error,
      )..stackTrace = stackTrace;
    }

    return Response<OuterObjectWithEnumProperty>(
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

  /// testBodyWithBinary
  /// For this test, the body has to be a binary file.
  ///
  /// Parameters:
  /// * [body] - image to upload
  /// * [cancelToken] - A [CancelToken] that can be used to cancel the operation
  /// * [headers] - Can be used to add additional headers to the request
  /// * [extras] - Can be used to add flags to the request
  /// * [validateStatus] - A [ValidateStatus] callback that can be used to determine request success based on the HTTP status of the response
  /// * [onSendProgress] - A [ProgressCallback] that can be used to get the send progress
  /// * [onReceiveProgress] - A [ProgressCallback] that can be used to get the receive progress
  ///
  /// Returns a [Future]
  /// Throws [DioError] if API call or serialization fails
  Future<Response<void>> testBodyWithBinary({ 
    MultipartFile? body,
    CancelToken? cancelToken,
    Map<String, dynamic>? headers,
    Map<String, dynamic>? extra,
    ValidateStatus? validateStatus,
    ProgressCallback? onSendProgress,
    ProgressCallback? onReceiveProgress,
  }) async {
    final _path = r'/fake/body-with-binary';
    final _options = Options(
      method: r'PUT',
      headers: <String, dynamic>{
        ...?headers,
      },
      extra: <String, dynamic>{
        'secure': <Map<String, String>>[],
        ...?extra,
      },
      contentType: 'image/png',
      validateStatus: validateStatus,
    );

    dynamic _bodyData;

    try {
      _bodyData = body?.finalize();

    } catch(error, stackTrace) {
      throw DioError(
         requestOptions: _options.compose(
          _dio.options,
          _path,
        ),
        type: DioErrorType.other,
        error: error,
      )..stackTrace = stackTrace;
    }

    final _response = await _dio.request<Object>(
      _path,
      data: _bodyData,
      options: _options,
      cancelToken: cancelToken,
      onSendProgress: onSendProgress,
      onReceiveProgress: onReceiveProgress,
    );

    return _response;
  }

  /// testBodyWithFileSchema
  /// For this test, the body for this request must reference a schema named &#x60;File&#x60;.
  ///
  /// Parameters:
  /// * [fileSchemaTestClass] 
  /// * [cancelToken] - A [CancelToken] that can be used to cancel the operation
  /// * [headers] - Can be used to add additional headers to the request
  /// * [extras] - Can be used to add flags to the request
  /// * [validateStatus] - A [ValidateStatus] callback that can be used to determine request success based on the HTTP status of the response
  /// * [onSendProgress] - A [ProgressCallback] that can be used to get the send progress
  /// * [onReceiveProgress] - A [ProgressCallback] that can be used to get the receive progress
  ///
  /// Returns a [Future]
  /// Throws [DioError] if API call or serialization fails
  Future<Response<void>> testBodyWithFileSchema({ 
    required FileSchemaTestClass fileSchemaTestClass,
    CancelToken? cancelToken,
    Map<String, dynamic>? headers,
    Map<String, dynamic>? extra,
    ValidateStatus? validateStatus,
    ProgressCallback? onSendProgress,
    ProgressCallback? onReceiveProgress,
  }) async {
    final _path = r'/fake/body-with-file-schema';
    final _options = Options(
      method: r'PUT',
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
      const _type = FullType(FileSchemaTestClass);
      _bodyData = _serializers.serialize(fileSchemaTestClass, specifiedType: _type);

    } catch(error, stackTrace) {
      throw DioError(
         requestOptions: _options.compose(
          _dio.options,
          _path,
        ),
        type: DioErrorType.other,
        error: error,
      )..stackTrace = stackTrace;
    }

    final _response = await _dio.request<Object>(
      _path,
      data: _bodyData,
      options: _options,
      cancelToken: cancelToken,
      onSendProgress: onSendProgress,
      onReceiveProgress: onReceiveProgress,
    );

    return _response;
  }

  /// testBodyWithQueryParams
  /// 
  ///
  /// Parameters:
  /// * [query] 
  /// * [user] 
  /// * [cancelToken] - A [CancelToken] that can be used to cancel the operation
  /// * [headers] - Can be used to add additional headers to the request
  /// * [extras] - Can be used to add flags to the request
  /// * [validateStatus] - A [ValidateStatus] callback that can be used to determine request success based on the HTTP status of the response
  /// * [onSendProgress] - A [ProgressCallback] that can be used to get the send progress
  /// * [onReceiveProgress] - A [ProgressCallback] that can be used to get the receive progress
  ///
  /// Returns a [Future]
  /// Throws [DioError] if API call or serialization fails
  Future<Response<void>> testBodyWithQueryParams({ 
    required String query,
    required User user,
    CancelToken? cancelToken,
    Map<String, dynamic>? headers,
    Map<String, dynamic>? extra,
    ValidateStatus? validateStatus,
    ProgressCallback? onSendProgress,
    ProgressCallback? onReceiveProgress,
  }) async {
    final _path = r'/fake/body-with-query-params';
    final _options = Options(
      method: r'PUT',
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

    final _queryParameters = <String, dynamic>{
      r'query': encodeQueryParameter(_serializers, query, const FullType(String)),
    };

    dynamic _bodyData;

    try {
      const _type = FullType(User);
      _bodyData = _serializers.serialize(user, specifiedType: _type);

    } catch(error, stackTrace) {
      throw DioError(
         requestOptions: _options.compose(
          _dio.options,
          _path,
          queryParameters: _queryParameters,
        ),
        type: DioErrorType.other,
        error: error,
      )..stackTrace = stackTrace;
    }

    final _response = await _dio.request<Object>(
      _path,
      data: _bodyData,
      options: _options,
      queryParameters: _queryParameters,
      cancelToken: cancelToken,
      onSendProgress: onSendProgress,
      onReceiveProgress: onReceiveProgress,
    );

    return _response;
  }

  /// To test \&quot;client\&quot; model
  /// To test \&quot;client\&quot; model
  ///
  /// Parameters:
  /// * [modelClient] - client model
  /// * [cancelToken] - A [CancelToken] that can be used to cancel the operation
  /// * [headers] - Can be used to add additional headers to the request
  /// * [extras] - Can be used to add flags to the request
  /// * [validateStatus] - A [ValidateStatus] callback that can be used to determine request success based on the HTTP status of the response
  /// * [onSendProgress] - A [ProgressCallback] that can be used to get the send progress
  /// * [onReceiveProgress] - A [ProgressCallback] that can be used to get the receive progress
  ///
  /// Returns a [Future] containing a [Response] with a [ModelClient] as data
  /// Throws [DioError] if API call or serialization fails
  Future<Response<ModelClient>> testClientModel({ 
    required ModelClient modelClient,
    CancelToken? cancelToken,
    Map<String, dynamic>? headers,
    Map<String, dynamic>? extra,
    ValidateStatus? validateStatus,
    ProgressCallback? onSendProgress,
    ProgressCallback? onReceiveProgress,
  }) async {
    final _path = r'/fake';
    final _options = Options(
      method: r'PATCH',
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
      const _type = FullType(ModelClient);
      _bodyData = _serializers.serialize(modelClient, specifiedType: _type);

    } catch(error, stackTrace) {
      throw DioError(
         requestOptions: _options.compose(
          _dio.options,
          _path,
        ),
        type: DioErrorType.other,
        error: error,
      )..stackTrace = stackTrace;
    }

    final _response = await _dio.request<Object>(
      _path,
      data: _bodyData,
      options: _options,
      cancelToken: cancelToken,
      onSendProgress: onSendProgress,
      onReceiveProgress: onReceiveProgress,
    );

    ModelClient _responseData;

    try {
      const _responseType = FullType(ModelClient);
      _responseData = _serializers.deserialize(
        _response.data!,
        specifiedType: _responseType,
      ) as ModelClient;

    } catch (error, stackTrace) {
      throw DioError(
        requestOptions: _response.requestOptions,
        response: _response,
        type: DioErrorType.other,
        error: error,
      )..stackTrace = stackTrace;
    }

    return Response<ModelClient>(
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

  /// Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
  /// Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
  ///
  /// Parameters:
  /// * [number] - None
  /// * [double_] - None
  /// * [patternWithoutDelimiter] - None
  /// * [byte] - None
  /// * [integer] - None
  /// * [int32] - None
  /// * [int64] - None
  /// * [float] - None
  /// * [string] - None
  /// * [binary] - None
  /// * [date] - None
  /// * [dateTime] - None
  /// * [password] - None
  /// * [callback] - None
  /// * [cancelToken] - A [CancelToken] that can be used to cancel the operation
  /// * [headers] - Can be used to add additional headers to the request
  /// * [extras] - Can be used to add flags to the request
  /// * [validateStatus] - A [ValidateStatus] callback that can be used to determine request success based on the HTTP status of the response
  /// * [onSendProgress] - A [ProgressCallback] that can be used to get the send progress
  /// * [onReceiveProgress] - A [ProgressCallback] that can be used to get the receive progress
  ///
  /// Returns a [Future]
  /// Throws [DioError] if API call or serialization fails
  Future<Response<void>> testEndpointParameters({ 
    required num number,
    required double double_,
    required String patternWithoutDelimiter,
    required String byte,
    int? integer,
    int? int32,
    int? int64,
    double? float,
    String? string,
    MultipartFile? binary,
    Date? date,
    DateTime? dateTime,
    String? password,
    String? callback,
    CancelToken? cancelToken,
    Map<String, dynamic>? headers,
    Map<String, dynamic>? extra,
    ValidateStatus? validateStatus,
    ProgressCallback? onSendProgress,
    ProgressCallback? onReceiveProgress,
  }) async {
    final _path = r'/fake';
    final _options = Options(
      method: r'POST',
      headers: <String, dynamic>{
        ...?headers,
      },
      extra: <String, dynamic>{
        'secure': <Map<String, String>>[
          {
            'type': 'http',
            'scheme': 'basic',
            'name': 'http_basic_test',
          },
        ],
        ...?extra,
      },
      contentType: 'application/x-www-form-urlencoded',
      validateStatus: validateStatus,
    );

    dynamic _bodyData;

    try {
      _bodyData = <String, dynamic>{
        if (integer != null) r'integer': encodeQueryParameter(_serializers, integer, const FullType(int)),
        if (int32 != null) r'int32': encodeQueryParameter(_serializers, int32, const FullType(int)),
        if (int64 != null) r'int64': encodeQueryParameter(_serializers, int64, const FullType(int)),
        r'number': encodeQueryParameter(_serializers, number, const FullType(num)),
        if (float != null) r'float': encodeQueryParameter(_serializers, float, const FullType(double)),
        r'double': encodeQueryParameter(_serializers, double_, const FullType(double)),
        if (string != null) r'string': encodeQueryParameter(_serializers, string, const FullType(String)),
        r'pattern_without_delimiter': encodeQueryParameter(_serializers, patternWithoutDelimiter, const FullType(String)),
        r'byte': encodeQueryParameter(_serializers, byte, const FullType(String)),
        if (binary != null) r'binary': encodeQueryParameter(_serializers, binary, const FullType(MultipartFile)),
        if (date != null) r'date': encodeQueryParameter(_serializers, date, const FullType(Date)),
        if (dateTime != null) r'dateTime': encodeQueryParameter(_serializers, dateTime, const FullType(DateTime)),
        if (password != null) r'password': encodeQueryParameter(_serializers, password, const FullType(String)),
        if (callback != null) r'callback': encodeQueryParameter(_serializers, callback, const FullType(String)),
      };

    } catch(error, stackTrace) {
      throw DioError(
         requestOptions: _options.compose(
          _dio.options,
          _path,
        ),
        type: DioErrorType.other,
        error: error,
      )..stackTrace = stackTrace;
    }

    final _response = await _dio.request<Object>(
      _path,
      data: _bodyData,
      options: _options,
      cancelToken: cancelToken,
      onSendProgress: onSendProgress,
      onReceiveProgress: onReceiveProgress,
    );

    return _response;
  }

  /// To test enum parameters
  /// To test enum parameters
  ///
  /// Parameters:
  /// * [enumHeaderStringArray] - Header parameter enum test (string array)
  /// * [enumHeaderString] - Header parameter enum test (string)
  /// * [enumQueryStringArray] - Query parameter enum test (string array)
  /// * [enumQueryString] - Query parameter enum test (string)
  /// * [enumQueryInteger] - Query parameter enum test (double)
  /// * [enumQueryDouble] - Query parameter enum test (double)
  /// * [enumFormStringArray] - Form parameter enum test (string array)
  /// * [enumFormString] - Form parameter enum test (string)
  /// * [cancelToken] - A [CancelToken] that can be used to cancel the operation
  /// * [headers] - Can be used to add additional headers to the request
  /// * [extras] - Can be used to add flags to the request
  /// * [validateStatus] - A [ValidateStatus] callback that can be used to determine request success based on the HTTP status of the response
  /// * [onSendProgress] - A [ProgressCallback] that can be used to get the send progress
  /// * [onReceiveProgress] - A [ProgressCallback] that can be used to get the receive progress
  ///
  /// Returns a [Future]
  /// Throws [DioError] if API call or serialization fails
  Future<Response<void>> testEnumParameters({ 
    BuiltList<EnumHeaderStringArrayEnum>? enumHeaderStringArray,
    EnumHeaderStringEnum? enumHeaderString = EnumHeaderStringEnum.efg,
    BuiltList<EnumQueryStringArrayEnum>? enumQueryStringArray,
    EnumQueryStringEnum? enumQueryString = EnumQueryStringEnum.efg,
    EnumQueryIntegerEnum? enumQueryInteger,
    EnumQueryDoubleEnum? enumQueryDouble,
    BuiltList<InnerEnum>? enumFormStringArray,
    EnumFormStringEnum? enumFormString,
    CancelToken? cancelToken,
    Map<String, dynamic>? headers,
    Map<String, dynamic>? extra,
    ValidateStatus? validateStatus,
    ProgressCallback? onSendProgress,
    ProgressCallback? onReceiveProgress,
  }) async {
    final _path = r'/fake';
    final _options = Options(
      method: r'GET',
      headers: <String, dynamic>{
        if (enumHeaderStringArray != null) r'enum_header_string_array': enumHeaderStringArray,
        if (enumHeaderString != null) r'enum_header_string': enumHeaderString,
        ...?headers,
      },
      extra: <String, dynamic>{
        'secure': <Map<String, String>>[],
        ...?extra,
      },
      contentType: 'application/x-www-form-urlencoded',
      validateStatus: validateStatus,
    );

    final _queryParameters = <String, dynamic>{
      if (enumQueryStringArray != null) r'enum_query_string_array': encodeCollectionQueryParameter<EnumQueryStringArrayEnum>(_serializers, enumQueryStringArray, const FullType(BuiltList, [FullType(EnumQueryStringArrayEnum)]), format: ListFormat.multi,),
      if (enumQueryString != null) r'enum_query_string': encodeQueryParameter(_serializers, enumQueryString, const FullType(EnumQueryStringEnum)),
      if (enumQueryInteger != null) r'enum_query_integer': encodeQueryParameter(_serializers, enumQueryInteger, const FullType(EnumQueryIntegerEnum)),
      if (enumQueryDouble != null) r'enum_query_double': encodeQueryParameter(_serializers, enumQueryDouble, const FullType(EnumQueryDoubleEnum)),
    };

    dynamic _bodyData;

    try {
      _bodyData = <String, dynamic>{
        if (enumFormStringArray != null) r'enum_form_string_array': encodeCollectionQueryParameter<InnerEnum>(_serializers, enumFormStringArray, const FullType(BuiltList, [FullType(InnerEnum)]), format: ListFormat.csv,),
        if (enumFormString != null) r'enum_form_string': encodeQueryParameter(_serializers, enumFormString, const FullType(EnumFormStringEnum)),
      };

    } catch(error, stackTrace) {
      throw DioError(
         requestOptions: _options.compose(
          _dio.options,
          _path,
          queryParameters: _queryParameters,
        ),
        type: DioErrorType.other,
        error: error,
      )..stackTrace = stackTrace;
    }

    final _response = await _dio.request<Object>(
      _path,
      data: _bodyData,
      options: _options,
      queryParameters: _queryParameters,
      cancelToken: cancelToken,
      onSendProgress: onSendProgress,
      onReceiveProgress: onReceiveProgress,
    );

    return _response;
  }

  /// Fake endpoint to test group parameters (optional)
  /// Fake endpoint to test group parameters (optional)
  ///
  /// Parameters:
  /// * [requiredStringGroup] - Required String in group parameters
  /// * [requiredBooleanGroup] - Required Boolean in group parameters
  /// * [requiredInt64Group] - Required Integer in group parameters
  /// * [stringGroup] - String in group parameters
  /// * [booleanGroup] - Boolean in group parameters
  /// * [int64Group] - Integer in group parameters
  /// * [cancelToken] - A [CancelToken] that can be used to cancel the operation
  /// * [headers] - Can be used to add additional headers to the request
  /// * [extras] - Can be used to add flags to the request
  /// * [validateStatus] - A [ValidateStatus] callback that can be used to determine request success based on the HTTP status of the response
  /// * [onSendProgress] - A [ProgressCallback] that can be used to get the send progress
  /// * [onReceiveProgress] - A [ProgressCallback] that can be used to get the receive progress
  ///
  /// Returns a [Future]
  /// Throws [DioError] if API call or serialization fails
  Future<Response<void>> testGroupParameters({ 
    required int requiredStringGroup,
    required bool requiredBooleanGroup,
    required int requiredInt64Group,
    int? stringGroup,
    bool? booleanGroup,
    int? int64Group,
    CancelToken? cancelToken,
    Map<String, dynamic>? headers,
    Map<String, dynamic>? extra,
    ValidateStatus? validateStatus,
    ProgressCallback? onSendProgress,
    ProgressCallback? onReceiveProgress,
  }) async {
    final _path = r'/fake';
    final _options = Options(
      method: r'DELETE',
      headers: <String, dynamic>{
        r'required_boolean_group': requiredBooleanGroup,
        if (booleanGroup != null) r'boolean_group': booleanGroup,
        ...?headers,
      },
      extra: <String, dynamic>{
        'secure': <Map<String, String>>[
          {
            'type': 'http',
            'scheme': 'bearer',
            'name': 'bearer_test',
          },
        ],
        ...?extra,
      },
      validateStatus: validateStatus,
    );

    final _queryParameters = <String, dynamic>{
      r'required_string_group': encodeQueryParameter(_serializers, requiredStringGroup, const FullType(int)),
      r'required_int64_group': encodeQueryParameter(_serializers, requiredInt64Group, const FullType(int)),
      if (stringGroup != null) r'string_group': encodeQueryParameter(_serializers, stringGroup, const FullType(int)),
      if (int64Group != null) r'int64_group': encodeQueryParameter(_serializers, int64Group, const FullType(int)),
    };

    final _response = await _dio.request<Object>(
      _path,
      options: _options,
      queryParameters: _queryParameters,
      cancelToken: cancelToken,
      onSendProgress: onSendProgress,
      onReceiveProgress: onReceiveProgress,
    );

    return _response;
  }

  /// test inline additionalProperties
  /// 
  ///
  /// Parameters:
  /// * [requestBody] - request body
  /// * [cancelToken] - A [CancelToken] that can be used to cancel the operation
  /// * [headers] - Can be used to add additional headers to the request
  /// * [extras] - Can be used to add flags to the request
  /// * [validateStatus] - A [ValidateStatus] callback that can be used to determine request success based on the HTTP status of the response
  /// * [onSendProgress] - A [ProgressCallback] that can be used to get the send progress
  /// * [onReceiveProgress] - A [ProgressCallback] that can be used to get the receive progress
  ///
  /// Returns a [Future]
  /// Throws [DioError] if API call or serialization fails
  Future<Response<void>> testInlineAdditionalProperties({ 
    required BuiltMap<String, String> requestBody,
    CancelToken? cancelToken,
    Map<String, dynamic>? headers,
    Map<String, dynamic>? extra,
    ValidateStatus? validateStatus,
    ProgressCallback? onSendProgress,
    ProgressCallback? onReceiveProgress,
  }) async {
    final _path = r'/fake/inline-additionalProperties';
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
      const _type = FullType(BuiltMap, [FullType(String), FullType(String)]);
      _bodyData = _serializers.serialize(requestBody, specifiedType: _type);

    } catch(error, stackTrace) {
      throw DioError(
         requestOptions: _options.compose(
          _dio.options,
          _path,
        ),
        type: DioErrorType.other,
        error: error,
      )..stackTrace = stackTrace;
    }

    final _response = await _dio.request<Object>(
      _path,
      data: _bodyData,
      options: _options,
      cancelToken: cancelToken,
      onSendProgress: onSendProgress,
      onReceiveProgress: onReceiveProgress,
    );

    return _response;
  }

  /// test json serialization of form data
  /// 
  ///
  /// Parameters:
  /// * [param] - field1
  /// * [param2] - field2
  /// * [cancelToken] - A [CancelToken] that can be used to cancel the operation
  /// * [headers] - Can be used to add additional headers to the request
  /// * [extras] - Can be used to add flags to the request
  /// * [validateStatus] - A [ValidateStatus] callback that can be used to determine request success based on the HTTP status of the response
  /// * [onSendProgress] - A [ProgressCallback] that can be used to get the send progress
  /// * [onReceiveProgress] - A [ProgressCallback] that can be used to get the receive progress
  ///
  /// Returns a [Future]
  /// Throws [DioError] if API call or serialization fails
  Future<Response<void>> testJsonFormData({ 
    required String param,
    required String param2,
    CancelToken? cancelToken,
    Map<String, dynamic>? headers,
    Map<String, dynamic>? extra,
    ValidateStatus? validateStatus,
    ProgressCallback? onSendProgress,
    ProgressCallback? onReceiveProgress,
  }) async {
    final _path = r'/fake/jsonFormData';
    final _options = Options(
      method: r'GET',
      headers: <String, dynamic>{
        ...?headers,
      },
      extra: <String, dynamic>{
        'secure': <Map<String, String>>[],
        ...?extra,
      },
      contentType: 'application/x-www-form-urlencoded',
      validateStatus: validateStatus,
    );

    dynamic _bodyData;

    try {
      _bodyData = <String, dynamic>{
        r'param': encodeQueryParameter(_serializers, param, const FullType(String)),
        r'param2': encodeQueryParameter(_serializers, param2, const FullType(String)),
      };

    } catch(error, stackTrace) {
      throw DioError(
         requestOptions: _options.compose(
          _dio.options,
          _path,
        ),
        type: DioErrorType.other,
        error: error,
      )..stackTrace = stackTrace;
    }

    final _response = await _dio.request<Object>(
      _path,
      data: _bodyData,
      options: _options,
      cancelToken: cancelToken,
      onSendProgress: onSendProgress,
      onReceiveProgress: onReceiveProgress,
    );

    return _response;
  }

  /// testQueryParameterCollectionFormat
  /// To test the collection format in query parameters
  ///
  /// Parameters:
  /// * [pipe] 
  /// * [ioutil] 
  /// * [http] 
  /// * [url] 
  /// * [context] 
  /// * [language] 
  /// * [cancelToken] - A [CancelToken] that can be used to cancel the operation
  /// * [headers] - Can be used to add additional headers to the request
  /// * [extras] - Can be used to add flags to the request
  /// * [validateStatus] - A [ValidateStatus] callback that can be used to determine request success based on the HTTP status of the response
  /// * [onSendProgress] - A [ProgressCallback] that can be used to get the send progress
  /// * [onReceiveProgress] - A [ProgressCallback] that can be used to get the receive progress
  ///
  /// Returns a [Future]
  /// Throws [DioError] if API call or serialization fails
  Future<Response<void>> testQueryParameterCollectionFormat({ 
    required BuiltList<String> pipe,
    required BuiltList<String> ioutil,
    required BuiltList<String> http,
    required BuiltList<String> url,
    required BuiltList<String> context,
    BuiltMap<String, String>? language,
    CancelToken? cancelToken,
    Map<String, dynamic>? headers,
    Map<String, dynamic>? extra,
    ValidateStatus? validateStatus,
    ProgressCallback? onSendProgress,
    ProgressCallback? onReceiveProgress,
  }) async {
    final _path = r'/fake/test-query-paramters';
    final _options = Options(
      method: r'PUT',
      headers: <String, dynamic>{
        ...?headers,
      },
      extra: <String, dynamic>{
        'secure': <Map<String, String>>[],
        ...?extra,
      },
      validateStatus: validateStatus,
    );

    final _queryParameters = <String, dynamic>{
      r'pipe': encodeCollectionQueryParameter<String>(_serializers, pipe, const FullType(BuiltList, [FullType(String)]), format: ListFormat.pipes,),
      r'ioutil': encodeCollectionQueryParameter<String>(_serializers, ioutil, const FullType(BuiltList, [FullType(String)]), format: ListFormat.csv,),
      r'http': encodeCollectionQueryParameter<String>(_serializers, http, const FullType(BuiltList, [FullType(String)]), format: ListFormat.ssv,),
      r'url': encodeCollectionQueryParameter<String>(_serializers, url, const FullType(BuiltList, [FullType(String)]), format: ListFormat.csv,),
      r'context': encodeCollectionQueryParameter<String>(_serializers, context, const FullType(BuiltList, [FullType(String)]), format: ListFormat.multi,),
      if (language != null) r'language': encodeQueryParameter(_serializers, language, const FullType(BuiltMap, [FullType(String), FullType(String)])),
    };

    final _response = await _dio.request<Object>(
      _path,
      options: _options,
      queryParameters: _queryParameters,
      cancelToken: cancelToken,
      onSendProgress: onSendProgress,
      onReceiveProgress: onReceiveProgress,
    );

    return _response;
  }

}
class InnerEnum extends EnumClass {

  @BuiltValueEnumConst(wireName: r'>')
  static const InnerEnum greaterThan = _$innerEnum_greaterThan;
  @BuiltValueEnumConst(wireName: r'$')
  static const InnerEnum dollar = _$innerEnum_dollar;

  static Serializer<InnerEnum> get serializer => _$innerEnumSerializer;

  const InnerEnum._(String name): super(name);

  static BuiltSet<InnerEnum> get values => _$innerEnumValues;
  static InnerEnum valueOf(String name) => _$innerEnumValueOf(name);
}
class EnumQueryDoubleEnum extends EnumClass {

  /// Query parameter enum test (double)
  @BuiltValueEnumConst(wireName: r'1.1')
  static const EnumQueryDoubleEnum number1Period1 = _$enumQueryDoubleEnum_number1Period1;
  /// Query parameter enum test (double)
  @BuiltValueEnumConst(wireName: r'-1.2')
  static const EnumQueryDoubleEnum numberNegative1Period2 = _$enumQueryDoubleEnum_numberNegative1Period2;

  static Serializer<EnumQueryDoubleEnum> get serializer => _$enumQueryDoubleEnumSerializer;

  const EnumQueryDoubleEnum._(String name): super(name);

  static BuiltSet<EnumQueryDoubleEnum> get values => _$enumQueryDoubleEnumValues;
  static EnumQueryDoubleEnum valueOf(String name) => _$enumQueryDoubleEnumValueOf(name);
}
class EnumFormStringArrayEnum extends EnumClass {

  /// Form parameter enum test (string array)
  @BuiltValueEnumConst(wireName: r'>')
  static const EnumFormStringArrayEnum greaterThan = _$enumFormStringArrayEnum_greaterThan;
  /// Form parameter enum test (string array)
  @BuiltValueEnumConst(wireName: r'$')
  static const EnumFormStringArrayEnum dollar = _$enumFormStringArrayEnum_dollar;

  static Serializer<EnumFormStringArrayEnum> get serializer => _$enumFormStringArrayEnumSerializer;

  const EnumFormStringArrayEnum._(String name): super(name);

  static BuiltSet<EnumFormStringArrayEnum> get values => _$enumFormStringArrayEnumValues;
  static EnumFormStringArrayEnum valueOf(String name) => _$enumFormStringArrayEnumValueOf(name);
}
class EnumHeaderStringEnum extends EnumClass {

  /// Header parameter enum test (string)
  @BuiltValueEnumConst(wireName: r'_abc')
  static const EnumHeaderStringEnum abc = _$enumHeaderStringEnum_abc;
  /// Header parameter enum test (string)
  @BuiltValueEnumConst(wireName: r'-efg')
  static const EnumHeaderStringEnum efg = _$enumHeaderStringEnum_efg;
  /// Header parameter enum test (string)
  @BuiltValueEnumConst(wireName: r'(xyz)')
  static const EnumHeaderStringEnum leftParenthesisXyzRightParenthesis = _$enumHeaderStringEnum_leftParenthesisXyzRightParenthesis;

  static Serializer<EnumHeaderStringEnum> get serializer => _$enumHeaderStringEnumSerializer;

  const EnumHeaderStringEnum._(String name): super(name);

  static BuiltSet<EnumHeaderStringEnum> get values => _$enumHeaderStringEnumValues;
  static EnumHeaderStringEnum valueOf(String name) => _$enumHeaderStringEnumValueOf(name);
}
class EnumQueryStringEnum extends EnumClass {

  /// Query parameter enum test (string)
  @BuiltValueEnumConst(wireName: r'_abc')
  static const EnumQueryStringEnum abc = _$enumQueryStringEnum_abc;
  /// Query parameter enum test (string)
  @BuiltValueEnumConst(wireName: r'-efg')
  static const EnumQueryStringEnum efg = _$enumQueryStringEnum_efg;
  /// Query parameter enum test (string)
  @BuiltValueEnumConst(wireName: r'(xyz)')
  static const EnumQueryStringEnum leftParenthesisXyzRightParenthesis = _$enumQueryStringEnum_leftParenthesisXyzRightParenthesis;

  static Serializer<EnumQueryStringEnum> get serializer => _$enumQueryStringEnumSerializer;

  const EnumQueryStringEnum._(String name): super(name);

  static BuiltSet<EnumQueryStringEnum> get values => _$enumQueryStringEnumValues;
  static EnumQueryStringEnum valueOf(String name) => _$enumQueryStringEnumValueOf(name);
}
class EnumFormStringEnum extends EnumClass {

  /// Form parameter enum test (string)
  @BuiltValueEnumConst(wireName: r'_abc')
  static const EnumFormStringEnum abc = _$enumFormStringEnum_abc;
  /// Form parameter enum test (string)
  @BuiltValueEnumConst(wireName: r'-efg')
  static const EnumFormStringEnum efg = _$enumFormStringEnum_efg;
  /// Form parameter enum test (string)
  @BuiltValueEnumConst(wireName: r'(xyz)')
  static const EnumFormStringEnum leftParenthesisXyzRightParenthesis = _$enumFormStringEnum_leftParenthesisXyzRightParenthesis;

  static Serializer<EnumFormStringEnum> get serializer => _$enumFormStringEnumSerializer;

  const EnumFormStringEnum._(String name): super(name);

  static BuiltSet<EnumFormStringEnum> get values => _$enumFormStringEnumValues;
  static EnumFormStringEnum valueOf(String name) => _$enumFormStringEnumValueOf(name);
}
class EnumQueryStringArrayEnum extends EnumClass {

  /// Query parameter enum test (string array)
  @BuiltValueEnumConst(wireName: r'>')
  static const EnumQueryStringArrayEnum greaterThan = _$enumQueryStringArrayEnum_greaterThan;
  /// Query parameter enum test (string array)
  @BuiltValueEnumConst(wireName: r'$')
  static const EnumQueryStringArrayEnum dollar = _$enumQueryStringArrayEnum_dollar;

  static Serializer<EnumQueryStringArrayEnum> get serializer => _$enumQueryStringArrayEnumSerializer;

  const EnumQueryStringArrayEnum._(String name): super(name);

  static BuiltSet<EnumQueryStringArrayEnum> get values => _$enumQueryStringArrayEnumValues;
  static EnumQueryStringArrayEnum valueOf(String name) => _$enumQueryStringArrayEnumValueOf(name);
}
class EnumQueryIntegerEnum extends EnumClass {

  /// Query parameter enum test (double)
  @BuiltValueEnumConst(wireNumber: 1)
  static const EnumQueryIntegerEnum number1 = _$enumQueryIntegerEnum_number1;
  /// Query parameter enum test (double)
  @BuiltValueEnumConst(wireNumber: -2)
  static const EnumQueryIntegerEnum numberNegative2 = _$enumQueryIntegerEnum_numberNegative2;

  static Serializer<EnumQueryIntegerEnum> get serializer => _$enumQueryIntegerEnumSerializer;

  const EnumQueryIntegerEnum._(String name): super(name);

  static BuiltSet<EnumQueryIntegerEnum> get values => _$enumQueryIntegerEnumValues;
  static EnumQueryIntegerEnum valueOf(String name) => _$enumQueryIntegerEnumValueOf(name);
}
class EnumHeaderStringArrayEnum extends EnumClass {

  /// Header parameter enum test (string array)
  @BuiltValueEnumConst(wireName: r'>')
  static const EnumHeaderStringArrayEnum greaterThan = _$enumHeaderStringArrayEnum_greaterThan;
  /// Header parameter enum test (string array)
  @BuiltValueEnumConst(wireName: r'$')
  static const EnumHeaderStringArrayEnum dollar = _$enumHeaderStringArrayEnum_dollar;

  static Serializer<EnumHeaderStringArrayEnum> get serializer => _$enumHeaderStringArrayEnumSerializer;

  const EnumHeaderStringArrayEnum._(String name): super(name);

  static BuiltSet<EnumHeaderStringArrayEnum> get values => _$enumHeaderStringArrayEnumValues;
  static EnumHeaderStringArrayEnum valueOf(String name) => _$enumHeaderStringArrayEnumValueOf(name);
}

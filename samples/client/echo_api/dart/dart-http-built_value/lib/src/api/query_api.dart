//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

import 'dart:async';

import 'package:built_value/serializer.dart';
import 'package:http/http.dart' as http;

import 'package:openapi/src/api_util.dart';
import 'package:openapi/src/model/pet.dart';
import 'package:openapi/src/model/test_query_style_form_explode_true_array_string_query_object_parameter.dart';

class QueryApi {
  final String _basePath;
  final http.Client? _client;

  final Serializers _serializers;

  const QueryApi(this._client, this._serializers, this._basePath);

  /// Test query parameter(s)
  /// Test query parameter(s)
  ///
  /// Parameters:
  /// * [integerQuery]
  /// * [booleanQuery]
  /// * [stringQuery]
  /// * [cancelToken] - A [CancelToken] that can be used to cancel the operation
  /// * [headers] - Can be used to add additional headers to the request
  /// * [validateStatus] - A [ValidateStatus] callback that can be used to determine request success based on the HTTP status of the response
  ///
  /// Throws [DioError] if API call or serialization fails

  Future<String> testQueryIntegerBooleanString({
    int? integerQuery,
    bool? booleanQuery,
    String? stringQuery,
    Map<String, String>? headers,
  }) async {
    final _path = r'/query/integer/boolean/string';
    final _uri = Uri.parse(_basePath + _path);

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

    final _queryParameters = <String, dynamic>{
      if (integerQuery != null)
        r'integer_query': encodeQueryParameter(
            _serializers, integerQuery, const FullType(int)),
      if (booleanQuery != null)
        r'boolean_query': encodeQueryParameter(
            _serializers, booleanQuery, const FullType(bool)),
      if (stringQuery != null)
        r'string_query': encodeQueryParameter(
            _serializers, stringQuery, const FullType(String)),
    };

    final _response = await _dio.request<Object>(
      _path,
      options: _options,
      queryParameters: _queryParameters,
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

  /// Test query parameter(s)
  /// Test query parameter(s)
  ///
  /// Parameters:
  /// * [queryObject]
  /// * [cancelToken] - A [CancelToken] that can be used to cancel the operation
  /// * [headers] - Can be used to add additional headers to the request
  /// * [validateStatus] - A [ValidateStatus] callback that can be used to determine request success based on the HTTP status of the response
  ///
  /// Throws [DioError] if API call or serialization fails

  Future<String> testQueryStyleFormExplodeTrueArrayString({
    TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter? queryObject,
    Map<String, String>? headers,
  }) async {
    final _path = r'/query/style_form/explode_true/array_string';
    final _uri = Uri.parse(_basePath + _path);

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

    final _queryParameters = <String, dynamic>{
      if (queryObject != null)
        r'query_object': encodeQueryParameter(
            _serializers,
            queryObject,
            const FullType(
                TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter)),
    };

    final _response = await _dio.request<Object>(
      _path,
      options: _options,
      queryParameters: _queryParameters,
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

  /// Test query parameter(s)
  /// Test query parameter(s)
  ///
  /// Parameters:
  /// * [queryObject]
  /// * [cancelToken] - A [CancelToken] that can be used to cancel the operation
  /// * [headers] - Can be used to add additional headers to the request
  /// * [validateStatus] - A [ValidateStatus] callback that can be used to determine request success based on the HTTP status of the response
  ///
  /// Throws [DioError] if API call or serialization fails

  Future<String> testQueryStyleFormExplodeTrueObject({
    Pet? queryObject,
    Map<String, String>? headers,
  }) async {
    final _path = r'/query/style_form/explode_true/object';
    final _uri = Uri.parse(_basePath + _path);

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

    final _queryParameters = <String, dynamic>{
      if (queryObject != null)
        r'query_object': encodeQueryParameter(
            _serializers, queryObject, const FullType(Pet)),
    };

    final _response = await _dio.request<Object>(
      _path,
      options: _options,
      queryParameters: _queryParameters,
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
}

//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.7

// ignore_for_file: unused_import

import 'dart:async';
import 'package:dio/dio.dart';
import 'package:built_value/serializer.dart';

import 'dart:typed_data';
import 'package:built_collection/built_collection.dart';
import 'package:openapi/api_util.dart';
import 'package:openapi/model/file_schema_test_class.dart';
import 'package:openapi/model/health_check_result.dart';
import 'package:openapi/model/model_client.dart';
import 'package:openapi/model/outer_composite.dart';
import 'package:openapi/model/outer_object_with_enum_property.dart';
import 'package:openapi/model/pet.dart';
import 'package:openapi/model/user.dart';

class FakeApi {

  final Dio _dio;

  final Serializers _serializers;

  const FakeApi(this._dio, this._serializers);

  /// Health check endpoint
  ///
  /// 
  Future<Response<HealthCheckResult>> fakeHealthGet({ 
    CancelToken cancelToken,
    Map<String, dynamic> headers,
    Map<String, dynamic> extra,
    ValidateStatus validateStatus,
    ProgressCallback onSendProgress,
    ProgressCallback onReceiveProgress,
  }) async {
    final _request = RequestOptions(
      path: r'/fake/health',
      method: 'GET',
      headers: <String, dynamic>{
        ...?headers,
      },
      extra: <String, dynamic>{
        'secure': <Map<String, String>>[],
        ...?extra,
      },
      validateStatus: validateStatus,
      contentType: 'application/json',
      cancelToken: cancelToken,
      onSendProgress: onSendProgress,
      onReceiveProgress: onReceiveProgress,
    );

    dynamic _bodyData;

    final _response = await _dio.request<dynamic>(
      _request.path,
      data: _bodyData,
      options: _request,
    );

    const _responseType = FullType(HealthCheckResult);
    final _responseData = _serializers.deserialize(
      _response.data,
      specifiedType: _responseType,
    ) as HealthCheckResult;

    return Response<HealthCheckResult>(
      data: _responseData,
      headers: _response.headers,
      isRedirect: _response.isRedirect,
      request: _response.request,
      redirects: _response.redirects,
      statusCode: _response.statusCode,
      statusMessage: _response.statusMessage,
      extra: _response.extra,
    );
  }

  /// test http signature authentication
  ///
  /// 
  Future<Response<void>> fakeHttpSignatureTest(
    Pet pet, { 
    String query1,
    String header1,
    CancelToken cancelToken,
    Map<String, dynamic> headers,
    Map<String, dynamic> extra,
    ValidateStatus validateStatus,
    ProgressCallback onSendProgress,
    ProgressCallback onReceiveProgress,
  }) async {
    final _request = RequestOptions(
      path: r'/fake/http-signature-test',
      method: 'GET',
      headers: <String, dynamic>{
        if (header1 != null) r'header_1': header1,
        ...?headers,
      },
      queryParameters: <String, dynamic>{
        if (query1 != null) r'query_1': query1,
      },
      extra: <String, dynamic>{
        'secure': <Map<String, String>>[
          {
            'type': 'http',
            'name': 'http_signature_test',
          },
        ],
        ...?extra,
      },
      validateStatus: validateStatus,
      contentType: 'application/json',
      cancelToken: cancelToken,
      onSendProgress: onSendProgress,
      onReceiveProgress: onReceiveProgress,
    );

    dynamic _bodyData;

    const _type = FullType(Pet);
    _bodyData = _serializers.serialize(pet, specifiedType: _type);

    final _response = await _dio.request<dynamic>(
      _request.path,
      data: _bodyData,
      options: _request,
    );

    return _response;
  }

  /// 
  ///
  /// Test serialization of outer boolean types
  Future<Response<bool>> fakeOuterBooleanSerialize({ 
    bool body,
    CancelToken cancelToken,
    Map<String, dynamic> headers,
    Map<String, dynamic> extra,
    ValidateStatus validateStatus,
    ProgressCallback onSendProgress,
    ProgressCallback onReceiveProgress,
  }) async {
    final _request = RequestOptions(
      path: r'/fake/outer/boolean',
      method: 'POST',
      headers: <String, dynamic>{
        ...?headers,
      },
      extra: <String, dynamic>{
        'secure': <Map<String, String>>[],
        ...?extra,
      },
      validateStatus: validateStatus,
      contentType: 'application/json',
      cancelToken: cancelToken,
      onSendProgress: onSendProgress,
      onReceiveProgress: onReceiveProgress,
    );

    dynamic _bodyData;

    _bodyData = body;

    final _response = await _dio.request<dynamic>(
      _request.path,
      data: _bodyData,
      options: _request,
    );

    final bool _responseData = _response.data as bool;

    return Response<bool>(
      data: _responseData,
      headers: _response.headers,
      isRedirect: _response.isRedirect,
      request: _response.request,
      redirects: _response.redirects,
      statusCode: _response.statusCode,
      statusMessage: _response.statusMessage,
      extra: _response.extra,
    );
  }

  /// 
  ///
  /// Test serialization of object with outer number type
  Future<Response<OuterComposite>> fakeOuterCompositeSerialize({ 
    OuterComposite outerComposite,
    CancelToken cancelToken,
    Map<String, dynamic> headers,
    Map<String, dynamic> extra,
    ValidateStatus validateStatus,
    ProgressCallback onSendProgress,
    ProgressCallback onReceiveProgress,
  }) async {
    final _request = RequestOptions(
      path: r'/fake/outer/composite',
      method: 'POST',
      headers: <String, dynamic>{
        ...?headers,
      },
      extra: <String, dynamic>{
        'secure': <Map<String, String>>[],
        ...?extra,
      },
      validateStatus: validateStatus,
      contentType: 'application/json',
      cancelToken: cancelToken,
      onSendProgress: onSendProgress,
      onReceiveProgress: onReceiveProgress,
    );

    dynamic _bodyData;

    const _type = FullType(OuterComposite);
    _bodyData = _serializers.serialize(outerComposite, specifiedType: _type);

    final _response = await _dio.request<dynamic>(
      _request.path,
      data: _bodyData,
      options: _request,
    );

    const _responseType = FullType(OuterComposite);
    final _responseData = _serializers.deserialize(
      _response.data,
      specifiedType: _responseType,
    ) as OuterComposite;

    return Response<OuterComposite>(
      data: _responseData,
      headers: _response.headers,
      isRedirect: _response.isRedirect,
      request: _response.request,
      redirects: _response.redirects,
      statusCode: _response.statusCode,
      statusMessage: _response.statusMessage,
      extra: _response.extra,
    );
  }

  /// 
  ///
  /// Test serialization of outer number types
  Future<Response<num>> fakeOuterNumberSerialize({ 
    num body,
    CancelToken cancelToken,
    Map<String, dynamic> headers,
    Map<String, dynamic> extra,
    ValidateStatus validateStatus,
    ProgressCallback onSendProgress,
    ProgressCallback onReceiveProgress,
  }) async {
    final _request = RequestOptions(
      path: r'/fake/outer/number',
      method: 'POST',
      headers: <String, dynamic>{
        ...?headers,
      },
      extra: <String, dynamic>{
        'secure': <Map<String, String>>[],
        ...?extra,
      },
      validateStatus: validateStatus,
      contentType: 'application/json',
      cancelToken: cancelToken,
      onSendProgress: onSendProgress,
      onReceiveProgress: onReceiveProgress,
    );

    dynamic _bodyData;

    _bodyData = body;

    final _response = await _dio.request<dynamic>(
      _request.path,
      data: _bodyData,
      options: _request,
    );

    final num _responseData = _response.data as num;

    return Response<num>(
      data: _responseData,
      headers: _response.headers,
      isRedirect: _response.isRedirect,
      request: _response.request,
      redirects: _response.redirects,
      statusCode: _response.statusCode,
      statusMessage: _response.statusMessage,
      extra: _response.extra,
    );
  }

  /// 
  ///
  /// Test serialization of outer string types
  Future<Response<String>> fakeOuterStringSerialize({ 
    String body,
    CancelToken cancelToken,
    Map<String, dynamic> headers,
    Map<String, dynamic> extra,
    ValidateStatus validateStatus,
    ProgressCallback onSendProgress,
    ProgressCallback onReceiveProgress,
  }) async {
    final _request = RequestOptions(
      path: r'/fake/outer/string',
      method: 'POST',
      headers: <String, dynamic>{
        ...?headers,
      },
      extra: <String, dynamic>{
        'secure': <Map<String, String>>[],
        ...?extra,
      },
      validateStatus: validateStatus,
      contentType: 'application/json',
      cancelToken: cancelToken,
      onSendProgress: onSendProgress,
      onReceiveProgress: onReceiveProgress,
    );

    dynamic _bodyData;

    _bodyData = body;

    final _response = await _dio.request<dynamic>(
      _request.path,
      data: _bodyData,
      options: _request,
    );

    final String _responseData = _response.data as String;

    return Response<String>(
      data: _responseData,
      headers: _response.headers,
      isRedirect: _response.isRedirect,
      request: _response.request,
      redirects: _response.redirects,
      statusCode: _response.statusCode,
      statusMessage: _response.statusMessage,
      extra: _response.extra,
    );
  }

  /// 
  ///
  /// Test serialization of enum (int) properties with examples
  Future<Response<OuterObjectWithEnumProperty>> fakePropertyEnumIntegerSerialize(
    OuterObjectWithEnumProperty outerObjectWithEnumProperty, { 
    CancelToken cancelToken,
    Map<String, dynamic> headers,
    Map<String, dynamic> extra,
    ValidateStatus validateStatus,
    ProgressCallback onSendProgress,
    ProgressCallback onReceiveProgress,
  }) async {
    final _request = RequestOptions(
      path: r'/fake/property/enum-int',
      method: 'POST',
      headers: <String, dynamic>{
        ...?headers,
      },
      extra: <String, dynamic>{
        'secure': <Map<String, String>>[],
        ...?extra,
      },
      validateStatus: validateStatus,
      contentType: 'application/json',
      cancelToken: cancelToken,
      onSendProgress: onSendProgress,
      onReceiveProgress: onReceiveProgress,
    );

    dynamic _bodyData;

    const _type = FullType(OuterObjectWithEnumProperty);
    _bodyData = _serializers.serialize(outerObjectWithEnumProperty, specifiedType: _type);

    final _response = await _dio.request<dynamic>(
      _request.path,
      data: _bodyData,
      options: _request,
    );

    const _responseType = FullType(OuterObjectWithEnumProperty);
    final _responseData = _serializers.deserialize(
      _response.data,
      specifiedType: _responseType,
    ) as OuterObjectWithEnumProperty;

    return Response<OuterObjectWithEnumProperty>(
      data: _responseData,
      headers: _response.headers,
      isRedirect: _response.isRedirect,
      request: _response.request,
      redirects: _response.redirects,
      statusCode: _response.statusCode,
      statusMessage: _response.statusMessage,
      extra: _response.extra,
    );
  }

  /// 
  ///
  /// For this test, the body has to be a binary file.
  Future<Response<void>> testBodyWithBinary(
    Uint8List body, { 
    CancelToken cancelToken,
    Map<String, dynamic> headers,
    Map<String, dynamic> extra,
    ValidateStatus validateStatus,
    ProgressCallback onSendProgress,
    ProgressCallback onReceiveProgress,
  }) async {
    final _request = RequestOptions(
      path: r'/fake/body-with-binary',
      method: 'PUT',
      headers: <String, dynamic>{
        ...?headers,
      },
      extra: <String, dynamic>{
        'secure': <Map<String, String>>[],
        ...?extra,
      },
      validateStatus: validateStatus,
      contentType: 'image/png',
      cancelToken: cancelToken,
      onSendProgress: onSendProgress,
      onReceiveProgress: onReceiveProgress,
    );

    dynamic _bodyData;

    _bodyData = body;

    final _response = await _dio.request<dynamic>(
      _request.path,
      data: _bodyData,
      options: _request,
    );

    return _response;
  }

  /// 
  ///
  /// For this test, the body for this request must reference a schema named `File`.
  Future<Response<void>> testBodyWithFileSchema(
    FileSchemaTestClass fileSchemaTestClass, { 
    CancelToken cancelToken,
    Map<String, dynamic> headers,
    Map<String, dynamic> extra,
    ValidateStatus validateStatus,
    ProgressCallback onSendProgress,
    ProgressCallback onReceiveProgress,
  }) async {
    final _request = RequestOptions(
      path: r'/fake/body-with-file-schema',
      method: 'PUT',
      headers: <String, dynamic>{
        ...?headers,
      },
      extra: <String, dynamic>{
        'secure': <Map<String, String>>[],
        ...?extra,
      },
      validateStatus: validateStatus,
      contentType: 'application/json',
      cancelToken: cancelToken,
      onSendProgress: onSendProgress,
      onReceiveProgress: onReceiveProgress,
    );

    dynamic _bodyData;

    const _type = FullType(FileSchemaTestClass);
    _bodyData = _serializers.serialize(fileSchemaTestClass, specifiedType: _type);

    final _response = await _dio.request<dynamic>(
      _request.path,
      data: _bodyData,
      options: _request,
    );

    return _response;
  }

  /// 
  ///
  /// 
  Future<Response<void>> testBodyWithQueryParams(
    String query,
    User user, { 
    CancelToken cancelToken,
    Map<String, dynamic> headers,
    Map<String, dynamic> extra,
    ValidateStatus validateStatus,
    ProgressCallback onSendProgress,
    ProgressCallback onReceiveProgress,
  }) async {
    final _request = RequestOptions(
      path: r'/fake/body-with-query-params',
      method: 'PUT',
      headers: <String, dynamic>{
        ...?headers,
      },
      queryParameters: <String, dynamic>{
        r'query': query,
      },
      extra: <String, dynamic>{
        'secure': <Map<String, String>>[],
        ...?extra,
      },
      validateStatus: validateStatus,
      contentType: 'application/json',
      cancelToken: cancelToken,
      onSendProgress: onSendProgress,
      onReceiveProgress: onReceiveProgress,
    );

    dynamic _bodyData;

    const _type = FullType(User);
    _bodyData = _serializers.serialize(user, specifiedType: _type);

    final _response = await _dio.request<dynamic>(
      _request.path,
      data: _bodyData,
      options: _request,
    );

    return _response;
  }

  /// To test \"client\" model
  ///
  /// To test \"client\" model
  Future<Response<ModelClient>> testClientModel(
    ModelClient modelClient, { 
    CancelToken cancelToken,
    Map<String, dynamic> headers,
    Map<String, dynamic> extra,
    ValidateStatus validateStatus,
    ProgressCallback onSendProgress,
    ProgressCallback onReceiveProgress,
  }) async {
    final _request = RequestOptions(
      path: r'/fake',
      method: 'PATCH',
      headers: <String, dynamic>{
        ...?headers,
      },
      extra: <String, dynamic>{
        'secure': <Map<String, String>>[],
        ...?extra,
      },
      validateStatus: validateStatus,
      contentType: 'application/json',
      cancelToken: cancelToken,
      onSendProgress: onSendProgress,
      onReceiveProgress: onReceiveProgress,
    );

    dynamic _bodyData;

    const _type = FullType(ModelClient);
    _bodyData = _serializers.serialize(modelClient, specifiedType: _type);

    final _response = await _dio.request<dynamic>(
      _request.path,
      data: _bodyData,
      options: _request,
    );

    const _responseType = FullType(ModelClient);
    final _responseData = _serializers.deserialize(
      _response.data,
      specifiedType: _responseType,
    ) as ModelClient;

    return Response<ModelClient>(
      data: _responseData,
      headers: _response.headers,
      isRedirect: _response.isRedirect,
      request: _response.request,
      redirects: _response.redirects,
      statusCode: _response.statusCode,
      statusMessage: _response.statusMessage,
      extra: _response.extra,
    );
  }

  /// Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
  ///
  /// Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
  Future<Response<void>> testEndpointParameters(
    num number,
    double double_,
    String patternWithoutDelimiter,
    String byte, { 
    int integer,
    int int32,
    int int64,
    double float,
    String string,
    Uint8List binary,
    DateTime date,
    DateTime dateTime,
    String password,
    String callback,
    CancelToken cancelToken,
    Map<String, dynamic> headers,
    Map<String, dynamic> extra,
    ValidateStatus validateStatus,
    ProgressCallback onSendProgress,
    ProgressCallback onReceiveProgress,
  }) async {
    final _request = RequestOptions(
      path: r'/fake',
      method: 'POST',
      headers: <String, dynamic>{
        ...?headers,
      },
      extra: <String, dynamic>{
        'secure': <Map<String, String>>[
          {
            'type': 'http',
            'name': 'http_basic_test',
          },
        ],
        ...?extra,
      },
      validateStatus: validateStatus,
      contentType: 'application/x-www-form-urlencoded',
      cancelToken: cancelToken,
      onSendProgress: onSendProgress,
      onReceiveProgress: onReceiveProgress,
    );

    dynamic _bodyData;

    _bodyData = <String, dynamic>{
      if (integer != null) r'integer': encodeFormParameter(_serializers, integer, const FullType(int)),
      if (int32 != null) r'int32': encodeFormParameter(_serializers, int32, const FullType(int)),
      if (int64 != null) r'int64': encodeFormParameter(_serializers, int64, const FullType(int)),
      r'number': encodeFormParameter(_serializers, number, const FullType(num)),
      if (float != null) r'float': encodeFormParameter(_serializers, float, const FullType(double)),
      r'double': encodeFormParameter(_serializers, double_, const FullType(double)),
      if (string != null) r'string': encodeFormParameter(_serializers, string, const FullType(String)),
      r'pattern_without_delimiter': encodeFormParameter(_serializers, patternWithoutDelimiter, const FullType(String)),
      r'byte': encodeFormParameter(_serializers, byte, const FullType(String)),
      if (binary != null) r'binary': MultipartFile.fromBytes(binary, filename: r'binary'),
      if (date != null) r'date': encodeFormParameter(_serializers, date, const FullType(DateTime)),
      if (dateTime != null) r'dateTime': encodeFormParameter(_serializers, dateTime, const FullType(DateTime)),
      if (password != null) r'password': encodeFormParameter(_serializers, password, const FullType(String)),
      if (callback != null) r'callback': encodeFormParameter(_serializers, callback, const FullType(String)),
    };

    final _response = await _dio.request<dynamic>(
      _request.path,
      data: _bodyData,
      options: _request,
    );

    return _response;
  }

  /// To test enum parameters
  ///
  /// To test enum parameters
  Future<Response<void>> testEnumParameters({ 
    BuiltList<String> enumHeaderStringArray,
    String enumHeaderString,
    BuiltList<String> enumQueryStringArray,
    String enumQueryString,
    int enumQueryInteger,
    double enumQueryDouble,
    BuiltList<String> enumFormStringArray,
    String enumFormString,
    CancelToken cancelToken,
    Map<String, dynamic> headers,
    Map<String, dynamic> extra,
    ValidateStatus validateStatus,
    ProgressCallback onSendProgress,
    ProgressCallback onReceiveProgress,
  }) async {
    final _request = RequestOptions(
      path: r'/fake',
      method: 'GET',
      headers: <String, dynamic>{
        if (enumHeaderStringArray != null) r'enum_header_string_array': enumHeaderStringArray,
        if (enumHeaderString != null) r'enum_header_string': enumHeaderString,
        ...?headers,
      },
      queryParameters: <String, dynamic>{
        if (enumQueryStringArray != null) r'enum_query_string_array': enumQueryStringArray,
        if (enumQueryString != null) r'enum_query_string': enumQueryString,
        if (enumQueryInteger != null) r'enum_query_integer': enumQueryInteger,
        if (enumQueryDouble != null) r'enum_query_double': enumQueryDouble,
      },
      extra: <String, dynamic>{
        'secure': <Map<String, String>>[],
        ...?extra,
      },
      validateStatus: validateStatus,
      contentType: 'application/x-www-form-urlencoded',
      cancelToken: cancelToken,
      onSendProgress: onSendProgress,
      onReceiveProgress: onReceiveProgress,
    );

    dynamic _bodyData;

    _bodyData = <String, dynamic>{
      if (enumFormStringArray != null) r'enum_form_string_array': encodeFormParameter(_serializers, enumFormStringArray, const FullType(BuiltList, [FullType(String)])),
      if (enumFormString != null) r'enum_form_string': encodeFormParameter(_serializers, enumFormString, const FullType(String)),
    };

    final _response = await _dio.request<dynamic>(
      _request.path,
      data: _bodyData,
      options: _request,
    );

    return _response;
  }

  /// Fake endpoint to test group parameters (optional)
  ///
  /// Fake endpoint to test group parameters (optional)
  Future<Response<void>> testGroupParameters(
    int requiredStringGroup,
    bool requiredBooleanGroup,
    int requiredInt64Group, { 
    int stringGroup,
    bool booleanGroup,
    int int64Group,
    CancelToken cancelToken,
    Map<String, dynamic> headers,
    Map<String, dynamic> extra,
    ValidateStatus validateStatus,
    ProgressCallback onSendProgress,
    ProgressCallback onReceiveProgress,
  }) async {
    final _request = RequestOptions(
      path: r'/fake',
      method: 'DELETE',
      headers: <String, dynamic>{
        r'required_boolean_group': requiredBooleanGroup,
        if (booleanGroup != null) r'boolean_group': booleanGroup,
        ...?headers,
      },
      queryParameters: <String, dynamic>{
        r'required_string_group': requiredStringGroup,
        r'required_int64_group': requiredInt64Group,
        if (stringGroup != null) r'string_group': stringGroup,
        if (int64Group != null) r'int64_group': int64Group,
      },
      extra: <String, dynamic>{
        'secure': <Map<String, String>>[
          {
            'type': 'http',
            'name': 'bearer_test',
          },
        ],
        ...?extra,
      },
      validateStatus: validateStatus,
      contentType: 'application/json',
      cancelToken: cancelToken,
      onSendProgress: onSendProgress,
      onReceiveProgress: onReceiveProgress,
    );

    dynamic _bodyData;

    final _response = await _dio.request<dynamic>(
      _request.path,
      data: _bodyData,
      options: _request,
    );

    return _response;
  }

  /// test inline additionalProperties
  ///
  /// 
  Future<Response<void>> testInlineAdditionalProperties(
    BuiltMap<String, String> requestBody, { 
    CancelToken cancelToken,
    Map<String, dynamic> headers,
    Map<String, dynamic> extra,
    ValidateStatus validateStatus,
    ProgressCallback onSendProgress,
    ProgressCallback onReceiveProgress,
  }) async {
    final _request = RequestOptions(
      path: r'/fake/inline-additionalProperties',
      method: 'POST',
      headers: <String, dynamic>{
        ...?headers,
      },
      extra: <String, dynamic>{
        'secure': <Map<String, String>>[],
        ...?extra,
      },
      validateStatus: validateStatus,
      contentType: 'application/json',
      cancelToken: cancelToken,
      onSendProgress: onSendProgress,
      onReceiveProgress: onReceiveProgress,
    );

    dynamic _bodyData;

    const _type = FullType(BuiltMap, [FullType(String), FullType(String)]);
    _bodyData = _serializers.serialize(requestBody, specifiedType: _type);

    final _response = await _dio.request<dynamic>(
      _request.path,
      data: _bodyData,
      options: _request,
    );

    return _response;
  }

  /// test json serialization of form data
  ///
  /// 
  Future<Response<void>> testJsonFormData(
    String param,
    String param2, { 
    CancelToken cancelToken,
    Map<String, dynamic> headers,
    Map<String, dynamic> extra,
    ValidateStatus validateStatus,
    ProgressCallback onSendProgress,
    ProgressCallback onReceiveProgress,
  }) async {
    final _request = RequestOptions(
      path: r'/fake/jsonFormData',
      method: 'GET',
      headers: <String, dynamic>{
        ...?headers,
      },
      extra: <String, dynamic>{
        'secure': <Map<String, String>>[],
        ...?extra,
      },
      validateStatus: validateStatus,
      contentType: 'application/x-www-form-urlencoded',
      cancelToken: cancelToken,
      onSendProgress: onSendProgress,
      onReceiveProgress: onReceiveProgress,
    );

    dynamic _bodyData;

    _bodyData = <String, dynamic>{
      r'param': encodeFormParameter(_serializers, param, const FullType(String)),
      r'param2': encodeFormParameter(_serializers, param2, const FullType(String)),
    };

    final _response = await _dio.request<dynamic>(
      _request.path,
      data: _bodyData,
      options: _request,
    );

    return _response;
  }

  /// 
  ///
  /// To test the collection format in query parameters
  Future<Response<void>> testQueryParameterCollectionFormat(
    BuiltList<String> pipe,
    BuiltList<String> ioutil,
    BuiltList<String> http,
    BuiltList<String> url,
    BuiltList<String> context,
    String allowEmpty, { 
    BuiltMap<String, String> language,
    CancelToken cancelToken,
    Map<String, dynamic> headers,
    Map<String, dynamic> extra,
    ValidateStatus validateStatus,
    ProgressCallback onSendProgress,
    ProgressCallback onReceiveProgress,
  }) async {
    final _request = RequestOptions(
      path: r'/fake/test-query-parameters',
      method: 'PUT',
      headers: <String, dynamic>{
        ...?headers,
      },
      queryParameters: <String, dynamic>{
        r'pipe': pipe,
        r'ioutil': ioutil,
        r'http': http,
        r'url': url,
        r'context': context,
        if (language != null) r'language': language,
        r'allowEmpty': allowEmpty,
      },
      extra: <String, dynamic>{
        'secure': <Map<String, String>>[],
        ...?extra,
      },
      validateStatus: validateStatus,
      contentType: 'application/json',
      cancelToken: cancelToken,
      onSendProgress: onSendProgress,
      onReceiveProgress: onReceiveProgress,
    );

    dynamic _bodyData;

    final _response = await _dio.request<dynamic>(
      _request.path,
      data: _bodyData,
      options: _request,
    );

    return _response;
  }

}

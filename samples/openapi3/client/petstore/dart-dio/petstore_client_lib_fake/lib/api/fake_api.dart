//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.6

// ignore_for_file: unused_import

import 'dart:async';
import 'dart:convert';
import 'package:dio/dio.dart';
import 'package:built_value/serializer.dart';

import 'package:openapi/model/file_schema_test_class.dart';
import 'package:openapi/model/outer_composite.dart';
import 'package:openapi/model/user.dart';
import 'package:openapi/model/health_check_result.dart';
import 'package:openapi/model/pet.dart';
import 'package:openapi/model/model_client.dart';
import 'dart:typed_data';
import 'package:built_collection/built_collection.dart';
import 'package:openapi/api_util.dart';

class FakeApi {
    final Dio _dio;
    Serializers _serializers;

    FakeApi(this._dio, this._serializers);

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
        final String _path = '/fake/health';

        final queryParams = <String, dynamic>{};
        final headerParams = <String, dynamic>{ 
            if (headers != null) ...headers,
        };
        dynamic bodyData;

        queryParams.removeWhere((key, dynamic value) => value == null);
        headerParams.removeWhere((key, dynamic value) => value == null);

        final contentTypes = <String>[];

        return _dio.request<dynamic>(
            _path,
            queryParameters: queryParams,
            data: bodyData,
            options: Options(
                method: 'get'.toUpperCase(),
                headers: headerParams,
                extra: <String, dynamic>{
                    'secure': <Map<String, String>>[],
                    if (extra != null) ...extra,
                },
                validateStatus: validateStatus,
                contentType: contentTypes.isNotEmpty ? contentTypes[0] : 'application/json',
            ),
            cancelToken: cancelToken,
            onSendProgress: onSendProgress,
            onReceiveProgress: onReceiveProgress,
        ).then((response) {
            final serializer = _serializers.serializerForType(HealthCheckResult) as Serializer<HealthCheckResult>;
            final data = _serializers.deserializeWith<HealthCheckResult>(
                serializer,
                response.data is String ? jsonDecode(response.data as String) : response.data,
            );

            return Response<HealthCheckResult>(
                data: data,
                headers: response.headers,
                request: response.request,
                redirects: response.redirects,
                statusCode: response.statusCode,
                statusMessage: response.statusMessage,
                extra: response.extra,
            );
        });
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
        final String _path = '/fake/http-signature-test';

        final queryParams = <String, dynamic>{};
        final headerParams = <String, dynamic>{ 
            if (headers != null) ...headers,
        };
        dynamic bodyData;

        headerParams[r'header_1'] = header1;
        queryParams[r'query_1'] = query1;
        queryParams.removeWhere((key, dynamic value) => value == null);
        headerParams.removeWhere((key, dynamic value) => value == null);

        final contentTypes = <String>[
            'application/json',
            'application/xml',
        ];

        final bodySerializer = _serializers.serializerForType(Pet) as Serializer<Pet>;
        final serializedBody = _serializers.serializeWith(bodySerializer, pet);
        final jsonpet = json.encode(serializedBody);
        bodyData = jsonpet;

        return _dio.request<dynamic>(
            _path,
            queryParameters: queryParams,
            data: bodyData,
            options: Options(
                method: 'get'.toUpperCase(),
                headers: headerParams,
                extra: <String, dynamic>{
                    'secure': <Map<String, String>>[
                        {
                            'type': 'http',
                            'name': 'http_signature_test',
                        },
                    ],
                    if (extra != null) ...extra,
                },
                validateStatus: validateStatus,
                contentType: contentTypes.isNotEmpty ? contentTypes[0] : 'application/json',
            ),
            cancelToken: cancelToken,
            onSendProgress: onSendProgress,
            onReceiveProgress: onReceiveProgress,
        );
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
        final String _path = '/fake/outer/boolean';

        final queryParams = <String, dynamic>{};
        final headerParams = <String, dynamic>{ 
            if (headers != null) ...headers,
        };
        dynamic bodyData;

        queryParams.removeWhere((key, dynamic value) => value == null);
        headerParams.removeWhere((key, dynamic value) => value == null);

        final contentTypes = <String>[
            'application/json',
        ];

        final serializedBody = body;
        final jsonbody = json.encode(serializedBody);
        bodyData = jsonbody;

        return _dio.request<dynamic>(
            _path,
            queryParameters: queryParams,
            data: bodyData,
            options: Options(
                method: 'post'.toUpperCase(),
                headers: headerParams,
                extra: <String, dynamic>{
                    'secure': <Map<String, String>>[],
                    if (extra != null) ...extra,
                },
                validateStatus: validateStatus,
                contentType: contentTypes.isNotEmpty ? contentTypes[0] : 'application/json',
            ),
            cancelToken: cancelToken,
            onSendProgress: onSendProgress,
            onReceiveProgress: onReceiveProgress,
        ).then((response) {
            final data = response.data as bool;

            return Response<bool>(
                data: data,
                headers: response.headers,
                request: response.request,
                redirects: response.redirects,
                statusCode: response.statusCode,
                statusMessage: response.statusMessage,
                extra: response.extra,
            );
        });
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
        final String _path = '/fake/outer/composite';

        final queryParams = <String, dynamic>{};
        final headerParams = <String, dynamic>{ 
            if (headers != null) ...headers,
        };
        dynamic bodyData;

        queryParams.removeWhere((key, dynamic value) => value == null);
        headerParams.removeWhere((key, dynamic value) => value == null);

        final contentTypes = <String>[
            'application/json',
        ];

        final bodySerializer = _serializers.serializerForType(OuterComposite) as Serializer<OuterComposite>;
        final serializedBody = _serializers.serializeWith(bodySerializer, outerComposite);
        final jsonouterComposite = json.encode(serializedBody);
        bodyData = jsonouterComposite;

        return _dio.request<dynamic>(
            _path,
            queryParameters: queryParams,
            data: bodyData,
            options: Options(
                method: 'post'.toUpperCase(),
                headers: headerParams,
                extra: <String, dynamic>{
                    'secure': <Map<String, String>>[],
                    if (extra != null) ...extra,
                },
                validateStatus: validateStatus,
                contentType: contentTypes.isNotEmpty ? contentTypes[0] : 'application/json',
            ),
            cancelToken: cancelToken,
            onSendProgress: onSendProgress,
            onReceiveProgress: onReceiveProgress,
        ).then((response) {
            final serializer = _serializers.serializerForType(OuterComposite) as Serializer<OuterComposite>;
            final data = _serializers.deserializeWith<OuterComposite>(
                serializer,
                response.data is String ? jsonDecode(response.data as String) : response.data,
            );

            return Response<OuterComposite>(
                data: data,
                headers: response.headers,
                request: response.request,
                redirects: response.redirects,
                statusCode: response.statusCode,
                statusMessage: response.statusMessage,
                extra: response.extra,
            );
        });
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
        final String _path = '/fake/outer/number';

        final queryParams = <String, dynamic>{};
        final headerParams = <String, dynamic>{ 
            if (headers != null) ...headers,
        };
        dynamic bodyData;

        queryParams.removeWhere((key, dynamic value) => value == null);
        headerParams.removeWhere((key, dynamic value) => value == null);

        final contentTypes = <String>[
            'application/json',
        ];

        final serializedBody = body;
        final jsonbody = json.encode(serializedBody);
        bodyData = jsonbody;

        return _dio.request<dynamic>(
            _path,
            queryParameters: queryParams,
            data: bodyData,
            options: Options(
                method: 'post'.toUpperCase(),
                headers: headerParams,
                extra: <String, dynamic>{
                    'secure': <Map<String, String>>[],
                    if (extra != null) ...extra,
                },
                validateStatus: validateStatus,
                contentType: contentTypes.isNotEmpty ? contentTypes[0] : 'application/json',
            ),
            cancelToken: cancelToken,
            onSendProgress: onSendProgress,
            onReceiveProgress: onReceiveProgress,
        ).then((response) {
            final data = response.data as num;

            return Response<num>(
                data: data,
                headers: response.headers,
                request: response.request,
                redirects: response.redirects,
                statusCode: response.statusCode,
                statusMessage: response.statusMessage,
                extra: response.extra,
            );
        });
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
        final String _path = '/fake/outer/string';

        final queryParams = <String, dynamic>{};
        final headerParams = <String, dynamic>{ 
            if (headers != null) ...headers,
        };
        dynamic bodyData;

        queryParams.removeWhere((key, dynamic value) => value == null);
        headerParams.removeWhere((key, dynamic value) => value == null);

        final contentTypes = <String>[
            'application/json',
        ];

        final serializedBody = body;
        final jsonbody = json.encode(serializedBody);
        bodyData = jsonbody;

        return _dio.request<dynamic>(
            _path,
            queryParameters: queryParams,
            data: bodyData,
            options: Options(
                method: 'post'.toUpperCase(),
                headers: headerParams,
                extra: <String, dynamic>{
                    'secure': <Map<String, String>>[],
                    if (extra != null) ...extra,
                },
                validateStatus: validateStatus,
                contentType: contentTypes.isNotEmpty ? contentTypes[0] : 'application/json',
            ),
            cancelToken: cancelToken,
            onSendProgress: onSendProgress,
            onReceiveProgress: onReceiveProgress,
        ).then((response) {
            final data = response.data as String;

            return Response<String>(
                data: data,
                headers: response.headers,
                request: response.request,
                redirects: response.redirects,
                statusCode: response.statusCode,
                statusMessage: response.statusMessage,
                extra: response.extra,
            );
        });
    }

    /// 
    ///
    /// For this test, the body for this request much reference a schema named `File`.
    Future<Response<void>> testBodyWithFileSchema(
        FileSchemaTestClass fileSchemaTestClass, { 
        CancelToken cancelToken,
        Map<String, dynamic> headers,
        Map<String, dynamic> extra,
        ValidateStatus validateStatus,
        ProgressCallback onSendProgress,
        ProgressCallback onReceiveProgress,
    }) async {
        final String _path = '/fake/body-with-file-schema';

        final queryParams = <String, dynamic>{};
        final headerParams = <String, dynamic>{ 
            if (headers != null) ...headers,
        };
        dynamic bodyData;

        queryParams.removeWhere((key, dynamic value) => value == null);
        headerParams.removeWhere((key, dynamic value) => value == null);

        final contentTypes = <String>[
            'application/json',
        ];

        final bodySerializer = _serializers.serializerForType(FileSchemaTestClass) as Serializer<FileSchemaTestClass>;
        final serializedBody = _serializers.serializeWith(bodySerializer, fileSchemaTestClass);
        final jsonfileSchemaTestClass = json.encode(serializedBody);
        bodyData = jsonfileSchemaTestClass;

        return _dio.request<dynamic>(
            _path,
            queryParameters: queryParams,
            data: bodyData,
            options: Options(
                method: 'put'.toUpperCase(),
                headers: headerParams,
                extra: <String, dynamic>{
                    'secure': <Map<String, String>>[],
                    if (extra != null) ...extra,
                },
                validateStatus: validateStatus,
                contentType: contentTypes.isNotEmpty ? contentTypes[0] : 'application/json',
            ),
            cancelToken: cancelToken,
            onSendProgress: onSendProgress,
            onReceiveProgress: onReceiveProgress,
        );
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
        final String _path = '/fake/body-with-query-params';

        final queryParams = <String, dynamic>{};
        final headerParams = <String, dynamic>{ 
            if (headers != null) ...headers,
        };
        dynamic bodyData;

        queryParams[r'query'] = query;
        queryParams.removeWhere((key, dynamic value) => value == null);
        headerParams.removeWhere((key, dynamic value) => value == null);

        final contentTypes = <String>[
            'application/json',
        ];

        final bodySerializer = _serializers.serializerForType(User) as Serializer<User>;
        final serializedBody = _serializers.serializeWith(bodySerializer, user);
        final jsonuser = json.encode(serializedBody);
        bodyData = jsonuser;

        return _dio.request<dynamic>(
            _path,
            queryParameters: queryParams,
            data: bodyData,
            options: Options(
                method: 'put'.toUpperCase(),
                headers: headerParams,
                extra: <String, dynamic>{
                    'secure': <Map<String, String>>[],
                    if (extra != null) ...extra,
                },
                validateStatus: validateStatus,
                contentType: contentTypes.isNotEmpty ? contentTypes[0] : 'application/json',
            ),
            cancelToken: cancelToken,
            onSendProgress: onSendProgress,
            onReceiveProgress: onReceiveProgress,
        );
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
        final String _path = '/fake';

        final queryParams = <String, dynamic>{};
        final headerParams = <String, dynamic>{ 
            if (headers != null) ...headers,
        };
        dynamic bodyData;

        queryParams.removeWhere((key, dynamic value) => value == null);
        headerParams.removeWhere((key, dynamic value) => value == null);

        final contentTypes = <String>[
            'application/json',
        ];

        final bodySerializer = _serializers.serializerForType(ModelClient) as Serializer<ModelClient>;
        final serializedBody = _serializers.serializeWith(bodySerializer, modelClient);
        final jsonmodelClient = json.encode(serializedBody);
        bodyData = jsonmodelClient;

        return _dio.request<dynamic>(
            _path,
            queryParameters: queryParams,
            data: bodyData,
            options: Options(
                method: 'patch'.toUpperCase(),
                headers: headerParams,
                extra: <String, dynamic>{
                    'secure': <Map<String, String>>[],
                    if (extra != null) ...extra,
                },
                validateStatus: validateStatus,
                contentType: contentTypes.isNotEmpty ? contentTypes[0] : 'application/json',
            ),
            cancelToken: cancelToken,
            onSendProgress: onSendProgress,
            onReceiveProgress: onReceiveProgress,
        ).then((response) {
            final serializer = _serializers.serializerForType(ModelClient) as Serializer<ModelClient>;
            final data = _serializers.deserializeWith<ModelClient>(
                serializer,
                response.data is String ? jsonDecode(response.data as String) : response.data,
            );

            return Response<ModelClient>(
                data: data,
                headers: response.headers,
                request: response.request,
                redirects: response.redirects,
                statusCode: response.statusCode,
                statusMessage: response.statusMessage,
                extra: response.extra,
            );
        });
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
        final String _path = '/fake';

        final queryParams = <String, dynamic>{};
        final headerParams = <String, dynamic>{ 
            if (headers != null) ...headers,
        };
        dynamic bodyData;

        queryParams.removeWhere((key, dynamic value) => value == null);
        headerParams.removeWhere((key, dynamic value) => value == null);

        final contentTypes = <String>[
            'application/x-www-form-urlencoded',
        ];

        final formData = <String, dynamic>{
            if (integer != null) r'integer': parameterToString(_serializers, integer),
            if (int32 != null) r'int32': parameterToString(_serializers, int32),
            if (int64 != null) r'int64': parameterToString(_serializers, int64),
            r'number': parameterToString(_serializers, number),
            if (float != null) r'float': parameterToString(_serializers, float),
            r'double': parameterToString(_serializers, double_),
            if (string != null) r'string': parameterToString(_serializers, string),
            r'pattern_without_delimiter': parameterToString(_serializers, patternWithoutDelimiter),
            r'byte': parameterToString(_serializers, byte),
            if (binary != null) r'binary': MultipartFile.fromBytes(binary, filename: r'binary'),
            if (date != null) r'date': parameterToString(_serializers, date),
            if (dateTime != null) r'dateTime': parameterToString(_serializers, dateTime),
            if (password != null) r'password': parameterToString(_serializers, password),
            if (callback != null) r'callback': parameterToString(_serializers, callback),
        };
        bodyData = formData;

        return _dio.request<dynamic>(
            _path,
            queryParameters: queryParams,
            data: bodyData,
            options: Options(
                method: 'post'.toUpperCase(),
                headers: headerParams,
                extra: <String, dynamic>{
                    'secure': <Map<String, String>>[
                        {
                            'type': 'http',
                            'name': 'http_basic_test',
                        },
                    ],
                    if (extra != null) ...extra,
                },
                validateStatus: validateStatus,
                contentType: contentTypes.isNotEmpty ? contentTypes[0] : 'application/json',
            ),
            cancelToken: cancelToken,
            onSendProgress: onSendProgress,
            onReceiveProgress: onReceiveProgress,
        );
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
        final String _path = '/fake';

        final queryParams = <String, dynamic>{};
        final headerParams = <String, dynamic>{ 
            if (headers != null) ...headers,
        };
        dynamic bodyData;

        headerParams[r'enum_header_string_array'] = enumHeaderStringArray;
        headerParams[r'enum_header_string'] = enumHeaderString;
        queryParams[r'enum_query_string_array'] = enumQueryStringArray;
        queryParams[r'enum_query_string'] = enumQueryString;
        queryParams[r'enum_query_integer'] = enumQueryInteger;
        queryParams[r'enum_query_double'] = enumQueryDouble;
        queryParams.removeWhere((key, dynamic value) => value == null);
        headerParams.removeWhere((key, dynamic value) => value == null);

        final contentTypes = <String>[
            'application/x-www-form-urlencoded',
        ];

        final formData = <String, dynamic>{
            if (enumFormStringArray != null) r'enum_form_string_array': parameterToString(_serializers, enumFormStringArray),
            if (enumFormString != null) r'enum_form_string': parameterToString(_serializers, enumFormString),
        };
        bodyData = formData;

        return _dio.request<dynamic>(
            _path,
            queryParameters: queryParams,
            data: bodyData,
            options: Options(
                method: 'get'.toUpperCase(),
                headers: headerParams,
                extra: <String, dynamic>{
                    'secure': <Map<String, String>>[],
                    if (extra != null) ...extra,
                },
                validateStatus: validateStatus,
                contentType: contentTypes.isNotEmpty ? contentTypes[0] : 'application/json',
            ),
            cancelToken: cancelToken,
            onSendProgress: onSendProgress,
            onReceiveProgress: onReceiveProgress,
        );
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
        final String _path = '/fake';

        final queryParams = <String, dynamic>{};
        final headerParams = <String, dynamic>{ 
            if (headers != null) ...headers,
        };
        dynamic bodyData;

        headerParams[r'required_boolean_group'] = requiredBooleanGroup;
        headerParams[r'boolean_group'] = booleanGroup;
        queryParams[r'required_string_group'] = requiredStringGroup;
        queryParams[r'required_int64_group'] = requiredInt64Group;
        queryParams[r'string_group'] = stringGroup;
        queryParams[r'int64_group'] = int64Group;
        queryParams.removeWhere((key, dynamic value) => value == null);
        headerParams.removeWhere((key, dynamic value) => value == null);

        final contentTypes = <String>[];

        return _dio.request<dynamic>(
            _path,
            queryParameters: queryParams,
            data: bodyData,
            options: Options(
                method: 'delete'.toUpperCase(),
                headers: headerParams,
                extra: <String, dynamic>{
                    'secure': <Map<String, String>>[
                        {
                            'type': 'http',
                            'name': 'bearer_test',
                        },
                    ],
                    if (extra != null) ...extra,
                },
                validateStatus: validateStatus,
                contentType: contentTypes.isNotEmpty ? contentTypes[0] : 'application/json',
            ),
            cancelToken: cancelToken,
            onSendProgress: onSendProgress,
            onReceiveProgress: onReceiveProgress,
        );
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
        final String _path = '/fake/inline-additionalProperties';

        final queryParams = <String, dynamic>{};
        final headerParams = <String, dynamic>{ 
            if (headers != null) ...headers,
        };
        dynamic bodyData;

        queryParams.removeWhere((key, dynamic value) => value == null);
        headerParams.removeWhere((key, dynamic value) => value == null);

        final contentTypes = <String>[
            'application/json',
        ];

        const type = FullType(BuiltMap, [FullType(String), FullType(String)]);
        final serializedBody = _serializers.serialize(requestBody, specifiedType: type);
        final jsonrequestBody = json.encode(serializedBody);
        bodyData = jsonrequestBody;

        return _dio.request<dynamic>(
            _path,
            queryParameters: queryParams,
            data: bodyData,
            options: Options(
                method: 'post'.toUpperCase(),
                headers: headerParams,
                extra: <String, dynamic>{
                    'secure': <Map<String, String>>[],
                    if (extra != null) ...extra,
                },
                validateStatus: validateStatus,
                contentType: contentTypes.isNotEmpty ? contentTypes[0] : 'application/json',
            ),
            cancelToken: cancelToken,
            onSendProgress: onSendProgress,
            onReceiveProgress: onReceiveProgress,
        );
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
        final String _path = '/fake/jsonFormData';

        final queryParams = <String, dynamic>{};
        final headerParams = <String, dynamic>{ 
            if (headers != null) ...headers,
        };
        dynamic bodyData;

        queryParams.removeWhere((key, dynamic value) => value == null);
        headerParams.removeWhere((key, dynamic value) => value == null);

        final contentTypes = <String>[
            'application/x-www-form-urlencoded',
        ];

        final formData = <String, dynamic>{
            r'param': parameterToString(_serializers, param),
            r'param2': parameterToString(_serializers, param2),
        };
        bodyData = formData;

        return _dio.request<dynamic>(
            _path,
            queryParameters: queryParams,
            data: bodyData,
            options: Options(
                method: 'get'.toUpperCase(),
                headers: headerParams,
                extra: <String, dynamic>{
                    'secure': <Map<String, String>>[],
                    if (extra != null) ...extra,
                },
                validateStatus: validateStatus,
                contentType: contentTypes.isNotEmpty ? contentTypes[0] : 'application/json',
            ),
            cancelToken: cancelToken,
            onSendProgress: onSendProgress,
            onReceiveProgress: onReceiveProgress,
        );
    }

    /// 
    ///
    /// To test the collection format in query parameters
    Future<Response<void>> testQueryParameterCollectionFormat(
        BuiltList<String> pipe,
        BuiltList<String> ioutil,
        BuiltList<String> http,
        BuiltList<String> url,
        BuiltList<String> context, { 
        CancelToken cancelToken,
        Map<String, dynamic> headers,
        Map<String, dynamic> extra,
        ValidateStatus validateStatus,
        ProgressCallback onSendProgress,
        ProgressCallback onReceiveProgress,
    }) async {
        final String _path = '/fake/test-query-paramters';

        final queryParams = <String, dynamic>{};
        final headerParams = <String, dynamic>{ 
            if (headers != null) ...headers,
        };
        dynamic bodyData;

        queryParams[r'pipe'] = pipe;
        queryParams[r'ioutil'] = ioutil;
        queryParams[r'http'] = http;
        queryParams[r'url'] = url;
        queryParams[r'context'] = context;
        queryParams.removeWhere((key, dynamic value) => value == null);
        headerParams.removeWhere((key, dynamic value) => value == null);

        final contentTypes = <String>[];

        return _dio.request<dynamic>(
            _path,
            queryParameters: queryParams,
            data: bodyData,
            options: Options(
                method: 'put'.toUpperCase(),
                headers: headerParams,
                extra: <String, dynamic>{
                    'secure': <Map<String, String>>[],
                    if (extra != null) ...extra,
                },
                validateStatus: validateStatus,
                contentType: contentTypes.isNotEmpty ? contentTypes[0] : 'application/json',
            ),
            cancelToken: cancelToken,
            onSendProgress: onSendProgress,
            onReceiveProgress: onReceiveProgress,
        );
    }

}

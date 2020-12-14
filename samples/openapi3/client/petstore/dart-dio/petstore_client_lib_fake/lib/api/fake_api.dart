import 'dart:async';
import 'dart:io';
import 'dart:convert';
import 'package:dio/dio.dart';
import 'package:built_value/serializer.dart';

import 'package:openapi/model/client.dart';
import 'package:openapi/model/file_schema_test_class.dart';
import 'package:openapi/model/outer_composite.dart';
import 'package:openapi/model/user.dart';
import 'package:openapi/model/health_check_result.dart';
import 'package:openapi/model/pet.dart';
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
        Future<Response<HealthCheckResult>>fakeHealthGet({ CancelToken cancelToken, Map<String, String> headers, ProgressCallback onSendProgress, ProgressCallback onReceiveProgress,}) async {

        String _path = "/fake/health";

        Map<String, dynamic> queryParams = {};
        Map<String, String> headerParams = Map.from(headers ?? {});
        dynamic bodyData;

        queryParams.removeWhere((key, value) => value == null);
        headerParams.removeWhere((key, value) => value == null);

        List<String> contentTypes = [];



            return _dio.request(
            _path,
            queryParameters: queryParams,
            data: bodyData,
            options: Options(
            method: 'get'.toUpperCase(),
            headers: headerParams,
            extra: {
                'secure': [],
            },
            contentType: contentTypes.isNotEmpty ? contentTypes[0] : "application/json",
            ),
            cancelToken: cancelToken,
            onSendProgress: onSendProgress,
            onReceiveProgress: onReceiveProgress,
            ).then((response) {

            final serializer = _serializers.serializerForType(HealthCheckResult);
            final data = _serializers.deserializeWith<HealthCheckResult>(serializer, response.data is String ? jsonDecode(response.data) : response.data);

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
        Future<Response>fakeHttpSignatureTest(Pet pet,{ String query1,String header1,CancelToken cancelToken, Map<String, String> headers, ProgressCallback onSendProgress, ProgressCallback onReceiveProgress,}) async {

        String _path = "/fake/http-signature-test";

        Map<String, dynamic> queryParams = {};
        Map<String, String> headerParams = Map.from(headers ?? {});
        dynamic bodyData;

                headerParams[r'header_1'] = header1;
                queryParams[r'query_1'] = query1;
        queryParams.removeWhere((key, value) => value == null);
        headerParams.removeWhere((key, value) => value == null);

        List<String> contentTypes = ["application/json","application/xml"];


            final serializedBody = _serializers.serialize(pet);
            final jsonpet = json.encode(serializedBody);
            bodyData = jsonpet;

            return _dio.request(
            _path,
            queryParameters: queryParams,
            data: bodyData,
            options: Options(
            method: 'get'.toUpperCase(),
            headers: headerParams,
            extra: {
                'secure': [ {"type": "http", "name": "http_signature_test" }],
            },
            contentType: contentTypes.isNotEmpty ? contentTypes[0] : "application/json",
            ),
            cancelToken: cancelToken,
            onSendProgress: onSendProgress,
            onReceiveProgress: onReceiveProgress,
            );
            }
        /// 
        ///
        /// Test serialization of outer boolean types
        Future<Response<bool>>fakeOuterBooleanSerialize({ bool body,CancelToken cancelToken, Map<String, String> headers, ProgressCallback onSendProgress, ProgressCallback onReceiveProgress,}) async {

        String _path = "/fake/outer/boolean";

        Map<String, dynamic> queryParams = {};
        Map<String, String> headerParams = Map.from(headers ?? {});
        dynamic bodyData;

        queryParams.removeWhere((key, value) => value == null);
        headerParams.removeWhere((key, value) => value == null);

        List<String> contentTypes = ["application/json"];


            final serializedBody = _serializers.serialize(body);
            final jsonbody = json.encode(serializedBody);
            bodyData = jsonbody;

            return _dio.request(
            _path,
            queryParameters: queryParams,
            data: bodyData,
            options: Options(
            method: 'post'.toUpperCase(),
            headers: headerParams,
            extra: {
                'secure': [],
            },
            contentType: contentTypes.isNotEmpty ? contentTypes[0] : "application/json",
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
        Future<Response<OuterComposite>>fakeOuterCompositeSerialize({ OuterComposite outerComposite,CancelToken cancelToken, Map<String, String> headers, ProgressCallback onSendProgress, ProgressCallback onReceiveProgress,}) async {

        String _path = "/fake/outer/composite";

        Map<String, dynamic> queryParams = {};
        Map<String, String> headerParams = Map.from(headers ?? {});
        dynamic bodyData;

        queryParams.removeWhere((key, value) => value == null);
        headerParams.removeWhere((key, value) => value == null);

        List<String> contentTypes = ["application/json"];


            final serializedBody = _serializers.serialize(outerComposite);
            final jsonouterComposite = json.encode(serializedBody);
            bodyData = jsonouterComposite;

            return _dio.request(
            _path,
            queryParameters: queryParams,
            data: bodyData,
            options: Options(
            method: 'post'.toUpperCase(),
            headers: headerParams,
            extra: {
                'secure': [],
            },
            contentType: contentTypes.isNotEmpty ? contentTypes[0] : "application/json",
            ),
            cancelToken: cancelToken,
            onSendProgress: onSendProgress,
            onReceiveProgress: onReceiveProgress,
            ).then((response) {

            final serializer = _serializers.serializerForType(OuterComposite);
            final data = _serializers.deserializeWith<OuterComposite>(serializer, response.data is String ? jsonDecode(response.data) : response.data);

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
        Future<Response<num>>fakeOuterNumberSerialize({ num body,CancelToken cancelToken, Map<String, String> headers, ProgressCallback onSendProgress, ProgressCallback onReceiveProgress,}) async {

        String _path = "/fake/outer/number";

        Map<String, dynamic> queryParams = {};
        Map<String, String> headerParams = Map.from(headers ?? {});
        dynamic bodyData;

        queryParams.removeWhere((key, value) => value == null);
        headerParams.removeWhere((key, value) => value == null);

        List<String> contentTypes = ["application/json"];


            final serializedBody = _serializers.serialize(body);
            final jsonbody = json.encode(serializedBody);
            bodyData = jsonbody;

            return _dio.request(
            _path,
            queryParameters: queryParams,
            data: bodyData,
            options: Options(
            method: 'post'.toUpperCase(),
            headers: headerParams,
            extra: {
                'secure': [],
            },
            contentType: contentTypes.isNotEmpty ? contentTypes[0] : "application/json",
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
        Future<Response<String>>fakeOuterStringSerialize({ String body,CancelToken cancelToken, Map<String, String> headers, ProgressCallback onSendProgress, ProgressCallback onReceiveProgress,}) async {

        String _path = "/fake/outer/string";

        Map<String, dynamic> queryParams = {};
        Map<String, String> headerParams = Map.from(headers ?? {});
        dynamic bodyData;

        queryParams.removeWhere((key, value) => value == null);
        headerParams.removeWhere((key, value) => value == null);

        List<String> contentTypes = ["application/json"];


            final serializedBody = _serializers.serialize(body);
            final jsonbody = json.encode(serializedBody);
            bodyData = jsonbody;

            return _dio.request(
            _path,
            queryParameters: queryParams,
            data: bodyData,
            options: Options(
            method: 'post'.toUpperCase(),
            headers: headerParams,
            extra: {
                'secure': [],
            },
            contentType: contentTypes.isNotEmpty ? contentTypes[0] : "application/json",
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
        /// For this test, the body for this request much reference a schema named &#x60;File&#x60;.
        Future<Response>testBodyWithFileSchema(FileSchemaTestClass fileSchemaTestClass,{ CancelToken cancelToken, Map<String, String> headers, ProgressCallback onSendProgress, ProgressCallback onReceiveProgress,}) async {

        String _path = "/fake/body-with-file-schema";

        Map<String, dynamic> queryParams = {};
        Map<String, String> headerParams = Map.from(headers ?? {});
        dynamic bodyData;

        queryParams.removeWhere((key, value) => value == null);
        headerParams.removeWhere((key, value) => value == null);

        List<String> contentTypes = ["application/json"];


            final serializedBody = _serializers.serialize(fileSchemaTestClass);
            final jsonfileSchemaTestClass = json.encode(serializedBody);
            bodyData = jsonfileSchemaTestClass;

            return _dio.request(
            _path,
            queryParameters: queryParams,
            data: bodyData,
            options: Options(
            method: 'put'.toUpperCase(),
            headers: headerParams,
            extra: {
                'secure': [],
            },
            contentType: contentTypes.isNotEmpty ? contentTypes[0] : "application/json",
            ),
            cancelToken: cancelToken,
            onSendProgress: onSendProgress,
            onReceiveProgress: onReceiveProgress,
            );
            }
        /// 
        ///
        /// 
        Future<Response>testBodyWithQueryParams(String query,User user,{ CancelToken cancelToken, Map<String, String> headers, ProgressCallback onSendProgress, ProgressCallback onReceiveProgress,}) async {

        String _path = "/fake/body-with-query-params";

        Map<String, dynamic> queryParams = {};
        Map<String, String> headerParams = Map.from(headers ?? {});
        dynamic bodyData;

                queryParams[r'query'] = query;
        queryParams.removeWhere((key, value) => value == null);
        headerParams.removeWhere((key, value) => value == null);

        List<String> contentTypes = ["application/json"];


            final serializedBody = _serializers.serialize(user);
            final jsonuser = json.encode(serializedBody);
            bodyData = jsonuser;

            return _dio.request(
            _path,
            queryParameters: queryParams,
            data: bodyData,
            options: Options(
            method: 'put'.toUpperCase(),
            headers: headerParams,
            extra: {
                'secure': [],
            },
            contentType: contentTypes.isNotEmpty ? contentTypes[0] : "application/json",
            ),
            cancelToken: cancelToken,
            onSendProgress: onSendProgress,
            onReceiveProgress: onReceiveProgress,
            );
            }
        /// To test \&quot;client\&quot; model
        ///
        /// To test \&quot;client\&quot; model
        Future<Response<Client>>testClientModel(Client client,{ CancelToken cancelToken, Map<String, String> headers, ProgressCallback onSendProgress, ProgressCallback onReceiveProgress,}) async {

        String _path = "/fake";

        Map<String, dynamic> queryParams = {};
        Map<String, String> headerParams = Map.from(headers ?? {});
        dynamic bodyData;

        queryParams.removeWhere((key, value) => value == null);
        headerParams.removeWhere((key, value) => value == null);

        List<String> contentTypes = ["application/json"];


            final serializedBody = _serializers.serialize(client);
            final jsonclient = json.encode(serializedBody);
            bodyData = jsonclient;

            return _dio.request(
            _path,
            queryParameters: queryParams,
            data: bodyData,
            options: Options(
            method: 'patch'.toUpperCase(),
            headers: headerParams,
            extra: {
                'secure': [],
            },
            contentType: contentTypes.isNotEmpty ? contentTypes[0] : "application/json",
            ),
            cancelToken: cancelToken,
            onSendProgress: onSendProgress,
            onReceiveProgress: onReceiveProgress,
            ).then((response) {

            final serializer = _serializers.serializerForType(Client);
            final data = _serializers.deserializeWith<Client>(serializer, response.data is String ? jsonDecode(response.data) : response.data);

            return Response<Client>(
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
        Future<Response>testEndpointParameters(num number,double double_,String patternWithoutDelimiter,String byte,{ int integer,int int32,int int64,double float,String string,Uint8List binary,DateTime date,DateTime dateTime,String password,String callback,CancelToken cancelToken, Map<String, String> headers, ProgressCallback onSendProgress, ProgressCallback onReceiveProgress,}) async {

        String _path = "/fake";

        Map<String, dynamic> queryParams = {};
        Map<String, String> headerParams = Map.from(headers ?? {});
        dynamic bodyData;

        queryParams.removeWhere((key, value) => value == null);
        headerParams.removeWhere((key, value) => value == null);

        List<String> contentTypes = ["application/x-www-form-urlencoded"];

        Map<String, dynamic> formData = {};
            formData[r'integer'] = parameterToString(_serializers, integer);
            formData[r'int32'] = parameterToString(_serializers, int32);
            formData[r'int64'] = parameterToString(_serializers, int64);
            formData[r'number'] = parameterToString(_serializers, number);
            formData[r'float'] = parameterToString(_serializers, float);
            formData[r'double'] = parameterToString(_serializers, double_);
            formData[r'string'] = parameterToString(_serializers, string);
            formData[r'pattern_without_delimiter'] = parameterToString(_serializers, patternWithoutDelimiter);
            formData[r'byte'] = parameterToString(_serializers, byte);
            formData[r'binary'] = parameterToString(_serializers, binary);
            formData[r'date'] = parameterToString(_serializers, date);
            formData[r'dateTime'] = parameterToString(_serializers, dateTime);
            formData[r'password'] = parameterToString(_serializers, password);
            formData[r'callback'] = parameterToString(_serializers, callback);
        bodyData = FormData.fromMap(formData);


            return _dio.request(
            _path,
            queryParameters: queryParams,
            data: bodyData,
            options: Options(
            method: 'post'.toUpperCase(),
            headers: headerParams,
            extra: {
                'secure': [ {"type": "http", "name": "http_basic_test" }],
            },
            contentType: contentTypes.isNotEmpty ? contentTypes[0] : "application/json",
            ),
            cancelToken: cancelToken,
            onSendProgress: onSendProgress,
            onReceiveProgress: onReceiveProgress,
            );
            }
        /// To test enum parameters
        ///
        /// To test enum parameters
        Future<Response>testEnumParameters({ BuiltList<String> enumHeaderStringArray,String enumHeaderString,BuiltList<String> enumQueryStringArray,String enumQueryString,int enumQueryInteger,double enumQueryDouble,BuiltList<String> enumFormStringArray,String enumFormString,CancelToken cancelToken, Map<String, String> headers, ProgressCallback onSendProgress, ProgressCallback onReceiveProgress,}) async {

        String _path = "/fake";

        Map<String, dynamic> queryParams = {};
        Map<String, String> headerParams = Map.from(headers ?? {});
        dynamic bodyData;

                headerParams[r'enum_header_string_array'] = enumHeaderStringArray;
                headerParams[r'enum_header_string'] = enumHeaderString;
                queryParams[r'enum_query_string_array'] = enumQueryStringArray;
                queryParams[r'enum_query_string'] = enumQueryString;
                queryParams[r'enum_query_integer'] = enumQueryInteger;
                queryParams[r'enum_query_double'] = enumQueryDouble;
        queryParams.removeWhere((key, value) => value == null);
        headerParams.removeWhere((key, value) => value == null);

        List<String> contentTypes = ["application/x-www-form-urlencoded"];

        Map<String, dynamic> formData = {};
            formData[r'enum_form_string_array'] = parameterToString(_serializers, enumFormStringArray);
            formData[r'enum_form_string'] = parameterToString(_serializers, enumFormString);
        bodyData = FormData.fromMap(formData);


            return _dio.request(
            _path,
            queryParameters: queryParams,
            data: bodyData,
            options: Options(
            method: 'get'.toUpperCase(),
            headers: headerParams,
            extra: {
                'secure': [],
            },
            contentType: contentTypes.isNotEmpty ? contentTypes[0] : "application/json",
            ),
            cancelToken: cancelToken,
            onSendProgress: onSendProgress,
            onReceiveProgress: onReceiveProgress,
            );
            }
        /// Fake endpoint to test group parameters (optional)
        ///
        /// Fake endpoint to test group parameters (optional)
        Future<Response>testGroupParameters(int requiredStringGroup,bool requiredBooleanGroup,int requiredInt64Group,{ int stringGroup,bool booleanGroup,int int64Group,CancelToken cancelToken, Map<String, String> headers, ProgressCallback onSendProgress, ProgressCallback onReceiveProgress,}) async {

        String _path = "/fake";

        Map<String, dynamic> queryParams = {};
        Map<String, String> headerParams = Map.from(headers ?? {});
        dynamic bodyData;

                headerParams[r'required_boolean_group'] = requiredBooleanGroup;
                headerParams[r'boolean_group'] = booleanGroup;
                queryParams[r'required_string_group'] = requiredStringGroup;
                queryParams[r'required_int64_group'] = requiredInt64Group;
                queryParams[r'string_group'] = stringGroup;
                queryParams[r'int64_group'] = int64Group;
        queryParams.removeWhere((key, value) => value == null);
        headerParams.removeWhere((key, value) => value == null);

        List<String> contentTypes = [];



            return _dio.request(
            _path,
            queryParameters: queryParams,
            data: bodyData,
            options: Options(
            method: 'delete'.toUpperCase(),
            headers: headerParams,
            extra: {
                'secure': [ {"type": "http", "name": "bearer_test" }],
            },
            contentType: contentTypes.isNotEmpty ? contentTypes[0] : "application/json",
            ),
            cancelToken: cancelToken,
            onSendProgress: onSendProgress,
            onReceiveProgress: onReceiveProgress,
            );
            }
        /// test inline additionalProperties
        ///
        /// 
        Future<Response>testInlineAdditionalProperties(BuiltMap<String, String> requestBody,{ CancelToken cancelToken, Map<String, String> headers, ProgressCallback onSendProgress, ProgressCallback onReceiveProgress,}) async {

        String _path = "/fake/inline-additionalProperties";

        Map<String, dynamic> queryParams = {};
        Map<String, String> headerParams = Map.from(headers ?? {});
        dynamic bodyData;

        queryParams.removeWhere((key, value) => value == null);
        headerParams.removeWhere((key, value) => value == null);

        List<String> contentTypes = ["application/json"];


            final serializedBody = _serializers.serialize(requestBody);
            final jsonrequestBody = json.encode(serializedBody);
            bodyData = jsonrequestBody;

            return _dio.request(
            _path,
            queryParameters: queryParams,
            data: bodyData,
            options: Options(
            method: 'post'.toUpperCase(),
            headers: headerParams,
            extra: {
                'secure': [],
            },
            contentType: contentTypes.isNotEmpty ? contentTypes[0] : "application/json",
            ),
            cancelToken: cancelToken,
            onSendProgress: onSendProgress,
            onReceiveProgress: onReceiveProgress,
            );
            }
        /// test json serialization of form data
        ///
        /// 
        Future<Response>testJsonFormData(String param,String param2,{ CancelToken cancelToken, Map<String, String> headers, ProgressCallback onSendProgress, ProgressCallback onReceiveProgress,}) async {

        String _path = "/fake/jsonFormData";

        Map<String, dynamic> queryParams = {};
        Map<String, String> headerParams = Map.from(headers ?? {});
        dynamic bodyData;

        queryParams.removeWhere((key, value) => value == null);
        headerParams.removeWhere((key, value) => value == null);

        List<String> contentTypes = ["application/x-www-form-urlencoded"];

        Map<String, dynamic> formData = {};
            formData[r'param'] = parameterToString(_serializers, param);
            formData[r'param2'] = parameterToString(_serializers, param2);
        bodyData = FormData.fromMap(formData);


            return _dio.request(
            _path,
            queryParameters: queryParams,
            data: bodyData,
            options: Options(
            method: 'get'.toUpperCase(),
            headers: headerParams,
            extra: {
                'secure': [],
            },
            contentType: contentTypes.isNotEmpty ? contentTypes[0] : "application/json",
            ),
            cancelToken: cancelToken,
            onSendProgress: onSendProgress,
            onReceiveProgress: onReceiveProgress,
            );
            }
        /// 
        ///
        /// To test the collection format in query parameters
        Future<Response>testQueryParameterCollectionFormat(BuiltList<String> pipe,BuiltList<String> ioutil,BuiltList<String> http,BuiltList<String> url,BuiltList<String> context,{ CancelToken cancelToken, Map<String, String> headers, ProgressCallback onSendProgress, ProgressCallback onReceiveProgress,}) async {

        String _path = "/fake/test-query-paramters";

        Map<String, dynamic> queryParams = {};
        Map<String, String> headerParams = Map.from(headers ?? {});
        dynamic bodyData;

                queryParams[r'pipe'] = pipe;
                queryParams[r'ioutil'] = ioutil;
                queryParams[r'http'] = http;
                queryParams[r'url'] = url;
                queryParams[r'context'] = context;
        queryParams.removeWhere((key, value) => value == null);
        headerParams.removeWhere((key, value) => value == null);

        List<String> contentTypes = [];



            return _dio.request(
            _path,
            queryParameters: queryParams,
            data: bodyData,
            options: Options(
            method: 'put'.toUpperCase(),
            headers: headerParams,
            extra: {
                'secure': [],
            },
            contentType: contentTypes.isNotEmpty ? contentTypes[0] : "application/json",
            ),
            cancelToken: cancelToken,
            onSendProgress: onSendProgress,
            onReceiveProgress: onReceiveProgress,
            );
            }
        }

import 'dart:async';
import 'dart:io';
import 'dart:convert';
import 'package:dio/dio.dart';
import 'package:built_collection/built_collection.dart';
import 'package:built_value/serializer.dart';

import 'package:openapi/model/pet.dart';
import 'package:openapi/model/api_response.dart';
import 'dart:typed_data';
import 'package:openapi/api_util.dart';

class PetApi {
    final Dio _dio;
    Serializers _serializers;

    PetApi(this._dio, this._serializers);

        /// Add a new pet to the store
        ///
        /// 
        Future<Response>addPet(Pet body,{ CancelToken cancelToken, Map<String, String> headers, ProgressCallback onSendProgress, ProgressCallback onReceiveProgress,}) async {

        String _path = "/pet";

        Map<String, dynamic> queryParams = {};
        Map<String, String> headerParams = Map.from(headers ?? {});
        dynamic bodyData;

        queryParams.removeWhere((key, value) => value == null);
        headerParams.removeWhere((key, value) => value == null);

        List<String> contentTypes = ["application/json","application/xml"];


            var serializedBody = _serializers.serialize(body);
            var jsonbody = json.encode(serializedBody);
            bodyData = jsonbody;

            return _dio.request(
            _path,
            queryParameters: queryParams,
            data: bodyData,
            options: Options(
            method: 'post'.toUpperCase(),
            headers: headerParams,
            extra: {
                'secure': [ {"type": "oauth2", "name": "petstore_auth" }],
            },
            contentType: contentTypes.isNotEmpty ? contentTypes[0] : "application/json",
            ),
            cancelToken: cancelToken,
            onSendProgress: onSendProgress,
            onReceiveProgress: onReceiveProgress,
            );
            }
        /// Deletes a pet
        ///
        /// 
        Future<Response>deletePet(int petId,{ String apiKey,CancelToken cancelToken, Map<String, String> headers, ProgressCallback onSendProgress, ProgressCallback onReceiveProgress,}) async {

        String _path = "/pet/{petId}".replaceAll("{" r'petId' "}", petId.toString());

        Map<String, dynamic> queryParams = {};
        Map<String, String> headerParams = Map.from(headers ?? {});
        dynamic bodyData;

                headerParams[r'api_key'] = apiKey;
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
                'secure': [ {"type": "oauth2", "name": "petstore_auth" }],
            },
            contentType: contentTypes.isNotEmpty ? contentTypes[0] : "application/json",
            ),
            cancelToken: cancelToken,
            onSendProgress: onSendProgress,
            onReceiveProgress: onReceiveProgress,
            );
            }
        /// Finds Pets by status
        ///
        /// Multiple status values can be provided with comma separated strings
        Future<Response<List<Pet>>>findPetsByStatus(List<String> status,{ CancelToken cancelToken, Map<String, String> headers, ProgressCallback onSendProgress, ProgressCallback onReceiveProgress,}) async {

        String _path = "/pet/findByStatus";

        Map<String, dynamic> queryParams = {};
        Map<String, String> headerParams = Map.from(headers ?? {});
        dynamic bodyData;

                queryParams[r'status'] = status;
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
                'secure': [ {"type": "oauth2", "name": "petstore_auth" }],
            },
            contentType: contentTypes.isNotEmpty ? contentTypes[0] : "application/json",
            ),
            cancelToken: cancelToken,
            onSendProgress: onSendProgress,
            onReceiveProgress: onReceiveProgress,
            ).then((response) {

                final FullType type = const FullType(BuiltList, const [const FullType(Pet)]);
                BuiltList<Pet> dataList = _serializers.deserialize(response.data is String ? jsonDecode(response.data) : response.data, specifiedType: type);
                var data = dataList.toList();

            return Response<List<Pet>>(
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
        /// Finds Pets by tags
        ///
        /// Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
        Future<Response<List<Pet>>>findPetsByTags(List<String> tags,{ CancelToken cancelToken, Map<String, String> headers, ProgressCallback onSendProgress, ProgressCallback onReceiveProgress,}) async {

        String _path = "/pet/findByTags";

        Map<String, dynamic> queryParams = {};
        Map<String, String> headerParams = Map.from(headers ?? {});
        dynamic bodyData;

                queryParams[r'tags'] = tags;
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
                'secure': [ {"type": "oauth2", "name": "petstore_auth" }],
            },
            contentType: contentTypes.isNotEmpty ? contentTypes[0] : "application/json",
            ),
            cancelToken: cancelToken,
            onSendProgress: onSendProgress,
            onReceiveProgress: onReceiveProgress,
            ).then((response) {

                final FullType type = const FullType(BuiltList, const [const FullType(Pet)]);
                BuiltList<Pet> dataList = _serializers.deserialize(response.data is String ? jsonDecode(response.data) : response.data, specifiedType: type);
                var data = dataList.toList();

            return Response<List<Pet>>(
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
        /// Find pet by ID
        ///
        /// Returns a single pet
        Future<Response<Pet>>getPetById(int petId,{ CancelToken cancelToken, Map<String, String> headers, ProgressCallback onSendProgress, ProgressCallback onReceiveProgress,}) async {

        String _path = "/pet/{petId}".replaceAll("{" r'petId' "}", petId.toString());

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
                'secure': [ {"type": "apiKey", "name": "api_key", "keyName": "api_key", "where": "header" }],
            },
            contentType: contentTypes.isNotEmpty ? contentTypes[0] : "application/json",
            ),
            cancelToken: cancelToken,
            onSendProgress: onSendProgress,
            onReceiveProgress: onReceiveProgress,
            ).then((response) {

                var serializer = _serializers.serializerForType(Pet);
                var data = _serializers.deserializeWith<Pet>(serializer, response.data is String ? jsonDecode(response.data) : response.data);

            return Response<Pet>(
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
        /// Update an existing pet
        ///
        /// 
        Future<Response>updatePet(Pet body,{ CancelToken cancelToken, Map<String, String> headers, ProgressCallback onSendProgress, ProgressCallback onReceiveProgress,}) async {

        String _path = "/pet";

        Map<String, dynamic> queryParams = {};
        Map<String, String> headerParams = Map.from(headers ?? {});
        dynamic bodyData;

        queryParams.removeWhere((key, value) => value == null);
        headerParams.removeWhere((key, value) => value == null);

        List<String> contentTypes = ["application/json","application/xml"];


            var serializedBody = _serializers.serialize(body);
            var jsonbody = json.encode(serializedBody);
            bodyData = jsonbody;

            return _dio.request(
            _path,
            queryParameters: queryParams,
            data: bodyData,
            options: Options(
            method: 'put'.toUpperCase(),
            headers: headerParams,
            extra: {
                'secure': [ {"type": "oauth2", "name": "petstore_auth" }],
            },
            contentType: contentTypes.isNotEmpty ? contentTypes[0] : "application/json",
            ),
            cancelToken: cancelToken,
            onSendProgress: onSendProgress,
            onReceiveProgress: onReceiveProgress,
            );
            }
        /// Updates a pet in the store with form data
        ///
        /// 
        Future<Response>updatePetWithForm(int petId,{ String name,String status,CancelToken cancelToken, Map<String, String> headers, ProgressCallback onSendProgress, ProgressCallback onReceiveProgress,}) async {

        String _path = "/pet/{petId}".replaceAll("{" r'petId' "}", petId.toString());

        Map<String, dynamic> queryParams = {};
        Map<String, String> headerParams = Map.from(headers ?? {});
        dynamic bodyData;

        queryParams.removeWhere((key, value) => value == null);
        headerParams.removeWhere((key, value) => value == null);

        List<String> contentTypes = ["application/x-www-form-urlencoded"];

        Map<String, dynamic> formData = {};
            formData['name'] = parameterToString(_serializers, name);
            formData['status'] = parameterToString(_serializers, status);
        bodyData = FormData.fromMap(formData);


            return _dio.request(
            _path,
            queryParameters: queryParams,
            data: bodyData,
            options: Options(
            method: 'post'.toUpperCase(),
            headers: headerParams,
            extra: {
                'secure': [ {"type": "oauth2", "name": "petstore_auth" }],
            },
            contentType: contentTypes.isNotEmpty ? contentTypes[0] : "application/json",
            ),
            cancelToken: cancelToken,
            onSendProgress: onSendProgress,
            onReceiveProgress: onReceiveProgress,
            );
            }
        /// uploads an image
        ///
        /// 
        Future<Response<ApiResponse>>uploadFile(int petId,{ String additionalMetadata,Uint8List file,CancelToken cancelToken, Map<String, String> headers, ProgressCallback onSendProgress, ProgressCallback onReceiveProgress,}) async {

        String _path = "/pet/{petId}/uploadImage".replaceAll("{" r'petId' "}", petId.toString());

        Map<String, dynamic> queryParams = {};
        Map<String, String> headerParams = Map.from(headers ?? {});
        dynamic bodyData;

        queryParams.removeWhere((key, value) => value == null);
        headerParams.removeWhere((key, value) => value == null);

        List<String> contentTypes = ["multipart/form-data"];

        Map<String, dynamic> formData = {};
                if (additionalMetadata != null) {
                    formData[r'additionalMetadata'] = parameterToString(_serializers, additionalMetadata);
                }
                if (file != null) {
                    formData[r'file'] = MultipartFile.fromBytes(file, filename: r'file');
                }
        bodyData = FormData.fromMap(formData);


            return _dio.request(
            _path,
            queryParameters: queryParams,
            data: bodyData,
            options: Options(
            method: 'post'.toUpperCase(),
            headers: headerParams,
            extra: {
                'secure': [ {"type": "oauth2", "name": "petstore_auth" }],
            },
            contentType: contentTypes.isNotEmpty ? contentTypes[0] : "application/json",
            ),
            cancelToken: cancelToken,
            onSendProgress: onSendProgress,
            onReceiveProgress: onReceiveProgress,
            ).then((response) {

                var serializer = _serializers.serializerForType(ApiResponse);
                var data = _serializers.deserializeWith<ApiResponse>(serializer, response.data is String ? jsonDecode(response.data) : response.data);

            return Response<ApiResponse>(
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
        }

import 'dart:async';
import 'dart:io';
import 'dart:convert';
import 'package:dio/dio.dart';
import 'package:built_collection/built_collection.dart';
import 'package:built_value/serializer.dart';

import 'package:openapi/model/user.dart';

class UserApi {
    final Dio _dio;
    Serializers _serializers;

    UserApi(this._dio, this._serializers);

        /// Create user
        ///
        /// This can only be done by the logged in user.
        Future<Response>createUser(User body,{ CancelToken cancelToken, Map<String, String> headers, ProgressCallback onSendProgress, ProgressCallback onReceiveProgress,}) async {

        String _path = "/user";

        Map<String, dynamic> queryParams = {};
        Map<String, String> headerParams = Map.from(headers ?? {});
        dynamic bodyData;

        queryParams.removeWhere((key, value) => value == null);
        headerParams.removeWhere((key, value) => value == null);

        List<String> contentTypes = [];


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
                'secure': [],
            },
            contentType: contentTypes.isNotEmpty ? contentTypes[0] : "application/json",
            ),
            cancelToken: cancelToken,
            onSendProgress: onSendProgress,
            onReceiveProgress: onReceiveProgress,
            );
            }
        /// Creates list of users with given input array
        ///
        /// 
        Future<Response>createUsersWithArrayInput(List<User> body,{ CancelToken cancelToken, Map<String, String> headers, ProgressCallback onSendProgress, ProgressCallback onReceiveProgress,}) async {

        String _path = "/user/createWithArray";

        Map<String, dynamic> queryParams = {};
        Map<String, String> headerParams = Map.from(headers ?? {});
        dynamic bodyData;

        queryParams.removeWhere((key, value) => value == null);
        headerParams.removeWhere((key, value) => value == null);

        List<String> contentTypes = [];


            final type = const FullType(BuiltList, const [const FullType(User)]);
            var serializedBody = _serializers.serialize(BuiltList<User>.from(body), specifiedType: type);
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
                'secure': [],
            },
            contentType: contentTypes.isNotEmpty ? contentTypes[0] : "application/json",
            ),
            cancelToken: cancelToken,
            onSendProgress: onSendProgress,
            onReceiveProgress: onReceiveProgress,
            );
            }
        /// Creates list of users with given input array
        ///
        /// 
        Future<Response>createUsersWithListInput(List<User> body,{ CancelToken cancelToken, Map<String, String> headers, ProgressCallback onSendProgress, ProgressCallback onReceiveProgress,}) async {

        String _path = "/user/createWithList";

        Map<String, dynamic> queryParams = {};
        Map<String, String> headerParams = Map.from(headers ?? {});
        dynamic bodyData;

        queryParams.removeWhere((key, value) => value == null);
        headerParams.removeWhere((key, value) => value == null);

        List<String> contentTypes = [];


            final type = const FullType(BuiltList, const [const FullType(User)]);
            var serializedBody = _serializers.serialize(BuiltList<User>.from(body), specifiedType: type);
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
                'secure': [],
            },
            contentType: contentTypes.isNotEmpty ? contentTypes[0] : "application/json",
            ),
            cancelToken: cancelToken,
            onSendProgress: onSendProgress,
            onReceiveProgress: onReceiveProgress,
            );
            }
        /// Delete user
        ///
        /// This can only be done by the logged in user.
        Future<Response>deleteUser(String username,{ CancelToken cancelToken, Map<String, String> headers, ProgressCallback onSendProgress, ProgressCallback onReceiveProgress,}) async {

        String _path = "/user/{username}".replaceAll("{" r'username' "}", username.toString());

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
            method: 'delete'.toUpperCase(),
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
        /// Get user by user name
        ///
        /// 
        Future<Response<User>>getUserByName(String username,{ CancelToken cancelToken, Map<String, String> headers, ProgressCallback onSendProgress, ProgressCallback onReceiveProgress,}) async {

        String _path = "/user/{username}".replaceAll("{" r'username' "}", username.toString());

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

                var serializer = _serializers.serializerForType(User);
                var data = _serializers.deserializeWith<User>(serializer, response.data is String ? jsonDecode(response.data) : response.data);

            return Response<User>(
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
        /// Logs user into the system
        ///
        /// 
        Future<Response<String>>loginUser(String username,String password,{ CancelToken cancelToken, Map<String, String> headers, ProgressCallback onSendProgress, ProgressCallback onReceiveProgress,}) async {

        String _path = "/user/login";

        Map<String, dynamic> queryParams = {};
        Map<String, String> headerParams = Map.from(headers ?? {});
        dynamic bodyData;

                queryParams[r'username'] = username;
                queryParams[r'password'] = password;
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

                var serializer = _serializers.serializerForType(String);
                var data = _serializers.deserializeWith<String>(serializer, response.data is String ? jsonDecode(response.data) : response.data);

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
        /// Logs out current logged in user session
        ///
        /// 
        Future<Response>logoutUser({ CancelToken cancelToken, Map<String, String> headers, ProgressCallback onSendProgress, ProgressCallback onReceiveProgress,}) async {

        String _path = "/user/logout";

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
            );
            }
        /// Updated user
        ///
        /// This can only be done by the logged in user.
        Future<Response>updateUser(String username,User body,{ CancelToken cancelToken, Map<String, String> headers, ProgressCallback onSendProgress, ProgressCallback onReceiveProgress,}) async {

        String _path = "/user/{username}".replaceAll("{" r'username' "}", username.toString());

        Map<String, dynamic> queryParams = {};
        Map<String, String> headerParams = Map.from(headers ?? {});
        dynamic bodyData;

        queryParams.removeWhere((key, value) => value == null);
        headerParams.removeWhere((key, value) => value == null);

        List<String> contentTypes = [];


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

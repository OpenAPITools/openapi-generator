//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.6

// ignore_for_file: unused_import

import 'dart:async';
import 'dart:convert';
import 'package:dio/dio.dart';
import 'package:built_value/serializer.dart';

import 'package:openapi/model/user.dart';
import 'package:built_collection/built_collection.dart';

class UserApi {

    final Dio _dio;

    final Serializers _serializers;

    const UserApi(this._dio, this._serializers);

    /// Create user
    ///
    /// This can only be done by the logged in user.
    Future<Response<void>> createUser(
        User body, { 
        CancelToken cancelToken,
        Map<String, dynamic> headers,
        Map<String, dynamic> extra,
        ValidateStatus validateStatus,
        ProgressCallback onSendProgress,
        ProgressCallback onReceiveProgress,
    }) async {
        final _request = RequestOptions(
          path: r'/user',
          method: 'POST',
          headers: <String, dynamic>{
            ...?headers,
          }..removeWhere((_, dynamic value) => value == null),
          queryParameters: <String, dynamic>{
          }..removeWhere((_, dynamic value) => value == null),
          extra: <String, dynamic>{
            'secure': <Map<String, String>>[],
            ...?extra,
          },
          validateStatus: validateStatus,
          contentType: [
            'application/json',
          ].first,
          cancelToken: cancelToken,
          onSendProgress: onSendProgress,
          onReceiveProgress: onReceiveProgress,
        );

        dynamic _bodyData;

        try {
          final _bodyType = FullType(User);
          final _serializedBody = _serializers.serialize(body, specifiedType: _bodyType);
          final _encodedJson = json.encode(_serializedBody);
          _bodyData = _encodedJson;
        } catch(error) {
          throw DioError(
            request: _request,
            type: DioErrorType.DEFAULT,
            error: error,
          );
        }

        final _response = await _dio.request<dynamic>(
          _request.path,
          data: _bodyData,
          options: _request,
        );

        return _response;
    }

    /// Creates list of users with given input array
    ///
    /// 
    Future<Response<void>> createUsersWithArrayInput(
        BuiltList<User> body, { 
        CancelToken cancelToken,
        Map<String, dynamic> headers,
        Map<String, dynamic> extra,
        ValidateStatus validateStatus,
        ProgressCallback onSendProgress,
        ProgressCallback onReceiveProgress,
    }) async {
        final _request = RequestOptions(
          path: r'/user/createWithArray',
          method: 'POST',
          headers: <String, dynamic>{
            ...?headers,
          }..removeWhere((_, dynamic value) => value == null),
          queryParameters: <String, dynamic>{
          }..removeWhere((_, dynamic value) => value == null),
          extra: <String, dynamic>{
            'secure': <Map<String, String>>[],
            ...?extra,
          },
          validateStatus: validateStatus,
          contentType: [
            'application/json',
          ].first,
          cancelToken: cancelToken,
          onSendProgress: onSendProgress,
          onReceiveProgress: onReceiveProgress,
        );

        dynamic _bodyData;

        try {
          const _bodyType = FullType(BuiltList, [FullType(User)]);
          final _serializedBody = _serializers.serialize(body, specifiedType: _bodyType);
          final _encodedJson = json.encode(_serializedBody);
          _bodyData = _encodedJson;
        } catch(error) {
          throw DioError(
            request: _request,
            type: DioErrorType.DEFAULT,
            error: error,
          );
        }

        final _response = await _dio.request<dynamic>(
          _request.path,
          data: _bodyData,
          options: _request,
        );

        return _response;
    }

    /// Creates list of users with given input array
    ///
    /// 
    Future<Response<void>> createUsersWithListInput(
        BuiltList<User> body, { 
        CancelToken cancelToken,
        Map<String, dynamic> headers,
        Map<String, dynamic> extra,
        ValidateStatus validateStatus,
        ProgressCallback onSendProgress,
        ProgressCallback onReceiveProgress,
    }) async {
        final _request = RequestOptions(
          path: r'/user/createWithList',
          method: 'POST',
          headers: <String, dynamic>{
            ...?headers,
          }..removeWhere((_, dynamic value) => value == null),
          queryParameters: <String, dynamic>{
          }..removeWhere((_, dynamic value) => value == null),
          extra: <String, dynamic>{
            'secure': <Map<String, String>>[],
            ...?extra,
          },
          validateStatus: validateStatus,
          contentType: [
            'application/json',
          ].first,
          cancelToken: cancelToken,
          onSendProgress: onSendProgress,
          onReceiveProgress: onReceiveProgress,
        );

        dynamic _bodyData;

        try {
          const _bodyType = FullType(BuiltList, [FullType(User)]);
          final _serializedBody = _serializers.serialize(body, specifiedType: _bodyType);
          final _encodedJson = json.encode(_serializedBody);
          _bodyData = _encodedJson;
        } catch(error) {
          throw DioError(
            request: _request,
            type: DioErrorType.DEFAULT,
            error: error,
          );
        }

        final _response = await _dio.request<dynamic>(
          _request.path,
          data: _bodyData,
          options: _request,
        );

        return _response;
    }

    /// Delete user
    ///
    /// This can only be done by the logged in user.
    Future<Response<void>> deleteUser(
        String username, { 
        CancelToken cancelToken,
        Map<String, dynamic> headers,
        Map<String, dynamic> extra,
        ValidateStatus validateStatus,
        ProgressCallback onSendProgress,
        ProgressCallback onReceiveProgress,
    }) async {
        final _request = RequestOptions(
          path: r'/user/{username}'.replaceAll('{' r'username' '}', username.toString()),
          method: 'DELETE',
          headers: <String, dynamic>{
            ...?headers,
          }..removeWhere((_, dynamic value) => value == null),
          queryParameters: <String, dynamic>{
          }..removeWhere((_, dynamic value) => value == null),
          extra: <String, dynamic>{
            'secure': <Map<String, String>>[],
            ...?extra,
          },
          validateStatus: validateStatus,
          contentType: [
            'application/json',
          ].first,
          cancelToken: cancelToken,
          onSendProgress: onSendProgress,
          onReceiveProgress: onReceiveProgress,
        );

        dynamic _bodyData;

        try {
        } catch(error) {
          throw DioError(
            request: _request,
            type: DioErrorType.DEFAULT,
            error: error,
          );
        }

        final _response = await _dio.request<dynamic>(
          _request.path,
          data: _bodyData,
          options: _request,
        );

        return _response;
    }

    /// Get user by user name
    ///
    /// 
    Future<Response<User>> getUserByName(
        String username, { 
        CancelToken cancelToken,
        Map<String, dynamic> headers,
        Map<String, dynamic> extra,
        ValidateStatus validateStatus,
        ProgressCallback onSendProgress,
        ProgressCallback onReceiveProgress,
    }) async {
        final _request = RequestOptions(
          path: r'/user/{username}'.replaceAll('{' r'username' '}', username.toString()),
          method: 'GET',
          headers: <String, dynamic>{
            ...?headers,
          }..removeWhere((_, dynamic value) => value == null),
          queryParameters: <String, dynamic>{
          }..removeWhere((_, dynamic value) => value == null),
          extra: <String, dynamic>{
            'secure': <Map<String, String>>[],
            ...?extra,
          },
          validateStatus: validateStatus,
          contentType: [
            'application/json',
          ].first,
          cancelToken: cancelToken,
          onSendProgress: onSendProgress,
          onReceiveProgress: onReceiveProgress,
        );

        dynamic _bodyData;

        try {
        } catch(error) {
          throw DioError(
            request: _request,
            type: DioErrorType.DEFAULT,
            error: error,
          );
        }

        final _response = await _dio.request<dynamic>(
          _request.path,
          data: _bodyData,
          options: _request,
        );

        try {
          const _responseType = FullType(User);
          final User _responseData = _serializers.deserialize(
            _response.data is String ? jsonDecode(_response.data as String) : _response.data,
            specifiedType: _responseType,
          ) as User;
          return Response<User>(
            data: _responseData,
            headers: _response.headers,
            isRedirect: _response.isRedirect,
            request: _response.request,
            redirects: _response.redirects,
            statusCode: _response.statusCode,
            statusMessage: _response.statusMessage,
            extra: _response.extra,
          );
        } catch (error) {
          throw DioError(
            request: _request,
            response: _response,
            type: DioErrorType.DEFAULT,
            error: error,
          );
        }
    }

    /// Logs user into the system
    ///
    /// 
    Future<Response<String>> loginUser(
        String username,
        String password, { 
        CancelToken cancelToken,
        Map<String, dynamic> headers,
        Map<String, dynamic> extra,
        ValidateStatus validateStatus,
        ProgressCallback onSendProgress,
        ProgressCallback onReceiveProgress,
    }) async {
        final _request = RequestOptions(
          path: r'/user/login',
          method: 'GET',
          headers: <String, dynamic>{
            ...?headers,
          }..removeWhere((_, dynamic value) => value == null),
          queryParameters: <String, dynamic>{
            r'username': username,
            r'password': password,
          }..removeWhere((_, dynamic value) => value == null),
          extra: <String, dynamic>{
            'secure': <Map<String, String>>[],
            ...?extra,
          },
          validateStatus: validateStatus,
          contentType: [
            'application/json',
          ].first,
          cancelToken: cancelToken,
          onSendProgress: onSendProgress,
          onReceiveProgress: onReceiveProgress,
        );

        dynamic _bodyData;

        try {
        } catch(error) {
          throw DioError(
            request: _request,
            type: DioErrorType.DEFAULT,
            error: error,
          );
        }

        final _response = await _dio.request<dynamic>(
          _request.path,
          data: _bodyData,
          options: _request,
        );

        try {
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
        } catch (error) {
          throw DioError(
            request: _request,
            response: _response,
            type: DioErrorType.DEFAULT,
            error: error,
          );
        }
    }

    /// Logs out current logged in user session
    ///
    /// 
    Future<Response<void>> logoutUser({ 
        CancelToken cancelToken,
        Map<String, dynamic> headers,
        Map<String, dynamic> extra,
        ValidateStatus validateStatus,
        ProgressCallback onSendProgress,
        ProgressCallback onReceiveProgress,
    }) async {
        final _request = RequestOptions(
          path: r'/user/logout',
          method: 'GET',
          headers: <String, dynamic>{
            ...?headers,
          }..removeWhere((_, dynamic value) => value == null),
          queryParameters: <String, dynamic>{
          }..removeWhere((_, dynamic value) => value == null),
          extra: <String, dynamic>{
            'secure': <Map<String, String>>[],
            ...?extra,
          },
          validateStatus: validateStatus,
          contentType: [
            'application/json',
          ].first,
          cancelToken: cancelToken,
          onSendProgress: onSendProgress,
          onReceiveProgress: onReceiveProgress,
        );

        dynamic _bodyData;

        try {
        } catch(error) {
          throw DioError(
            request: _request,
            type: DioErrorType.DEFAULT,
            error: error,
          );
        }

        final _response = await _dio.request<dynamic>(
          _request.path,
          data: _bodyData,
          options: _request,
        );

        return _response;
    }

    /// Updated user
    ///
    /// This can only be done by the logged in user.
    Future<Response<void>> updateUser(
        String username,
        User body, { 
        CancelToken cancelToken,
        Map<String, dynamic> headers,
        Map<String, dynamic> extra,
        ValidateStatus validateStatus,
        ProgressCallback onSendProgress,
        ProgressCallback onReceiveProgress,
    }) async {
        final _request = RequestOptions(
          path: r'/user/{username}'.replaceAll('{' r'username' '}', username.toString()),
          method: 'PUT',
          headers: <String, dynamic>{
            ...?headers,
          }..removeWhere((_, dynamic value) => value == null),
          queryParameters: <String, dynamic>{
          }..removeWhere((_, dynamic value) => value == null),
          extra: <String, dynamic>{
            'secure': <Map<String, String>>[],
            ...?extra,
          },
          validateStatus: validateStatus,
          contentType: [
            'application/json',
          ].first,
          cancelToken: cancelToken,
          onSendProgress: onSendProgress,
          onReceiveProgress: onReceiveProgress,
        );

        dynamic _bodyData;

        try {
          final _bodyType = FullType(User);
          final _serializedBody = _serializers.serialize(body, specifiedType: _bodyType);
          final _encodedJson = json.encode(_serializedBody);
          _bodyData = _encodedJson;
        } catch(error) {
          throw DioError(
            request: _request,
            type: DioErrorType.DEFAULT,
            error: error,
          );
        }

        final _response = await _dio.request<dynamic>(
          _request.path,
          data: _bodyData,
          options: _request,
        );

        return _response;
    }

}

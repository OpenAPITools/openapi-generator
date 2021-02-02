//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.6

// ignore_for_file: unused_import

import 'dart:async';
import 'dart:convert';
import 'package:dio/dio.dart';
import 'package:built_value/serializer.dart';

import 'package:openapi/model/model_client.dart';

class AnotherFakeApi {

    final Dio _dio;

    final Serializers _serializers;

    const AnotherFakeApi(this._dio, this._serializers);

    /// To test special tags
    ///
    /// To test special tags and operation ID starting with number
    Future<Response<ModelClient>> call123testSpecialTags(
        ModelClient modelClient, { 
        CancelToken cancelToken,
        Map<String, dynamic> headers,
        Map<String, dynamic> extra,
        ValidateStatus validateStatus,
        ProgressCallback onSendProgress,
        ProgressCallback onReceiveProgress,
    }) async {
        final _request = RequestOptions(
          path: r'/another-fake/dummy',
          method: 'PATCH',
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
          final _bodyType = FullType(ModelClient);
          final _serializedBody = _serializers.serialize(modelClient, specifiedType: _bodyType);
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

        try {
          const _responseType = FullType(ModelClient);
          final ModelClient _responseData = _serializers.deserialize(
            _response.data is String ? jsonDecode(_response.data as String) : _response.data,
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
        } catch (error) {
          throw DioError(
            request: _request,
            response: _response,
            type: DioErrorType.DEFAULT,
            error: error,
          );
        }
    }

}

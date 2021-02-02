//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.6

// ignore_for_file: unused_import

import 'dart:async';
import 'dart:convert';
import 'package:dio/dio.dart';
import 'package:built_value/serializer.dart';

import 'package:openapi/model/order.dart';
import 'package:built_collection/built_collection.dart';

class StoreApi {

    final Dio _dio;

    final Serializers _serializers;

    const StoreApi(this._dio, this._serializers);

    /// Delete purchase order by ID
    ///
    /// For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors
    Future<Response<void>> deleteOrder(
        String orderId, { 
        CancelToken cancelToken,
        Map<String, dynamic> headers,
        Map<String, dynamic> extra,
        ValidateStatus validateStatus,
        ProgressCallback onSendProgress,
        ProgressCallback onReceiveProgress,
    }) async {
        final _request = RequestOptions(
          path: r'/store/order/{order_id}'.replaceAll('{' r'order_id' '}', orderId.toString()),
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

    /// Returns pet inventories by status
    ///
    /// Returns a map of status codes to quantities
    Future<Response<BuiltMap<String, int>>> getInventory({ 
        CancelToken cancelToken,
        Map<String, dynamic> headers,
        Map<String, dynamic> extra,
        ValidateStatus validateStatus,
        ProgressCallback onSendProgress,
        ProgressCallback onReceiveProgress,
    }) async {
        final _request = RequestOptions(
          path: r'/store/inventory',
          method: 'GET',
          headers: <String, dynamic>{
            ...?headers,
          }..removeWhere((_, dynamic value) => value == null),
          queryParameters: <String, dynamic>{
          }..removeWhere((_, dynamic value) => value == null),
          extra: <String, dynamic>{
            'secure': <Map<String, String>>[
               {
                'type': 'apiKey',
                'name': 'api_key',
                'keyName': 'api_key',
                'where': 'header',
              },
            ],
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
          const _responseType = FullType(BuiltMap, [FullType(String), FullType(int)]);
          final BuiltMap<String, int> _responseData = _serializers.deserialize(
            _response.data is String ? jsonDecode(_response.data as String) : _response.data,
            specifiedType: _responseType,
          ) as BuiltMap<String, int>;
          return Response<BuiltMap<String, int>>(
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

    /// Find purchase order by ID
    ///
    /// For valid response try integer IDs with value <= 5 or > 10. Other values will generated exceptions
    Future<Response<Order>> getOrderById(
        int orderId, { 
        CancelToken cancelToken,
        Map<String, dynamic> headers,
        Map<String, dynamic> extra,
        ValidateStatus validateStatus,
        ProgressCallback onSendProgress,
        ProgressCallback onReceiveProgress,
    }) async {
        final _request = RequestOptions(
          path: r'/store/order/{order_id}'.replaceAll('{' r'order_id' '}', orderId.toString()),
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
          const _responseType = FullType(Order);
          final Order _responseData = _serializers.deserialize(
            _response.data is String ? jsonDecode(_response.data as String) : _response.data,
            specifiedType: _responseType,
          ) as Order;
          return Response<Order>(
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

    /// Place an order for a pet
    ///
    /// 
    Future<Response<Order>> placeOrder(
        Order order, { 
        CancelToken cancelToken,
        Map<String, dynamic> headers,
        Map<String, dynamic> extra,
        ValidateStatus validateStatus,
        ProgressCallback onSendProgress,
        ProgressCallback onReceiveProgress,
    }) async {
        final _request = RequestOptions(
          path: r'/store/order',
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
          final _bodyType = FullType(Order);
          final _serializedBody = _serializers.serialize(order, specifiedType: _bodyType);
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
          const _responseType = FullType(Order);
          final Order _responseData = _serializers.deserialize(
            _response.data is String ? jsonDecode(_response.data as String) : _response.data,
            specifiedType: _responseType,
          ) as Order;
          return Response<Order>(
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

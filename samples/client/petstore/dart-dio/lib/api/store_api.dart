import 'dart:async';
import 'dart:io';
import 'dart:convert';
import 'package:dio/dio.dart';
import 'package:built_collection/built_collection.dart';
import 'package:built_value/serializer.dart';

import 'package:openapi/model/order.dart';

class StoreApi {
    final Dio _dio;
    Serializers _serializers;

    StoreApi(this._dio, this._serializers);

        /// Delete purchase order by ID
        ///
        /// For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
        Future<Response>deleteOrder(String orderId,{ CancelToken cancelToken, Map<String, String> headers, ProgressCallback onSendProgress, ProgressCallback onReceiveProgress,}) async {

        String _path = "/store/order/{orderId}".replaceAll("{" r'orderId' "}", orderId.toString());

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
        /// Returns pet inventories by status
        ///
        /// Returns a map of status codes to quantities
        Future<Response<Map<String, int>>>getInventory({ CancelToken cancelToken, Map<String, String> headers, ProgressCallback onSendProgress, ProgressCallback onReceiveProgress,}) async {

        String _path = "/store/inventory";

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

                var serializer = _serializers.serializerForType(Map<String, int>);
                var data = _serializers.deserializeWith<Map<String, int>>(serializer, response.data is String ? jsonDecode(response.data) : response.data);

            return Response<Map<String, int>>(
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
        /// Find purchase order by ID
        ///
        /// For valid response try integer IDs with value &lt;&#x3D; 5 or &gt; 10. Other values will generated exceptions
        Future<Response<Order>>getOrderById(int orderId,{ CancelToken cancelToken, Map<String, String> headers, ProgressCallback onSendProgress, ProgressCallback onReceiveProgress,}) async {

        String _path = "/store/order/{orderId}".replaceAll("{" r'orderId' "}", orderId.toString());

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

                var serializer = _serializers.serializerForType(Order);
                var data = _serializers.deserializeWith<Order>(serializer, response.data is String ? jsonDecode(response.data) : response.data);

            return Response<Order>(
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
        /// Place an order for a pet
        ///
        /// 
        Future<Response<Order>>placeOrder(Order body,{ CancelToken cancelToken, Map<String, String> headers, ProgressCallback onSendProgress, ProgressCallback onReceiveProgress,}) async {

        String _path = "/store/order";

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
            ).then((response) {

                var serializer = _serializers.serializerForType(Order);
                var data = _serializers.deserializeWith<Order>(serializer, response.data is String ? jsonDecode(response.data) : response.data);

            return Response<Order>(
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

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
        Future<Response>deleteOrder(String orderId,{ CancelToken cancelToken, Map<String, String> headers,}) async {

            String path = "/store/order/{orderId}".replaceAll("{" + "orderId" + "}", orderId.toString());

            // query params
            Map<String, dynamic> queryParams = {};
            Map<String, String> headerParams = Map.from(headers ?? {});
            Map<String, String> formParams = {};

            queryParams.removeWhere((key, value) => value == null);
            headerParams.removeWhere((key, value) => value == null);
            formParams.removeWhere((key, value) => value == null);

            List<String> contentTypes = [];


            return _dio.request(
            path,
            queryParameters: queryParams,
            options: Options(
            method: 'delete'.toUpperCase(),
            headers: headerParams,
            contentType: contentTypes.isNotEmpty ? contentTypes[0] : "application/json",
            ),
            cancelToken: cancelToken,
            );
            }
        /// Returns pet inventories by status
        ///
        /// Returns a map of status codes to quantities
        Future<Response<Map<String, int>>>getInventory({ CancelToken cancelToken, Map<String, String> headers,}) async {

            String path = "/store/inventory";

            // query params
            Map<String, dynamic> queryParams = {};
            Map<String, String> headerParams = Map.from(headers ?? {});
            Map<String, String> formParams = {};

            queryParams.removeWhere((key, value) => value == null);
            headerParams.removeWhere((key, value) => value == null);
            formParams.removeWhere((key, value) => value == null);

            List<String> contentTypes = [];


            return _dio.request(
            path,
            queryParameters: queryParams,
            options: Options(
            method: 'get'.toUpperCase(),
            headers: headerParams,
            contentType: contentTypes.isNotEmpty ? contentTypes[0] : "application/json",
            ),
            cancelToken: cancelToken,
            ).then((response) {

        var serializer = _serializers.serializerForType(Map<String, int>);
        var data = _serializers.deserializeWith<Map<String, int>>(serializer, response.data);

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
        Future<Response<Order>>getOrderById(int orderId,{ CancelToken cancelToken, Map<String, String> headers,}) async {

            String path = "/store/order/{orderId}".replaceAll("{" + "orderId" + "}", orderId.toString());

            // query params
            Map<String, dynamic> queryParams = {};
            Map<String, String> headerParams = Map.from(headers ?? {});
            Map<String, String> formParams = {};

            queryParams.removeWhere((key, value) => value == null);
            headerParams.removeWhere((key, value) => value == null);
            formParams.removeWhere((key, value) => value == null);

            List<String> contentTypes = [];


            return _dio.request(
            path,
            queryParameters: queryParams,
            options: Options(
            method: 'get'.toUpperCase(),
            headers: headerParams,
            contentType: contentTypes.isNotEmpty ? contentTypes[0] : "application/json",
            ),
            cancelToken: cancelToken,
            ).then((response) {

        var serializer = _serializers.serializerForType(Order);
        var data = _serializers.deserializeWith<Order>(serializer, response.data);

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
        Future<Response<Order>>placeOrder(Order body,{ CancelToken cancelToken, Map<String, String> headers,}) async {

            String path = "/store/order";

            // query params
            Map<String, dynamic> queryParams = {};
            Map<String, String> headerParams = Map.from(headers ?? {});
            Map<String, String> formParams = {};

            queryParams.removeWhere((key, value) => value == null);
            headerParams.removeWhere((key, value) => value == null);
            formParams.removeWhere((key, value) => value == null);

            List<String> contentTypes = [];

            var serializedBody = _serializers.serialize(body);
            var jsonbody = json.encode(serializedBody);

            return _dio.request(
            path,
            queryParameters: queryParams,
                data: jsonbody,
            options: Options(
            method: 'post'.toUpperCase(),
            headers: headerParams,
            contentType: contentTypes.isNotEmpty ? contentTypes[0] : "application/json",
            ),
            cancelToken: cancelToken,
            ).then((response) {

        var serializer = _serializers.serializerForType(Order);
        var data = _serializers.deserializeWith<Order>(serializer, response.data);

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

// GENERATED CODE - DO NOT MODIFY BY HAND

part of 'store_api.dart';

// **************************************************************************
// JaguarHttpGenerator
// **************************************************************************

abstract class _$StoreApiClient implements ApiClient {
  final String basePath = "";
  Future<void> deleteOrder(String orderId) async {
    var req = base.delete
        .path(basePath)
        .path("/store/order/:orderId")
        .pathParams("orderId", orderId);
    await req.go(throwOnErr: true);
  }

  Future<Map<String, int>> getInventory() async {
    var req = base.get
        .metadata({
          "auth": [
            {
              "type": "apiKey",
              "name": "api_key",
              "keyName": "api_key",
              "where": "header",
            }
          ],
        })
        .path(basePath)
        .path("/store/inventory");
    return req.one().then((v) => jsonConverter.mapFrom<int>(v));
  }

  Future<Order> getOrderById(int orderId) async {
    var req = base.get
        .path(basePath)
        .path("/store/order/:orderId")
        .pathParams("orderId", orderId);
    return req.go(throwOnErr: true).then(decodeOne);
  }

  Future<Order> placeOrder(Order body) async {
    var req = base.post
        .path(basePath)
        .path("/store/order")
        .body(converters["application/octet-stream"].encode(body));
    return req.go(throwOnErr: true).then(decodeOne);
  }
}

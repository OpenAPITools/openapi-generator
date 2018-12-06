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
    await req.go();
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
    return req.one().then((v) => serializers.mapFrom<int>(v));
  }

  Future<Order> getOrderById(int orderId) async {
    var req = base.get
        .path(basePath)
        .path("/store/order/:orderId")
        .pathParams("orderId", orderId);
    return req.one(convert: serializers.oneFrom);
  }

  Future<Order> placeOrder(Order order) async {
    var req = base.post
        .path(basePath)
        .path("/store/order")
        .json(serializers.to(order));
    return req.one(convert: serializers.oneFrom);
  }
}

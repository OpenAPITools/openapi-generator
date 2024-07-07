import 'package:petstore_api/_internal.dart';

part 'store_api.requests.dart';
part 'store_api.responses.dart';

class StoreApi {
  final NetworkingClientBase networkingClient;
  final Uri baseUrl;
  final Map<String, dynamic> context;

  const StoreApi({
    required this.networkingClient,
    required this.baseUrl,
    this.context = const {},
  });

  Future<StoreApiDeleteOrderResponse> deleteOrder(
    StoreApiDeleteOrderRequest request, {
    Map<String, dynamic> context = const {},
  }) async {
    final newContext = {...this.context, ...context};
    final httpRequest = await request.createHttpRequest(
      context: newContext,
      baseUrl: baseUrl,
    );
    final response = await networkingClient.sendRequest(httpRequest);
    return StoreApiDeleteOrderResponse.fromResponse(
      response,
      context: newContext,
    );
  }
  Future<StoreApiGetInventoryResponse> getInventory(
    StoreApiGetInventoryRequest request, {
    Map<String, dynamic> context = const {},
  }) async {
    final newContext = {...this.context, ...context};
    final httpRequest = await request.createHttpRequest(
      context: newContext,
      baseUrl: baseUrl,
    );
    final response = await networkingClient.sendRequest(httpRequest);
    return StoreApiGetInventoryResponse.fromResponse(
      response,
      context: newContext,
    );
  }
  Future<StoreApiGetOrderByIdResponse> getOrderById(
    StoreApiGetOrderByIdRequest request, {
    Map<String, dynamic> context = const {},
  }) async {
    final newContext = {...this.context, ...context};
    final httpRequest = await request.createHttpRequest(
      context: newContext,
      baseUrl: baseUrl,
    );
    final response = await networkingClient.sendRequest(httpRequest);
    return StoreApiGetOrderByIdResponse.fromResponse(
      response,
      context: newContext,
    );
  }
  Future<StoreApiPlaceOrderResponse> placeOrder(
    StoreApiPlaceOrderRequest request, {
    Map<String, dynamic> context = const {},
  }) async {
    final newContext = {...this.context, ...context};
    final httpRequest = await request.createHttpRequest(
      context: newContext,
      baseUrl: baseUrl,
    );
    final response = await networkingClient.sendRequest(httpRequest);
    return StoreApiPlaceOrderResponse.fromResponse(
      response,
      context: newContext,
    );
  }
}

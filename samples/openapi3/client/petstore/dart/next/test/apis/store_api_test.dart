import 'package:test/test.dart';
import 'package:petstore_api/_internal.dart';

Stream<List<int>> _streamOfText(String value, {Encoding? encoding}) async* {
  yield (encoding ?? utf8).encode(value);
}

final baseUrl = Uri.https("example.com", "/api");
HttpResponseBase createFakeResponse({
  required int statusCode,
  required Stream<List<int>> bodyBytesStream,
  Map<String, String> headers = const {},
  String? reasonPhrase,
}) {
  return HttpResponseBase.stream(
    originalRequest: HttpRequestBase.empty(url: baseUrl, method: 'GET'),
    bodyBytesStream: bodyBytesStream,
    statusCode: statusCode,
    reasonPhrase: reasonPhrase,
    headers: headers,
  );
}

HttpResponseBase createFakeTextResponse({
  required int statusCode,
  required String value,
  Map<String, String> headers = const {},
  String? contentType,
  Encoding? encoding,
  String? reasonPhrase,
}) {
  var parsedContentType =
      contentType == null ? null : MediaType.parse(contentType);
  if (encoding != null) {
    parsedContentType = parsedContentType?.change(parameters: {
      ...parsedContentType.parameters,
      'charset': encoding.name,
    });
  }
  return createFakeResponse(
    statusCode: statusCode,
    headers: {
      'Content-Type': 'text/plain; charset=utf-8',
      if (parsedContentType != null)
        'Content-Type': parsedContentType.toString(),
      ...headers,
    },
    reasonPhrase: reasonPhrase,
    bodyBytesStream: _streamOfText(value, encoding: encoding),
  );
}

/// tests for StoreApi
void main() {
  group(StoreApi, () {
    final baseUrl = Uri.https("example.com", "/api");
    // Delete purchase order by ID
    //
    // For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors
    //
    group(r'deleteOrder', () {
      group(StoreApiDeleteOrderRequest, () {
    late StoreApiDeleteOrderRequest request;
    test(r'No Body', () async {
        request = StoreApiDeleteOrderRequest(
            extraHeaders: {
                "hello":"world",
            },
            extraQueryParameters: {
                "welcome": "back",
            },
            extraCookies: {
                "nice": "work",
            },
            
orderId:  


            
            


    
    exampleString()


,

        );
        final createdRequest = await request.createHttpRequest(baseUrl: baseUrl);
        expect(createdRequest, isNotNull);
    });
});

      group(StoreApiDeleteOrderResponse, () {
  late StoreApiDeleteOrderResponse response;
  test('Unkown status code', () async {
    response = await StoreApiDeleteOrderResponse.fromResponse(
      createFakeTextResponse(
        statusCode: 2,
        value: "value",
      ),
      context: {},
    );
    expect(response.statusCode, 2);
  });
  group(StoreApiDeleteOrderResponse400, () {
    test('Unkown mime', () async {

    });
    
  });
  group(StoreApiDeleteOrderResponse404, () {
    test('Unkown mime', () async {

    });
    
  });

});
    });
    // Returns pet inventories by status
    //
    // Returns a map of status codes to quantities
    //
    group(r'getInventory', () {
      group(StoreApiGetInventoryRequest, () {
    late StoreApiGetInventoryRequest request;
    test(r'No Body', () async {
        request = StoreApiGetInventoryRequest(
            extraHeaders: {
                "hello":"world",
            },
            extraQueryParameters: {
                "welcome": "back",
            },
            extraCookies: {
                "nice": "work",
            },
            
        );
        final createdRequest = await request.createHttpRequest(baseUrl: baseUrl);
        expect(createdRequest, isNotNull);
    });
});

      group(StoreApiGetInventoryResponse, () {
  late StoreApiGetInventoryResponse response;
  test('Unkown status code', () async {
    response = await StoreApiGetInventoryResponse.fromResponse(
      createFakeTextResponse(
        statusCode: 2,
        value: "value",
      ),
      context: {},
    );
    expect(response.statusCode, 2);
  });
  group(StoreApiGetInventoryResponse200, () {
    test('Unkown mime', () async {

    });
    
    test(StoreApiGetInventoryResponse200ApplicationJson, () async {

    });
    
  });

});
    });
    // Find purchase order by ID
    //
    // For valid response try integer IDs with value <= 5 or > 10. Other values will generate exceptions
    //
    group(r'getOrderById', () {
      group(StoreApiGetOrderByIdRequest, () {
    late StoreApiGetOrderByIdRequest request;
    test(r'No Body', () async {
        request = StoreApiGetOrderByIdRequest(
            extraHeaders: {
                "hello":"world",
            },
            extraQueryParameters: {
                "welcome": "back",
            },
            extraCookies: {
                "nice": "work",
            },
            
orderId:  


            
            


    
    exampleint()


,

        );
        final createdRequest = await request.createHttpRequest(baseUrl: baseUrl);
        expect(createdRequest, isNotNull);
    });
});

      group(StoreApiGetOrderByIdResponse, () {
  late StoreApiGetOrderByIdResponse response;
  test('Unkown status code', () async {
    response = await StoreApiGetOrderByIdResponse.fromResponse(
      createFakeTextResponse(
        statusCode: 2,
        value: "value",
      ),
      context: {},
    );
    expect(response.statusCode, 2);
  });
  group(StoreApiGetOrderByIdResponse200, () {
    test('Unkown mime', () async {

    });
    
    test(StoreApiGetOrderByIdResponse200ApplicationXml, () async {

    });
    
    test(StoreApiGetOrderByIdResponse200ApplicationJson, () async {

    });
    
  });
  group(StoreApiGetOrderByIdResponse400, () {
    test('Unkown mime', () async {

    });
    
  });
  group(StoreApiGetOrderByIdResponse404, () {
    test('Unkown mime', () async {

    });
    
  });

});
    });
    // Place an order for a pet
    //
    // 
    //
    group(r'placeOrder', () {
      group(StoreApiPlaceOrderRequest, () {
    late StoreApiPlaceOrderRequest request;
    test(StoreApiPlaceOrderRequestUnsafe, () async {
        request = StoreApiPlaceOrderRequest.unsafe(
            bodyBytesStream: Stream.value(Uint8List.fromList([1, 2, 3])),
            extraHeaders: {
                "hello":"world",
            },
            extraQueryParameters: {
                "welcome": "back",
            },
            extraCookies: {
                "nice": "work",
            },
            
        );
        final createdRequest = await request.createHttpRequest(baseUrl: baseUrl);
        expect(createdRequest, isNotNull);
    });
    
    test(StoreApiPlaceOrderRequestApplicationJson, () async {
        request = StoreApiPlaceOrderRequest.applicationJson(
            data: 


            
            


    Order.$reflection.example()
    


,
            extraHeaders: {
                "hello":"world",
            },
            extraQueryParameters: {
                "welcome": "back",
            },
            extraCookies: {
                "nice": "work",
            },
            
        );
        final createdRequest = await request.createHttpRequest(baseUrl: baseUrl);
        expect(createdRequest, isNotNull);
    });
    
});

      group(StoreApiPlaceOrderResponse, () {
  late StoreApiPlaceOrderResponse response;
  test('Unkown status code', () async {
    response = await StoreApiPlaceOrderResponse.fromResponse(
      createFakeTextResponse(
        statusCode: 2,
        value: "value",
      ),
      context: {},
    );
    expect(response.statusCode, 2);
  });
  group(StoreApiPlaceOrderResponse200, () {
    test('Unkown mime', () async {

    });
    
    test(StoreApiPlaceOrderResponse200ApplicationXml, () async {

    });
    
    test(StoreApiPlaceOrderResponse200ApplicationJson, () async {

    });
    
  });
  group(StoreApiPlaceOrderResponse400, () {
    test('Unkown mime', () async {

    });
    
  });

});
    });
  });
}

import 'package:test/test.dart';
import 'package:petstore_api/_internal.dart';
import '../utils.dart';

/// tests for StoreApi
void main() {
  final wireOptions = WireSerializationOptions();
  final exampleContext = ExampleContext();
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
            
orderId: XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forString
        
,
)
.exampleFunction(exampleContext),

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
      wireSerializationOptions: wireOptions,
      userContext: {},
    );
    expect(response, isA<StoreApiDeleteOrderResponse>()
      .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
    );
    expect(response.statusCode, 2);
  });
  group(StoreApiDeleteOrderResponse400, () {
    test('Unknown mime', () async {
      final codeExample = 
    400
;
      response = await StoreApiDeleteOrderResponse.fromResponse(
        createFakeTextResponse(
          statusCode: codeExample,
          value: "value",
          contentType: "application/whatever",
        ),
        wireSerializationOptions: wireOptions,
        userContext: {},
      );
      expect(response.statusCode, codeExample);
      expect(response, isA<StoreApiDeleteOrderResponse400>()
        .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
      );
    });
    
  });
  group(StoreApiDeleteOrderResponse404, () {
    test('Unknown mime', () async {
      final codeExample = 
    404
;
      response = await StoreApiDeleteOrderResponse.fromResponse(
        createFakeTextResponse(
          statusCode: codeExample,
          value: "value",
          contentType: "application/whatever",
        ),
        wireSerializationOptions: wireOptions,
        userContext: {},
      );
      expect(response.statusCode, codeExample);
      expect(response, isA<StoreApiDeleteOrderResponse404>()
        .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
      );
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
      wireSerializationOptions: wireOptions,
      userContext: {},
    );
    expect(response, isA<StoreApiGetInventoryResponse>()
      .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
    );
    expect(response.statusCode, 2);
  });
  group(StoreApiGetInventoryResponse200, () {
    test('Unknown mime', () async {
      final codeExample = 
    200
;
      response = await StoreApiGetInventoryResponse.fromResponse(
        createFakeTextResponse(
          statusCode: codeExample,
          value: "value",
          contentType: "application/whatever",
        ),
        wireSerializationOptions: wireOptions,
        userContext: {},
      );
      expect(response.statusCode, codeExample);
      expect(response, isA<StoreApiGetInventoryResponse200>()
        .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
      );
    });
    
    test(StoreApiGetInventoryResponse200ApplicationJson, () async {
      final codeExample = 
    200
;
      final mimeExample = MediaType.parse(exampleContext.exampleMimeType(r'application/json'));
      final bodyReflection = StoreApiGetInventoryResponse200ApplicationJson.bodyReflection;
      // v: an example response body.
      final v = bodyReflection.exampleFunction(exampleContext);
      SerializationContext context;
      if (wireOptions.isXml(mimeExample)) {
        context = const SerializationContext.xml();
      } else {
        context = const SerializationContext.json();
      }
      final serializedBody = bodyReflection.serializeFunction(v, context);
      Object? finalValue = null;
      finalValue = jsonEncode(serializedBody, toEncodable: wireOptions.toEncodable);

      response = await StoreApiGetInventoryResponse.fromResponse(
        switch (finalValue) {
          String() => createFakeTextResponse(
            statusCode: codeExample,
            value: finalValue,
            contentType: mimeExample.toString(),
          ),
          _ => createFakeTextResponse(
            value: serializedBody.toString(),
            statusCode: codeExample,
            contentType: mimeExample.toString(),
          ),
        },
        userContext: {},
        wireSerializationOptions: wireOptions,
      );
      expect(response.statusCode, codeExample);
      expect(response.headers, containsPair('content-type', mimeExample.toString()));
      //bodyBytesStream SHOULD be null if the response was handled successfully.
      expect(response.bodyBytesStream, OASNetworkingUtils.isMediaTypeSerializable(mimeExample) ? isNull : isNotNull);
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
            
orderId: XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forint
        
,
)
.exampleFunction(exampleContext),

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
      wireSerializationOptions: wireOptions,
      userContext: {},
    );
    expect(response, isA<StoreApiGetOrderByIdResponse>()
      .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
    );
    expect(response.statusCode, 2);
  });
  group(StoreApiGetOrderByIdResponse200, () {
    test('Unknown mime', () async {
      final codeExample = 
    200
;
      response = await StoreApiGetOrderByIdResponse.fromResponse(
        createFakeTextResponse(
          statusCode: codeExample,
          value: "value",
          contentType: "application/whatever",
        ),
        wireSerializationOptions: wireOptions,
        userContext: {},
      );
      expect(response.statusCode, codeExample);
      expect(response, isA<StoreApiGetOrderByIdResponse200>()
        .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
      );
    });
    
    test(StoreApiGetOrderByIdResponse200ApplicationXml, () async {
      final codeExample = 
    200
;
      final mimeExample = MediaType.parse(exampleContext.exampleMimeType(r'application/xml'));
      final bodyReflection = StoreApiGetOrderByIdResponse200ApplicationXml.bodyReflection;
      // v: an example response body.
      final v = bodyReflection.exampleFunction(exampleContext);
      SerializationContext context;
      if (wireOptions.isXml(mimeExample)) {
        context = const SerializationContext.xml();
      } else {
        context = const SerializationContext.json();
      }
      final serializedBody = bodyReflection.serializeFunction(v, context);
      Object? finalValue = null;
      finalValue = serializedBody;

      response = await StoreApiGetOrderByIdResponse.fromResponse(
        switch (finalValue) {
          String() => createFakeTextResponse(
            statusCode: codeExample,
            value: finalValue,
            contentType: mimeExample.toString(),
          ),
          _ => createFakeTextResponse(
            value: serializedBody.toString(),
            statusCode: codeExample,
            contentType: mimeExample.toString(),
          ),
        },
        userContext: {},
        wireSerializationOptions: wireOptions,
      );
      expect(response.statusCode, codeExample);
      expect(response.headers, containsPair('content-type', mimeExample.toString()));
      //bodyBytesStream SHOULD be null if the response was handled successfully.
      expect(response.bodyBytesStream, OASNetworkingUtils.isMediaTypeSerializable(mimeExample) ? isNull : isNotNull);
    });
    
    test(StoreApiGetOrderByIdResponse200ApplicationJson, () async {
      final codeExample = 
    200
;
      final mimeExample = MediaType.parse(exampleContext.exampleMimeType(r'application/json'));
      final bodyReflection = StoreApiGetOrderByIdResponse200ApplicationJson.bodyReflection;
      // v: an example response body.
      final v = bodyReflection.exampleFunction(exampleContext);
      SerializationContext context;
      if (wireOptions.isXml(mimeExample)) {
        context = const SerializationContext.xml();
      } else {
        context = const SerializationContext.json();
      }
      final serializedBody = bodyReflection.serializeFunction(v, context);
      Object? finalValue = null;
      finalValue = jsonEncode(serializedBody, toEncodable: wireOptions.toEncodable);

      response = await StoreApiGetOrderByIdResponse.fromResponse(
        switch (finalValue) {
          String() => createFakeTextResponse(
            statusCode: codeExample,
            value: finalValue,
            contentType: mimeExample.toString(),
          ),
          _ => createFakeTextResponse(
            value: serializedBody.toString(),
            statusCode: codeExample,
            contentType: mimeExample.toString(),
          ),
        },
        userContext: {},
        wireSerializationOptions: wireOptions,
      );
      expect(response.statusCode, codeExample);
      expect(response.headers, containsPair('content-type', mimeExample.toString()));
      //bodyBytesStream SHOULD be null if the response was handled successfully.
      expect(response.bodyBytesStream, OASNetworkingUtils.isMediaTypeSerializable(mimeExample) ? isNull : isNotNull);
    });
    
  });
  group(StoreApiGetOrderByIdResponse400, () {
    test('Unknown mime', () async {
      final codeExample = 
    400
;
      response = await StoreApiGetOrderByIdResponse.fromResponse(
        createFakeTextResponse(
          statusCode: codeExample,
          value: "value",
          contentType: "application/whatever",
        ),
        wireSerializationOptions: wireOptions,
        userContext: {},
      );
      expect(response.statusCode, codeExample);
      expect(response, isA<StoreApiGetOrderByIdResponse400>()
        .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
      );
    });
    
  });
  group(StoreApiGetOrderByIdResponse404, () {
    test('Unknown mime', () async {
      final codeExample = 
    404
;
      response = await StoreApiGetOrderByIdResponse.fromResponse(
        createFakeTextResponse(
          statusCode: codeExample,
          value: "value",
          contentType: "application/whatever",
        ),
        wireSerializationOptions: wireOptions,
        userContext: {},
      );
      expect(response.statusCode, codeExample);
      expect(response, isA<StoreApiGetOrderByIdResponse404>()
        .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
      );
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
            data: StoreApiPlaceOrderRequestApplicationJson.dataReflection.exampleFunction(exampleContext),
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
      wireSerializationOptions: wireOptions,
      userContext: {},
    );
    expect(response, isA<StoreApiPlaceOrderResponse>()
      .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
    );
    expect(response.statusCode, 2);
  });
  group(StoreApiPlaceOrderResponse200, () {
    test('Unknown mime', () async {
      final codeExample = 
    200
;
      response = await StoreApiPlaceOrderResponse.fromResponse(
        createFakeTextResponse(
          statusCode: codeExample,
          value: "value",
          contentType: "application/whatever",
        ),
        wireSerializationOptions: wireOptions,
        userContext: {},
      );
      expect(response.statusCode, codeExample);
      expect(response, isA<StoreApiPlaceOrderResponse200>()
        .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
      );
    });
    
    test(StoreApiPlaceOrderResponse200ApplicationXml, () async {
      final codeExample = 
    200
;
      final mimeExample = MediaType.parse(exampleContext.exampleMimeType(r'application/xml'));
      final bodyReflection = StoreApiPlaceOrderResponse200ApplicationXml.bodyReflection;
      // v: an example response body.
      final v = bodyReflection.exampleFunction(exampleContext);
      SerializationContext context;
      if (wireOptions.isXml(mimeExample)) {
        context = const SerializationContext.xml();
      } else {
        context = const SerializationContext.json();
      }
      final serializedBody = bodyReflection.serializeFunction(v, context);
      Object? finalValue = null;
      finalValue = serializedBody;

      response = await StoreApiPlaceOrderResponse.fromResponse(
        switch (finalValue) {
          String() => createFakeTextResponse(
            statusCode: codeExample,
            value: finalValue,
            contentType: mimeExample.toString(),
          ),
          _ => createFakeTextResponse(
            value: serializedBody.toString(),
            statusCode: codeExample,
            contentType: mimeExample.toString(),
          ),
        },
        userContext: {},
        wireSerializationOptions: wireOptions,
      );
      expect(response.statusCode, codeExample);
      expect(response.headers, containsPair('content-type', mimeExample.toString()));
      //bodyBytesStream SHOULD be null if the response was handled successfully.
      expect(response.bodyBytesStream, OASNetworkingUtils.isMediaTypeSerializable(mimeExample) ? isNull : isNotNull);
    });
    
    test(StoreApiPlaceOrderResponse200ApplicationJson, () async {
      final codeExample = 
    200
;
      final mimeExample = MediaType.parse(exampleContext.exampleMimeType(r'application/json'));
      final bodyReflection = StoreApiPlaceOrderResponse200ApplicationJson.bodyReflection;
      // v: an example response body.
      final v = bodyReflection.exampleFunction(exampleContext);
      SerializationContext context;
      if (wireOptions.isXml(mimeExample)) {
        context = const SerializationContext.xml();
      } else {
        context = const SerializationContext.json();
      }
      final serializedBody = bodyReflection.serializeFunction(v, context);
      Object? finalValue = null;
      finalValue = jsonEncode(serializedBody, toEncodable: wireOptions.toEncodable);

      response = await StoreApiPlaceOrderResponse.fromResponse(
        switch (finalValue) {
          String() => createFakeTextResponse(
            statusCode: codeExample,
            value: finalValue,
            contentType: mimeExample.toString(),
          ),
          _ => createFakeTextResponse(
            value: serializedBody.toString(),
            statusCode: codeExample,
            contentType: mimeExample.toString(),
          ),
        },
        userContext: {},
        wireSerializationOptions: wireOptions,
      );
      expect(response.statusCode, codeExample);
      expect(response.headers, containsPair('content-type', mimeExample.toString()));
      //bodyBytesStream SHOULD be null if the response was handled successfully.
      expect(response.bodyBytesStream, OASNetworkingUtils.isMediaTypeSerializable(mimeExample) ? isNull : isNotNull);
    });
    
  });
  group(StoreApiPlaceOrderResponse400, () {
    test('Unknown mime', () async {
      final codeExample = 
    400
;
      response = await StoreApiPlaceOrderResponse.fromResponse(
        createFakeTextResponse(
          statusCode: codeExample,
          value: "value",
          contentType: "application/whatever",
        ),
        wireSerializationOptions: wireOptions,
        userContext: {},
      );
      expect(response.statusCode, codeExample);
      expect(response, isA<StoreApiPlaceOrderResponse400>()
        .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
      );
    });
    
  });

});
    });
  });
}

import 'package:test/test.dart';
import 'package:petstore_api/_internal.dart';
import '../utils.dart';

/// tests for FakeApi
void main() {
  final wireOptions = WireSerializationOptions();
  final exampleContext = ExampleContext();
  group(FakeApi, () {
    final baseUrl = Uri.https("example.com", "/api");
    // Get a free form object or Json string
    //
    group(r'fakeGetFreeFormObjectGet', () {
      group(FakeApiFakeGetFreeFormObjectGetRequest, () {
    late FakeApiFakeGetFreeFormObjectGetRequest request;
    test(r'No Body', () async {
        request = FakeApiFakeGetFreeFormObjectGetRequest(
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

      group(FakeApiFakeGetFreeFormObjectGetResponse, () {
  late FakeApiFakeGetFreeFormObjectGetResponse response;
  test('Unkown status code', () async {
    response = await FakeApiFakeGetFreeFormObjectGetResponse.fromResponse(
      createFakeTextResponse(
        statusCode: 2,
        value: "value",
      ),
      wireSerializationOptions: wireOptions,
      userContext: {},
    );
    expect(response, isA<FakeApiFakeGetFreeFormObjectGetResponse>()
      .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
    );
    expect(response.statusCode, 2);
  });
  group(FakeApiFakeGetFreeFormObjectGetResponse200, () {
    test('Unknown mime', () async {
      final codeExample = 
    200
;
      response = await FakeApiFakeGetFreeFormObjectGetResponse.fromResponse(
        createFakeTextResponse(
          statusCode: codeExample,
          value: "value",
          contentType: "application/whatever",
        ),
        wireSerializationOptions: wireOptions,
        userContext: {},
      );
      expect(response.statusCode, codeExample);
      expect(response, isA<FakeApiFakeGetFreeFormObjectGetResponse200>()
        .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
      );
    });
    
    test(FakeApiFakeGetFreeFormObjectGetResponse200ApplicationJson, () async {
      final codeExample = 
    200
;
      final mimeExample = MediaType.parse(exampleContext.exampleMimeType(r'application/json'));
      final bodyReflection = FakeApiFakeGetFreeFormObjectGetResponse200ApplicationJson.bodyReflection;
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

      response = await FakeApiFakeGetFreeFormObjectGetResponse.fromResponse(
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
    // Test serialization of outer boolean types
    //
    group(r'fakeOuterBooleanSerialize', () {
      group(FakeApiFakeOuterBooleanSerializeRequest, () {
    late FakeApiFakeOuterBooleanSerializeRequest request;
    test(FakeApiFakeOuterBooleanSerializeRequestUnsafe, () async {
        request = FakeApiFakeOuterBooleanSerializeRequest.unsafe(
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
    
    test(FakeApiFakeOuterBooleanSerializeRequestApplicationJson, () async {
        request = FakeApiFakeOuterBooleanSerializeRequest.applicationJson(
            data: FakeApiFakeOuterBooleanSerializeRequestApplicationJson.dataReflection.exampleFunction(exampleContext),
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

      group(FakeApiFakeOuterBooleanSerializeResponse, () {
  late FakeApiFakeOuterBooleanSerializeResponse response;
  test('Unkown status code', () async {
    response = await FakeApiFakeOuterBooleanSerializeResponse.fromResponse(
      createFakeTextResponse(
        statusCode: 2,
        value: "value",
      ),
      wireSerializationOptions: wireOptions,
      userContext: {},
    );
    expect(response, isA<FakeApiFakeOuterBooleanSerializeResponse>()
      .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
    );
    expect(response.statusCode, 2);
  });
  group(FakeApiFakeOuterBooleanSerializeResponse200, () {
    test('Unknown mime', () async {
      final codeExample = 
    200
;
      response = await FakeApiFakeOuterBooleanSerializeResponse.fromResponse(
        createFakeTextResponse(
          statusCode: codeExample,
          value: "value",
          contentType: "application/whatever",
        ),
        wireSerializationOptions: wireOptions,
        userContext: {},
      );
      expect(response.statusCode, codeExample);
      expect(response, isA<FakeApiFakeOuterBooleanSerializeResponse200>()
        .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
      );
    });
    
    test(FakeApiFakeOuterBooleanSerializeResponse200AnyAny, () async {
      final codeExample = 
    200
;
      final mimeExample = MediaType.parse(exampleContext.exampleMimeType(r'*/*'));
      final bodyReflection = FakeApiFakeOuterBooleanSerializeResponse200AnyAny.bodyReflection;
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

      response = await FakeApiFakeOuterBooleanSerializeResponse.fromResponse(
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
    // Test serialization of object with outer number type
    //
    group(r'fakeOuterCompositeSerialize', () {
      group(FakeApiFakeOuterCompositeSerializeRequest, () {
    late FakeApiFakeOuterCompositeSerializeRequest request;
    test(FakeApiFakeOuterCompositeSerializeRequestUnsafe, () async {
        request = FakeApiFakeOuterCompositeSerializeRequest.unsafe(
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
    
    test(FakeApiFakeOuterCompositeSerializeRequestApplicationJson, () async {
        request = FakeApiFakeOuterCompositeSerializeRequest.applicationJson(
            data: FakeApiFakeOuterCompositeSerializeRequestApplicationJson.dataReflection.exampleFunction(exampleContext),
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

      group(FakeApiFakeOuterCompositeSerializeResponse, () {
  late FakeApiFakeOuterCompositeSerializeResponse response;
  test('Unkown status code', () async {
    response = await FakeApiFakeOuterCompositeSerializeResponse.fromResponse(
      createFakeTextResponse(
        statusCode: 2,
        value: "value",
      ),
      wireSerializationOptions: wireOptions,
      userContext: {},
    );
    expect(response, isA<FakeApiFakeOuterCompositeSerializeResponse>()
      .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
    );
    expect(response.statusCode, 2);
  });
  group(FakeApiFakeOuterCompositeSerializeResponse200, () {
    test('Unknown mime', () async {
      final codeExample = 
    200
;
      response = await FakeApiFakeOuterCompositeSerializeResponse.fromResponse(
        createFakeTextResponse(
          statusCode: codeExample,
          value: "value",
          contentType: "application/whatever",
        ),
        wireSerializationOptions: wireOptions,
        userContext: {},
      );
      expect(response.statusCode, codeExample);
      expect(response, isA<FakeApiFakeOuterCompositeSerializeResponse200>()
        .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
      );
    });
    
    test(FakeApiFakeOuterCompositeSerializeResponse200AnyAny, () async {
      final codeExample = 
    200
;
      final mimeExample = MediaType.parse(exampleContext.exampleMimeType(r'*/*'));
      final bodyReflection = FakeApiFakeOuterCompositeSerializeResponse200AnyAny.bodyReflection;
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

      response = await FakeApiFakeOuterCompositeSerializeResponse.fromResponse(
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
    // Test serialization of outer number types
    //
    group(r'fakeOuterNumberSerialize', () {
      group(FakeApiFakeOuterNumberSerializeRequest, () {
    late FakeApiFakeOuterNumberSerializeRequest request;
    test(FakeApiFakeOuterNumberSerializeRequestUnsafe, () async {
        request = FakeApiFakeOuterNumberSerializeRequest.unsafe(
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
    
    test(FakeApiFakeOuterNumberSerializeRequestApplicationJson, () async {
        request = FakeApiFakeOuterNumberSerializeRequest.applicationJson(
            data: FakeApiFakeOuterNumberSerializeRequestApplicationJson.dataReflection.exampleFunction(exampleContext),
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

      group(FakeApiFakeOuterNumberSerializeResponse, () {
  late FakeApiFakeOuterNumberSerializeResponse response;
  test('Unkown status code', () async {
    response = await FakeApiFakeOuterNumberSerializeResponse.fromResponse(
      createFakeTextResponse(
        statusCode: 2,
        value: "value",
      ),
      wireSerializationOptions: wireOptions,
      userContext: {},
    );
    expect(response, isA<FakeApiFakeOuterNumberSerializeResponse>()
      .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
    );
    expect(response.statusCode, 2);
  });
  group(FakeApiFakeOuterNumberSerializeResponse200, () {
    test('Unknown mime', () async {
      final codeExample = 
    200
;
      response = await FakeApiFakeOuterNumberSerializeResponse.fromResponse(
        createFakeTextResponse(
          statusCode: codeExample,
          value: "value",
          contentType: "application/whatever",
        ),
        wireSerializationOptions: wireOptions,
        userContext: {},
      );
      expect(response.statusCode, codeExample);
      expect(response, isA<FakeApiFakeOuterNumberSerializeResponse200>()
        .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
      );
    });
    
    test(FakeApiFakeOuterNumberSerializeResponse200AnyAny, () async {
      final codeExample = 
    200
;
      final mimeExample = MediaType.parse(exampleContext.exampleMimeType(r'*/*'));
      final bodyReflection = FakeApiFakeOuterNumberSerializeResponse200AnyAny.bodyReflection;
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

      response = await FakeApiFakeOuterNumberSerializeResponse.fromResponse(
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
    // /fake/outer/string
    //
    // Test serialization of outer string types
    //
    group(r'fakeOuterStringSerialize', () {
      group(FakeApiFakeOuterStringSerializeRequest, () {
    late FakeApiFakeOuterStringSerializeRequest request;
    test(FakeApiFakeOuterStringSerializeRequestUnsafe, () async {
        request = FakeApiFakeOuterStringSerializeRequest.unsafe(
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
    
    test(FakeApiFakeOuterStringSerializeRequestApplicationJson, () async {
        request = FakeApiFakeOuterStringSerializeRequest.applicationJson(
            data: FakeApiFakeOuterStringSerializeRequestApplicationJson.dataReflection.exampleFunction(exampleContext),
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

      group(FakeApiFakeOuterStringSerializeResponse, () {
  late FakeApiFakeOuterStringSerializeResponse response;
  test('Unkown status code', () async {
    response = await FakeApiFakeOuterStringSerializeResponse.fromResponse(
      createFakeTextResponse(
        statusCode: 2,
        value: "value",
      ),
      wireSerializationOptions: wireOptions,
      userContext: {},
    );
    expect(response, isA<FakeApiFakeOuterStringSerializeResponse>()
      .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
    );
    expect(response.statusCode, 2);
  });
  group(FakeApiFakeOuterStringSerializeResponse200, () {
    test('Unknown mime', () async {
      final codeExample = 
    200
;
      response = await FakeApiFakeOuterStringSerializeResponse.fromResponse(
        createFakeTextResponse(
          statusCode: codeExample,
          value: "value",
          contentType: "application/whatever",
        ),
        wireSerializationOptions: wireOptions,
        userContext: {},
      );
      expect(response.statusCode, codeExample);
      expect(response, isA<FakeApiFakeOuterStringSerializeResponse200>()
        .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
      );
    });
    
    test(FakeApiFakeOuterStringSerializeResponse200AnyAny, () async {
      final codeExample = 
    200
;
      final mimeExample = MediaType.parse(exampleContext.exampleMimeType(r'*/*'));
      final bodyReflection = FakeApiFakeOuterStringSerializeResponse200AnyAny.bodyReflection;
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

      response = await FakeApiFakeOuterStringSerializeResponse.fromResponse(
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
    // fake uploads an image with ref request bodies
    //
    // 
    //
    group(r'fakeUploadRefRequestBodies', () {
      group(FakeApiFakeUploadRefRequestBodiesRequest, () {
    late FakeApiFakeUploadRefRequestBodiesRequest request;
    test(FakeApiFakeUploadRefRequestBodiesRequestUnsafe, () async {
        request = FakeApiFakeUploadRefRequestBodiesRequest.unsafe(
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
            
petId: XmlReflectionWrapper(
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

      group(FakeApiFakeUploadRefRequestBodiesResponse, () {
  late FakeApiFakeUploadRefRequestBodiesResponse response;
  test('Unkown status code', () async {
    response = await FakeApiFakeUploadRefRequestBodiesResponse.fromResponse(
      createFakeTextResponse(
        statusCode: 2,
        value: "value",
      ),
      wireSerializationOptions: wireOptions,
      userContext: {},
    );
    expect(response, isA<FakeApiFakeUploadRefRequestBodiesResponse>()
      .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
    );
    expect(response.statusCode, 2);
  });
  group(FakeApiFakeUploadRefRequestBodiesResponse200, () {
    test('Unknown mime', () async {
      final codeExample = 
    200
;
      response = await FakeApiFakeUploadRefRequestBodiesResponse.fromResponse(
        createFakeTextResponse(
          statusCode: codeExample,
          value: "value",
          contentType: "application/whatever",
        ),
        wireSerializationOptions: wireOptions,
        userContext: {},
      );
      expect(response.statusCode, codeExample);
      expect(response, isA<FakeApiFakeUploadRefRequestBodiesResponse200>()
        .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
      );
    });
    
    test(FakeApiFakeUploadRefRequestBodiesResponse200ApplicationJson, () async {
      final codeExample = 
    200
;
      final mimeExample = MediaType.parse(exampleContext.exampleMimeType(r'application/json'));
      final bodyReflection = FakeApiFakeUploadRefRequestBodiesResponse200ApplicationJson.bodyReflection;
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

      response = await FakeApiFakeUploadRefRequestBodiesResponse.fromResponse(
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
    // Array of Enums
    //
    group(r'getFakeArrayofenums', () {
      group(FakeApiGetFakeArrayofenumsRequest, () {
    late FakeApiGetFakeArrayofenumsRequest request;
    test(r'No Body', () async {
        request = FakeApiGetFakeArrayofenumsRequest(
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

      group(FakeApiGetFakeArrayofenumsResponse, () {
  late FakeApiGetFakeArrayofenumsResponse response;
  test('Unkown status code', () async {
    response = await FakeApiGetFakeArrayofenumsResponse.fromResponse(
      createFakeTextResponse(
        statusCode: 2,
        value: "value",
      ),
      wireSerializationOptions: wireOptions,
      userContext: {},
    );
    expect(response, isA<FakeApiGetFakeArrayofenumsResponse>()
      .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
    );
    expect(response.statusCode, 2);
  });
  group(FakeApiGetFakeArrayofenumsResponse200, () {
    test('Unknown mime', () async {
      final codeExample = 
    200
;
      response = await FakeApiGetFakeArrayofenumsResponse.fromResponse(
        createFakeTextResponse(
          statusCode: codeExample,
          value: "value",
          contentType: "application/whatever",
        ),
        wireSerializationOptions: wireOptions,
        userContext: {},
      );
      expect(response.statusCode, codeExample);
      expect(response, isA<FakeApiGetFakeArrayofenumsResponse200>()
        .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
      );
    });
    
    test(FakeApiGetFakeArrayofenumsResponse200ApplicationJson, () async {
      final codeExample = 
    200
;
      final mimeExample = MediaType.parse(exampleContext.exampleMimeType(r'application/json'));
      final bodyReflection = FakeApiGetFakeArrayofenumsResponse200ApplicationJson.bodyReflection;
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

      response = await FakeApiGetFakeArrayofenumsResponse.fromResponse(
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
    // Health check endpoint
    //
    group(r'getFakeHealth', () {
      group(FakeApiGetFakeHealthRequest, () {
    late FakeApiGetFakeHealthRequest request;
    test(r'No Body', () async {
        request = FakeApiGetFakeHealthRequest(
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

      group(FakeApiGetFakeHealthResponse, () {
  late FakeApiGetFakeHealthResponse response;
  test('Unkown status code', () async {
    response = await FakeApiGetFakeHealthResponse.fromResponse(
      createFakeTextResponse(
        statusCode: 2,
        value: "value",
      ),
      wireSerializationOptions: wireOptions,
      userContext: {},
    );
    expect(response, isA<FakeApiGetFakeHealthResponse>()
      .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
    );
    expect(response.statusCode, 2);
  });
  group(FakeApiGetFakeHealthResponse200, () {
    test('Unknown mime', () async {
      final codeExample = 
    200
;
      response = await FakeApiGetFakeHealthResponse.fromResponse(
        createFakeTextResponse(
          statusCode: codeExample,
          value: "value",
          contentType: "application/whatever",
        ),
        wireSerializationOptions: wireOptions,
        userContext: {},
      );
      expect(response.statusCode, codeExample);
      expect(response, isA<FakeApiGetFakeHealthResponse200>()
        .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
      );
    });
    
    test(FakeApiGetFakeHealthResponse200ApplicationJson, () async {
      final codeExample = 
    200
;
      final mimeExample = MediaType.parse(exampleContext.exampleMimeType(r'application/json'));
      final bodyReflection = FakeApiGetFakeHealthResponse200ApplicationJson.bodyReflection;
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

      response = await FakeApiGetFakeHealthResponse.fromResponse(
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
    // parameter name mapping test
    //
    group(r'getParameterNameMapping', () {
      group(FakeApiGetParameterNameMappingRequest, () {
    late FakeApiGetParameterNameMappingRequest request;
    test(r'No Body', () async {
        request = FakeApiGetParameterNameMappingRequest(
            extraHeaders: {
                "hello":"world",
            },
            extraQueryParameters: {
                "welcome": "back",
            },
            extraCookies: {
                "nice": "work",
            },
            
$type: XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forint
        
,
)
.exampleFunction(exampleContext),

type: XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forString
        
,
)
.exampleFunction(exampleContext),

type$: XmlReflectionWrapper(
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

      group(FakeApiGetParameterNameMappingResponse, () {
  late FakeApiGetParameterNameMappingResponse response;
  test('Unkown status code', () async {
    response = await FakeApiGetParameterNameMappingResponse.fromResponse(
      createFakeTextResponse(
        statusCode: 2,
        value: "value",
      ),
      wireSerializationOptions: wireOptions,
      userContext: {},
    );
    expect(response, isA<FakeApiGetParameterNameMappingResponse>()
      .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
    );
    expect(response.statusCode, 2);
  });
  group(FakeApiGetParameterNameMappingResponse200, () {
    test('Unknown mime', () async {
      final codeExample = 
    200
;
      response = await FakeApiGetParameterNameMappingResponse.fromResponse(
        createFakeTextResponse(
          statusCode: codeExample,
          value: "value",
          contentType: "application/whatever",
        ),
        wireSerializationOptions: wireOptions,
        userContext: {},
      );
      expect(response.statusCode, codeExample);
      expect(response, isA<FakeApiGetParameterNameMappingResponse200>()
        .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
      );
    });
    
  });

});
    });
    // test referenced additionalProperties
    //
    // 
    //
    group(r'testAdditionalPropertiesReference', () {
      group(FakeApiTestAdditionalPropertiesReferenceRequest, () {
    late FakeApiTestAdditionalPropertiesReferenceRequest request;
    test(FakeApiTestAdditionalPropertiesReferenceRequestUnsafe, () async {
        request = FakeApiTestAdditionalPropertiesReferenceRequest.unsafe(
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
    
    test(FakeApiTestAdditionalPropertiesReferenceRequestApplicationJson, () async {
        request = FakeApiTestAdditionalPropertiesReferenceRequest.applicationJson(
            data: FakeApiTestAdditionalPropertiesReferenceRequestApplicationJson.dataReflection.exampleFunction(exampleContext),
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

      group(FakeApiTestAdditionalPropertiesReferenceResponse, () {
  late FakeApiTestAdditionalPropertiesReferenceResponse response;
  test('Unkown status code', () async {
    response = await FakeApiTestAdditionalPropertiesReferenceResponse.fromResponse(
      createFakeTextResponse(
        statusCode: 2,
        value: "value",
      ),
      wireSerializationOptions: wireOptions,
      userContext: {},
    );
    expect(response, isA<FakeApiTestAdditionalPropertiesReferenceResponse>()
      .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
    );
    expect(response.statusCode, 2);
  });
  group(FakeApiTestAdditionalPropertiesReferenceResponse200, () {
    test('Unknown mime', () async {
      final codeExample = 
    200
;
      response = await FakeApiTestAdditionalPropertiesReferenceResponse.fromResponse(
        createFakeTextResponse(
          statusCode: codeExample,
          value: "value",
          contentType: "application/whatever",
        ),
        wireSerializationOptions: wireOptions,
        userContext: {},
      );
      expect(response.statusCode, codeExample);
      expect(response, isA<FakeApiTestAdditionalPropertiesReferenceResponse200>()
        .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
      );
    });
    
  });

});
    });
    // For this test, the body for this request much reference a schema named `File`.
    //
    group(r'testBodyWithFileSchema', () {
      group(FakeApiTestBodyWithFileSchemaRequest, () {
    late FakeApiTestBodyWithFileSchemaRequest request;
    test(FakeApiTestBodyWithFileSchemaRequestUnsafe, () async {
        request = FakeApiTestBodyWithFileSchemaRequest.unsafe(
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
    
    test(FakeApiTestBodyWithFileSchemaRequestApplicationJson, () async {
        request = FakeApiTestBodyWithFileSchemaRequest.applicationJson(
            data: FakeApiTestBodyWithFileSchemaRequestApplicationJson.dataReflection.exampleFunction(exampleContext),
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

      group(FakeApiTestBodyWithFileSchemaResponse, () {
  late FakeApiTestBodyWithFileSchemaResponse response;
  test('Unkown status code', () async {
    response = await FakeApiTestBodyWithFileSchemaResponse.fromResponse(
      createFakeTextResponse(
        statusCode: 2,
        value: "value",
      ),
      wireSerializationOptions: wireOptions,
      userContext: {},
    );
    expect(response, isA<FakeApiTestBodyWithFileSchemaResponse>()
      .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
    );
    expect(response.statusCode, 2);
  });
  group(FakeApiTestBodyWithFileSchemaResponse200, () {
    test('Unknown mime', () async {
      final codeExample = 
    200
;
      response = await FakeApiTestBodyWithFileSchemaResponse.fromResponse(
        createFakeTextResponse(
          statusCode: codeExample,
          value: "value",
          contentType: "application/whatever",
        ),
        wireSerializationOptions: wireOptions,
        userContext: {},
      );
      expect(response.statusCode, codeExample);
      expect(response, isA<FakeApiTestBodyWithFileSchemaResponse200>()
        .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
      );
    });
    
  });

});
    });
    group(r'testBodyWithQueryParams', () {
      group(FakeApiTestBodyWithQueryParamsRequest, () {
    late FakeApiTestBodyWithQueryParamsRequest request;
    test(FakeApiTestBodyWithQueryParamsRequestUnsafe, () async {
        request = FakeApiTestBodyWithQueryParamsRequest.unsafe(
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
            
query: XmlReflectionWrapper(
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
    
    test(FakeApiTestBodyWithQueryParamsRequestApplicationJson, () async {
        request = FakeApiTestBodyWithQueryParamsRequest.applicationJson(
            data: FakeApiTestBodyWithQueryParamsRequestApplicationJson.dataReflection.exampleFunction(exampleContext),
            extraHeaders: {
                "hello":"world",
            },
            extraQueryParameters: {
                "welcome": "back",
            },
            extraCookies: {
                "nice": "work",
            },
            
query: XmlReflectionWrapper(
    xml: XmlReflection(
    xmlName: r'User',
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

      group(FakeApiTestBodyWithQueryParamsResponse, () {
  late FakeApiTestBodyWithQueryParamsResponse response;
  test('Unkown status code', () async {
    response = await FakeApiTestBodyWithQueryParamsResponse.fromResponse(
      createFakeTextResponse(
        statusCode: 2,
        value: "value",
      ),
      wireSerializationOptions: wireOptions,
      userContext: {},
    );
    expect(response, isA<FakeApiTestBodyWithQueryParamsResponse>()
      .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
    );
    expect(response.statusCode, 2);
  });
  group(FakeApiTestBodyWithQueryParamsResponse200, () {
    test('Unknown mime', () async {
      final codeExample = 
    200
;
      response = await FakeApiTestBodyWithQueryParamsResponse.fromResponse(
        createFakeTextResponse(
          statusCode: codeExample,
          value: "value",
          contentType: "application/whatever",
        ),
        wireSerializationOptions: wireOptions,
        userContext: {},
      );
      expect(response.statusCode, codeExample);
      expect(response, isA<FakeApiTestBodyWithQueryParamsResponse200>()
        .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
      );
    });
    
  });

});
    });
    // To test \"client\" model
    //
    // To test \"client\" model
    //
    group(r'testClientModel', () {
      group(FakeApiTestClientModelRequest, () {
    late FakeApiTestClientModelRequest request;
    test(FakeApiTestClientModelRequestUnsafe, () async {
        request = FakeApiTestClientModelRequest.unsafe(
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
    
    test(FakeApiTestClientModelRequestApplicationJson, () async {
        request = FakeApiTestClientModelRequest.applicationJson(
            data: FakeApiTestClientModelRequestApplicationJson.dataReflection.exampleFunction(exampleContext),
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

      group(FakeApiTestClientModelResponse, () {
  late FakeApiTestClientModelResponse response;
  test('Unkown status code', () async {
    response = await FakeApiTestClientModelResponse.fromResponse(
      createFakeTextResponse(
        statusCode: 2,
        value: "value",
      ),
      wireSerializationOptions: wireOptions,
      userContext: {},
    );
    expect(response, isA<FakeApiTestClientModelResponse>()
      .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
    );
    expect(response.statusCode, 2);
  });
  group(FakeApiTestClientModelResponse200, () {
    test('Unknown mime', () async {
      final codeExample = 
    200
;
      response = await FakeApiTestClientModelResponse.fromResponse(
        createFakeTextResponse(
          statusCode: codeExample,
          value: "value",
          contentType: "application/whatever",
        ),
        wireSerializationOptions: wireOptions,
        userContext: {},
      );
      expect(response.statusCode, codeExample);
      expect(response, isA<FakeApiTestClientModelResponse200>()
        .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
      );
    });
    
    test(FakeApiTestClientModelResponse200ApplicationJson, () async {
      final codeExample = 
    200
;
      final mimeExample = MediaType.parse(exampleContext.exampleMimeType(r'application/json'));
      final bodyReflection = FakeApiTestClientModelResponse200ApplicationJson.bodyReflection;
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

      response = await FakeApiTestClientModelResponse.fromResponse(
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
    // Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
    //
    // Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
    //
    group(r'testEndpointParameters', () {
      group(FakeApiTestEndpointParametersRequest, () {
    late FakeApiTestEndpointParametersRequest request;
    test(FakeApiTestEndpointParametersRequestUnsafe, () async {
        request = FakeApiTestEndpointParametersRequest.unsafe(
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
    
});

      group(FakeApiTestEndpointParametersResponse, () {
  late FakeApiTestEndpointParametersResponse response;
  test('Unkown status code', () async {
    response = await FakeApiTestEndpointParametersResponse.fromResponse(
      createFakeTextResponse(
        statusCode: 2,
        value: "value",
      ),
      wireSerializationOptions: wireOptions,
      userContext: {},
    );
    expect(response, isA<FakeApiTestEndpointParametersResponse>()
      .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
    );
    expect(response.statusCode, 2);
  });
  group(FakeApiTestEndpointParametersResponse400, () {
    test('Unknown mime', () async {
      final codeExample = 
    400
;
      response = await FakeApiTestEndpointParametersResponse.fromResponse(
        createFakeTextResponse(
          statusCode: codeExample,
          value: "value",
          contentType: "application/whatever",
        ),
        wireSerializationOptions: wireOptions,
        userContext: {},
      );
      expect(response.statusCode, codeExample);
      expect(response, isA<FakeApiTestEndpointParametersResponse400>()
        .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
      );
    });
    
  });
  group(FakeApiTestEndpointParametersResponse404, () {
    test('Unknown mime', () async {
      final codeExample = 
    404
;
      response = await FakeApiTestEndpointParametersResponse.fromResponse(
        createFakeTextResponse(
          statusCode: codeExample,
          value: "value",
          contentType: "application/whatever",
        ),
        wireSerializationOptions: wireOptions,
        userContext: {},
      );
      expect(response.statusCode, codeExample);
      expect(response, isA<FakeApiTestEndpointParametersResponse404>()
        .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
      );
    });
    
  });

});
    });
    // To test enum parameters
    //
    // To test enum parameters
    //
    group(r'testEnumParameters', () {
      group(FakeApiTestEnumParametersRequest, () {
    late FakeApiTestEnumParametersRequest request;
    test(r'No Body', () async {
        request = FakeApiTestEnumParametersRequest(
            extraHeaders: {
                "hello":"world",
            },
            extraQueryParameters: {
                "welcome": "back",
            },
            extraCookies: {
                "nice": "work",
            },
            
enumHeaderStringArray: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
    ListReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            EnumHeaderStringArrayEnum.$reflection
        
        
,
)
)
,
)
).exampleFunction(exampleContext),

enumHeaderString: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            EnumHeaderStringEnum.$reflection
        
        
,
)
).exampleFunction(exampleContext),

enumQueryStringArray: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
    ListReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            EnumQueryStringArrayEnum.$reflection
        
        
,
)
)
,
)
).exampleFunction(exampleContext),

enumQueryString: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            EnumQueryStringEnum.$reflection
        
        
,
)
).exampleFunction(exampleContext),

enumQueryInteger: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            EnumQueryIntegerEnum.$reflection
        
        
,
)
).exampleFunction(exampleContext),

enumQueryDouble: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            EnumQueryDoubleEnum.$reflection
        
        
,
)
).exampleFunction(exampleContext),

        );
        final createdRequest = await request.createHttpRequest(baseUrl: baseUrl);
        expect(createdRequest, isNotNull);
    });
});

      group(FakeApiTestEnumParametersResponse, () {
  late FakeApiTestEnumParametersResponse response;
  test('Unkown status code', () async {
    response = await FakeApiTestEnumParametersResponse.fromResponse(
      createFakeTextResponse(
        statusCode: 2,
        value: "value",
      ),
      wireSerializationOptions: wireOptions,
      userContext: {},
    );
    expect(response, isA<FakeApiTestEnumParametersResponse>()
      .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
    );
    expect(response.statusCode, 2);
  });
  group(FakeApiTestEnumParametersResponse400, () {
    test('Unknown mime', () async {
      final codeExample = 
    400
;
      response = await FakeApiTestEnumParametersResponse.fromResponse(
        createFakeTextResponse(
          statusCode: codeExample,
          value: "value",
          contentType: "application/whatever",
        ),
        wireSerializationOptions: wireOptions,
        userContext: {},
      );
      expect(response.statusCode, codeExample);
      expect(response, isA<FakeApiTestEnumParametersResponse400>()
        .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
      );
    });
    
  });
  group(FakeApiTestEnumParametersResponse404, () {
    test('Unknown mime', () async {
      final codeExample = 
    404
;
      response = await FakeApiTestEnumParametersResponse.fromResponse(
        createFakeTextResponse(
          statusCode: codeExample,
          value: "value",
          contentType: "application/whatever",
        ),
        wireSerializationOptions: wireOptions,
        userContext: {},
      );
      expect(response.statusCode, codeExample);
      expect(response, isA<FakeApiTestEnumParametersResponse404>()
        .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
      );
    });
    
  });

});
    });
    // Fake endpoint to test group parameters (optional)
    //
    // Fake endpoint to test group parameters (optional)
    //
    group(r'testGroupParameters', () {
      group(FakeApiTestGroupParametersRequest, () {
    late FakeApiTestGroupParametersRequest request;
    test(r'No Body', () async {
        request = FakeApiTestGroupParametersRequest(
            extraHeaders: {
                "hello":"world",
            },
            extraQueryParameters: {
                "welcome": "back",
            },
            extraCookies: {
                "nice": "work",
            },
            
requiredStringGroup: XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forint
        
,
)
.exampleFunction(exampleContext),

requiredBooleanGroup: XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forbool
        
,
)
.exampleFunction(exampleContext),

requiredInt64Group: XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forint
        
,
)
.exampleFunction(exampleContext),

stringGroup: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forint
        
,
)
).exampleFunction(exampleContext),

booleanGroup: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forbool
        
,
)
).exampleFunction(exampleContext),

int64Group: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forint
        
,
)
).exampleFunction(exampleContext),

        );
        final createdRequest = await request.createHttpRequest(baseUrl: baseUrl);
        expect(createdRequest, isNotNull);
    });
});

      group(FakeApiTestGroupParametersResponse, () {
  late FakeApiTestGroupParametersResponse response;
  test('Unkown status code', () async {
    response = await FakeApiTestGroupParametersResponse.fromResponse(
      createFakeTextResponse(
        statusCode: 2,
        value: "value",
      ),
      wireSerializationOptions: wireOptions,
      userContext: {},
    );
    expect(response, isA<FakeApiTestGroupParametersResponse>()
      .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
    );
    expect(response.statusCode, 2);
  });
  group(FakeApiTestGroupParametersResponse400, () {
    test('Unknown mime', () async {
      final codeExample = 
    400
;
      response = await FakeApiTestGroupParametersResponse.fromResponse(
        createFakeTextResponse(
          statusCode: codeExample,
          value: "value",
          contentType: "application/whatever",
        ),
        wireSerializationOptions: wireOptions,
        userContext: {},
      );
      expect(response.statusCode, codeExample);
      expect(response, isA<FakeApiTestGroupParametersResponse400>()
        .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
      );
    });
    
  });

});
    });
    // test inline additionalProperties
    //
    // 
    //
    group(r'testInlineAdditionalProperties', () {
      group(FakeApiTestInlineAdditionalPropertiesRequest, () {
    late FakeApiTestInlineAdditionalPropertiesRequest request;
    test(FakeApiTestInlineAdditionalPropertiesRequestUnsafe, () async {
        request = FakeApiTestInlineAdditionalPropertiesRequest.unsafe(
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
    
    test(FakeApiTestInlineAdditionalPropertiesRequestApplicationJson, () async {
        request = FakeApiTestInlineAdditionalPropertiesRequest.applicationJson(
            data: FakeApiTestInlineAdditionalPropertiesRequestApplicationJson.dataReflection.exampleFunction(exampleContext),
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

      group(FakeApiTestInlineAdditionalPropertiesResponse, () {
  late FakeApiTestInlineAdditionalPropertiesResponse response;
  test('Unkown status code', () async {
    response = await FakeApiTestInlineAdditionalPropertiesResponse.fromResponse(
      createFakeTextResponse(
        statusCode: 2,
        value: "value",
      ),
      wireSerializationOptions: wireOptions,
      userContext: {},
    );
    expect(response, isA<FakeApiTestInlineAdditionalPropertiesResponse>()
      .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
    );
    expect(response.statusCode, 2);
  });
  group(FakeApiTestInlineAdditionalPropertiesResponse200, () {
    test('Unknown mime', () async {
      final codeExample = 
    200
;
      response = await FakeApiTestInlineAdditionalPropertiesResponse.fromResponse(
        createFakeTextResponse(
          statusCode: codeExample,
          value: "value",
          contentType: "application/whatever",
        ),
        wireSerializationOptions: wireOptions,
        userContext: {},
      );
      expect(response.statusCode, codeExample);
      expect(response, isA<FakeApiTestInlineAdditionalPropertiesResponse200>()
        .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
      );
    });
    
  });

});
    });
    // test inline free-form additionalProperties
    //
    // 
    //
    group(r'testInlineFreeformAdditionalProperties', () {
      group(FakeApiTestInlineFreeformAdditionalPropertiesRequest, () {
    late FakeApiTestInlineFreeformAdditionalPropertiesRequest request;
    test(FakeApiTestInlineFreeformAdditionalPropertiesRequestUnsafe, () async {
        request = FakeApiTestInlineFreeformAdditionalPropertiesRequest.unsafe(
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
    
    test(FakeApiTestInlineFreeformAdditionalPropertiesRequestApplicationJson, () async {
        request = FakeApiTestInlineFreeformAdditionalPropertiesRequest.applicationJson(
            data: FakeApiTestInlineFreeformAdditionalPropertiesRequestApplicationJson.dataReflection.exampleFunction(exampleContext),
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

      group(FakeApiTestInlineFreeformAdditionalPropertiesResponse, () {
  late FakeApiTestInlineFreeformAdditionalPropertiesResponse response;
  test('Unkown status code', () async {
    response = await FakeApiTestInlineFreeformAdditionalPropertiesResponse.fromResponse(
      createFakeTextResponse(
        statusCode: 2,
        value: "value",
      ),
      wireSerializationOptions: wireOptions,
      userContext: {},
    );
    expect(response, isA<FakeApiTestInlineFreeformAdditionalPropertiesResponse>()
      .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
    );
    expect(response.statusCode, 2);
  });
  group(FakeApiTestInlineFreeformAdditionalPropertiesResponse200, () {
    test('Unknown mime', () async {
      final codeExample = 
    200
;
      response = await FakeApiTestInlineFreeformAdditionalPropertiesResponse.fromResponse(
        createFakeTextResponse(
          statusCode: codeExample,
          value: "value",
          contentType: "application/whatever",
        ),
        wireSerializationOptions: wireOptions,
        userContext: {},
      );
      expect(response.statusCode, codeExample);
      expect(response, isA<FakeApiTestInlineFreeformAdditionalPropertiesResponse200>()
        .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
      );
    });
    
  });

});
    });
    // test json serialization of form data
    //
    // 
    //
    group(r'testJsonFormData', () {
      group(FakeApiTestJsonFormDataRequest, () {
    late FakeApiTestJsonFormDataRequest request;
    test(r'No Body', () async {
        request = FakeApiTestJsonFormDataRequest(
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

      group(FakeApiTestJsonFormDataResponse, () {
  late FakeApiTestJsonFormDataResponse response;
  test('Unkown status code', () async {
    response = await FakeApiTestJsonFormDataResponse.fromResponse(
      createFakeTextResponse(
        statusCode: 2,
        value: "value",
      ),
      wireSerializationOptions: wireOptions,
      userContext: {},
    );
    expect(response, isA<FakeApiTestJsonFormDataResponse>()
      .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
    );
    expect(response.statusCode, 2);
  });
  group(FakeApiTestJsonFormDataResponse200, () {
    test('Unknown mime', () async {
      final codeExample = 
    200
;
      response = await FakeApiTestJsonFormDataResponse.fromResponse(
        createFakeTextResponse(
          statusCode: codeExample,
          value: "value",
          contentType: "application/whatever",
        ),
        wireSerializationOptions: wireOptions,
        userContext: {},
      );
      expect(response.statusCode, codeExample);
      expect(response, isA<FakeApiTestJsonFormDataResponse200>()
        .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
      );
    });
    
  });

});
    });
    // To test the collection format in query parameters
    //
    group(r'testQueryParameterCollectionFormat', () {
      group(FakeApiTestQueryParameterCollectionFormatRequest, () {
    late FakeApiTestQueryParameterCollectionFormatRequest request;
    test(FakeApiTestQueryParameterCollectionFormatRequestUnsafe, () async {
        request = FakeApiTestQueryParameterCollectionFormatRequest.unsafe(
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
            
pipe: XmlReflectionWrapper(
    xml: XmlReflection(
),
    
    ListReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forString
        
,
)
)
,
)
.exampleFunction(exampleContext),

ioutil: XmlReflectionWrapper(
    xml: XmlReflection(
),
    
    ListReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forString
        
,
)
)
,
)
.exampleFunction(exampleContext),

http: XmlReflectionWrapper(
    xml: XmlReflection(
),
    
    ListReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forString
        
,
)
)
,
)
.exampleFunction(exampleContext),

url: XmlReflectionWrapper(
    xml: XmlReflection(
),
    
    ListReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forString
        
,
)
)
,
)
.exampleFunction(exampleContext),

context: XmlReflectionWrapper(
    xml: XmlReflection(
),
    
    ListReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forString
        
,
)
)
,
)
.exampleFunction(exampleContext),

        );
        final createdRequest = await request.createHttpRequest(baseUrl: baseUrl);
        expect(createdRequest, isNotNull);
    });
    
});

      group(FakeApiTestQueryParameterCollectionFormatResponse, () {
  late FakeApiTestQueryParameterCollectionFormatResponse response;
  test('Unkown status code', () async {
    response = await FakeApiTestQueryParameterCollectionFormatResponse.fromResponse(
      createFakeTextResponse(
        statusCode: 2,
        value: "value",
      ),
      wireSerializationOptions: wireOptions,
      userContext: {},
    );
    expect(response, isA<FakeApiTestQueryParameterCollectionFormatResponse>()
      .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
    );
    expect(response.statusCode, 2);
  });
  group(FakeApiTestQueryParameterCollectionFormatResponse200, () {
    test('Unknown mime', () async {
      final codeExample = 
    200
;
      response = await FakeApiTestQueryParameterCollectionFormatResponse.fromResponse(
        createFakeTextResponse(
          statusCode: codeExample,
          value: "value",
          contentType: "application/whatever",
        ),
        wireSerializationOptions: wireOptions,
        userContext: {},
      );
      expect(response.statusCode, codeExample);
      expect(response, isA<FakeApiTestQueryParameterCollectionFormatResponse200>()
        .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
      );
    });
    
  });

});
    });
    // test referenced string map
    //
    // 
    //
    group(r'testStringMapReference', () {
      group(FakeApiTestStringMapReferenceRequest, () {
    late FakeApiTestStringMapReferenceRequest request;
    test(FakeApiTestStringMapReferenceRequestUnsafe, () async {
        request = FakeApiTestStringMapReferenceRequest.unsafe(
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
    
    test(FakeApiTestStringMapReferenceRequestApplicationJson, () async {
        request = FakeApiTestStringMapReferenceRequest.applicationJson(
            data: FakeApiTestStringMapReferenceRequestApplicationJson.dataReflection.exampleFunction(exampleContext),
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

      group(FakeApiTestStringMapReferenceResponse, () {
  late FakeApiTestStringMapReferenceResponse response;
  test('Unkown status code', () async {
    response = await FakeApiTestStringMapReferenceResponse.fromResponse(
      createFakeTextResponse(
        statusCode: 2,
        value: "value",
      ),
      wireSerializationOptions: wireOptions,
      userContext: {},
    );
    expect(response, isA<FakeApiTestStringMapReferenceResponse>()
      .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
    );
    expect(response.statusCode, 2);
  });
  group(FakeApiTestStringMapReferenceResponse200, () {
    test('Unknown mime', () async {
      final codeExample = 
    200
;
      response = await FakeApiTestStringMapReferenceResponse.fromResponse(
        createFakeTextResponse(
          statusCode: codeExample,
          value: "value",
          contentType: "application/whatever",
        ),
        wireSerializationOptions: wireOptions,
        userContext: {},
      );
      expect(response.statusCode, codeExample);
      expect(response, isA<FakeApiTestStringMapReferenceResponse200>()
        .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
      );
    });
    
  });

});
    });
  });
}

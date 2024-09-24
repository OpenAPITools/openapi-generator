import 'package:test/test.dart';
import 'package:petstore_api/_internal.dart';
import '../utils.dart';

/// tests for PetApi
void main() {
  final wireOptions = WireSerializationOptions();
  final exampleContext = ExampleContext();
  group(PetApi, () {
    final baseUrl = Uri.https("example.com", "/api");
    // Add a new pet to the store
    //
    // 
    //
    group(r'addPet', () {
      group(PetApiAddPetRequest, () {
    late PetApiAddPetRequest request;
    test(PetApiAddPetRequestUnsafe, () async {
        request = PetApiAddPetRequest.unsafe(
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
    
    test(PetApiAddPetRequestApplicationJson, () async {
        request = PetApiAddPetRequest.applicationJson(
            data: PetApiAddPetRequestApplicationJson.dataReflection.exampleFunction(exampleContext),
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
    
    test(PetApiAddPetRequestApplicationXml, () async {
        request = PetApiAddPetRequest.applicationXml(
            data: PetApiAddPetRequestApplicationXml.dataReflection.exampleFunction(exampleContext),
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

      group(PetApiAddPetResponse, () {
  late PetApiAddPetResponse response;
  test('Unkown status code', () async {
    response = await PetApiAddPetResponse.fromResponse(
      createFakeTextResponse(
        statusCode: 2,
        value: "value",
      ),
      wireSerializationOptions: wireOptions,
      userContext: {},
    );
    expect(response, isA<PetApiAddPetResponse>()
      .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
    );
    expect(response.statusCode, 2);
  });
  group(PetApiAddPetResponse405, () {
    test('Unknown mime', () async {
      final codeExample = 
    405
;
      response = await PetApiAddPetResponse.fromResponse(
        createFakeTextResponse(
          statusCode: codeExample,
          value: "value",
          contentType: "application/whatever",
        ),
        wireSerializationOptions: wireOptions,
        userContext: {},
      );
      expect(response.statusCode, codeExample);
      expect(response, isA<PetApiAddPetResponse405>()
        .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
      );
    });
    
  });

});
    });
    // Deletes a pet
    //
    // 
    //
    group(r'deletePet', () {
      group(PetApiDeletePetRequest, () {
    late PetApiDeletePetRequest request;
    test(r'No Body', () async {
        request = PetApiDeletePetRequest(
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

apiKey: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forString
        
,
)
).exampleFunction(exampleContext),

        );
        final createdRequest = await request.createHttpRequest(baseUrl: baseUrl);
        expect(createdRequest, isNotNull);
    });
});

      group(PetApiDeletePetResponse, () {
  late PetApiDeletePetResponse response;
  test('Unkown status code', () async {
    response = await PetApiDeletePetResponse.fromResponse(
      createFakeTextResponse(
        statusCode: 2,
        value: "value",
      ),
      wireSerializationOptions: wireOptions,
      userContext: {},
    );
    expect(response, isA<PetApiDeletePetResponse>()
      .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
    );
    expect(response.statusCode, 2);
  });
  group(PetApiDeletePetResponse400, () {
    test('Unknown mime', () async {
      final codeExample = 
    400
;
      response = await PetApiDeletePetResponse.fromResponse(
        createFakeTextResponse(
          statusCode: codeExample,
          value: "value",
          contentType: "application/whatever",
        ),
        wireSerializationOptions: wireOptions,
        userContext: {},
      );
      expect(response.statusCode, codeExample);
      expect(response, isA<PetApiDeletePetResponse400>()
        .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
      );
    });
    
  });

});
    });
    // Finds Pets by status
    //
    // Multiple status values can be provided with comma separated strings
    //
    group(r'findPetsByStatus', () {
      group(PetApiFindPetsByStatusRequest, () {
    late PetApiFindPetsByStatusRequest request;
    test(r'No Body', () async {
        request = PetApiFindPetsByStatusRequest(
            extraHeaders: {
                "hello":"world",
            },
            extraQueryParameters: {
                "welcome": "back",
            },
            extraCookies: {
                "nice": "work",
            },
            
status: XmlReflectionWrapper(
    xml: XmlReflection(
),
    
    ListReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            StatusEnum.$reflection
        
        
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

      group(PetApiFindPetsByStatusResponse, () {
  late PetApiFindPetsByStatusResponse response;
  test('Unkown status code', () async {
    response = await PetApiFindPetsByStatusResponse.fromResponse(
      createFakeTextResponse(
        statusCode: 2,
        value: "value",
      ),
      wireSerializationOptions: wireOptions,
      userContext: {},
    );
    expect(response, isA<PetApiFindPetsByStatusResponse>()
      .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
    );
    expect(response.statusCode, 2);
  });
  group(PetApiFindPetsByStatusResponse200, () {
    test('Unknown mime', () async {
      final codeExample = 
    200
;
      response = await PetApiFindPetsByStatusResponse.fromResponse(
        createFakeTextResponse(
          statusCode: codeExample,
          value: "value",
          contentType: "application/whatever",
        ),
        wireSerializationOptions: wireOptions,
        userContext: {},
      );
      expect(response.statusCode, codeExample);
      expect(response, isA<PetApiFindPetsByStatusResponse200>()
        .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
      );
    });
    
    test(PetApiFindPetsByStatusResponse200ApplicationXml, () async {
      final codeExample = 
    200
;
      final mimeExample = MediaType.parse(exampleContext.exampleMimeType(r'application/xml'));
      final bodyReflection = PetApiFindPetsByStatusResponse200ApplicationXml.bodyReflection;
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

      response = await PetApiFindPetsByStatusResponse.fromResponse(
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
    
    test(PetApiFindPetsByStatusResponse200ApplicationJson, () async {
      final codeExample = 
    200
;
      final mimeExample = MediaType.parse(exampleContext.exampleMimeType(r'application/json'));
      final bodyReflection = PetApiFindPetsByStatusResponse200ApplicationJson.bodyReflection;
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

      response = await PetApiFindPetsByStatusResponse.fromResponse(
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
  group(PetApiFindPetsByStatusResponse400, () {
    test('Unknown mime', () async {
      final codeExample = 
    400
;
      response = await PetApiFindPetsByStatusResponse.fromResponse(
        createFakeTextResponse(
          statusCode: codeExample,
          value: "value",
          contentType: "application/whatever",
        ),
        wireSerializationOptions: wireOptions,
        userContext: {},
      );
      expect(response.statusCode, codeExample);
      expect(response, isA<PetApiFindPetsByStatusResponse400>()
        .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
      );
    });
    
  });

});
    });
    // Finds Pets by tags
    //
    // Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
    //
    group(r'findPetsByTags', () {
      group(PetApiFindPetsByTagsRequest, () {
    late PetApiFindPetsByTagsRequest request;
    test(r'No Body', () async {
        request = PetApiFindPetsByTagsRequest(
            extraHeaders: {
                "hello":"world",
            },
            extraQueryParameters: {
                "welcome": "back",
            },
            extraCookies: {
                "nice": "work",
            },
            
tags: XmlReflectionWrapper(
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

      group(PetApiFindPetsByTagsResponse, () {
  late PetApiFindPetsByTagsResponse response;
  test('Unkown status code', () async {
    response = await PetApiFindPetsByTagsResponse.fromResponse(
      createFakeTextResponse(
        statusCode: 2,
        value: "value",
      ),
      wireSerializationOptions: wireOptions,
      userContext: {},
    );
    expect(response, isA<PetApiFindPetsByTagsResponse>()
      .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
    );
    expect(response.statusCode, 2);
  });
  group(PetApiFindPetsByTagsResponse200, () {
    test('Unknown mime', () async {
      final codeExample = 
    200
;
      response = await PetApiFindPetsByTagsResponse.fromResponse(
        createFakeTextResponse(
          statusCode: codeExample,
          value: "value",
          contentType: "application/whatever",
        ),
        wireSerializationOptions: wireOptions,
        userContext: {},
      );
      expect(response.statusCode, codeExample);
      expect(response, isA<PetApiFindPetsByTagsResponse200>()
        .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
      );
    });
    
    test(PetApiFindPetsByTagsResponse200ApplicationXml, () async {
      final codeExample = 
    200
;
      final mimeExample = MediaType.parse(exampleContext.exampleMimeType(r'application/xml'));
      final bodyReflection = PetApiFindPetsByTagsResponse200ApplicationXml.bodyReflection;
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

      response = await PetApiFindPetsByTagsResponse.fromResponse(
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
    
    test(PetApiFindPetsByTagsResponse200ApplicationJson, () async {
      final codeExample = 
    200
;
      final mimeExample = MediaType.parse(exampleContext.exampleMimeType(r'application/json'));
      final bodyReflection = PetApiFindPetsByTagsResponse200ApplicationJson.bodyReflection;
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

      response = await PetApiFindPetsByTagsResponse.fromResponse(
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
  group(PetApiFindPetsByTagsResponse400, () {
    test('Unknown mime', () async {
      final codeExample = 
    400
;
      response = await PetApiFindPetsByTagsResponse.fromResponse(
        createFakeTextResponse(
          statusCode: codeExample,
          value: "value",
          contentType: "application/whatever",
        ),
        wireSerializationOptions: wireOptions,
        userContext: {},
      );
      expect(response.statusCode, codeExample);
      expect(response, isA<PetApiFindPetsByTagsResponse400>()
        .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
      );
    });
    
  });

});
    });
    // Find pet by ID
    //
    // Returns a single pet
    //
    group(r'getPetById', () {
      group(PetApiGetPetByIdRequest, () {
    late PetApiGetPetByIdRequest request;
    test(r'No Body', () async {
        request = PetApiGetPetByIdRequest(
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

      group(PetApiGetPetByIdResponse, () {
  late PetApiGetPetByIdResponse response;
  test('Unkown status code', () async {
    response = await PetApiGetPetByIdResponse.fromResponse(
      createFakeTextResponse(
        statusCode: 2,
        value: "value",
      ),
      wireSerializationOptions: wireOptions,
      userContext: {},
    );
    expect(response, isA<PetApiGetPetByIdResponse>()
      .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
    );
    expect(response.statusCode, 2);
  });
  group(PetApiGetPetByIdResponse200, () {
    test('Unknown mime', () async {
      final codeExample = 
    200
;
      response = await PetApiGetPetByIdResponse.fromResponse(
        createFakeTextResponse(
          statusCode: codeExample,
          value: "value",
          contentType: "application/whatever",
        ),
        wireSerializationOptions: wireOptions,
        userContext: {},
      );
      expect(response.statusCode, codeExample);
      expect(response, isA<PetApiGetPetByIdResponse200>()
        .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
      );
    });
    
    test(PetApiGetPetByIdResponse200ApplicationXml, () async {
      final codeExample = 
    200
;
      final mimeExample = MediaType.parse(exampleContext.exampleMimeType(r'application/xml'));
      final bodyReflection = PetApiGetPetByIdResponse200ApplicationXml.bodyReflection;
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

      response = await PetApiGetPetByIdResponse.fromResponse(
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
    
    test(PetApiGetPetByIdResponse200ApplicationJson, () async {
      final codeExample = 
    200
;
      final mimeExample = MediaType.parse(exampleContext.exampleMimeType(r'application/json'));
      final bodyReflection = PetApiGetPetByIdResponse200ApplicationJson.bodyReflection;
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

      response = await PetApiGetPetByIdResponse.fromResponse(
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
  group(PetApiGetPetByIdResponse400, () {
    test('Unknown mime', () async {
      final codeExample = 
    400
;
      response = await PetApiGetPetByIdResponse.fromResponse(
        createFakeTextResponse(
          statusCode: codeExample,
          value: "value",
          contentType: "application/whatever",
        ),
        wireSerializationOptions: wireOptions,
        userContext: {},
      );
      expect(response.statusCode, codeExample);
      expect(response, isA<PetApiGetPetByIdResponse400>()
        .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
      );
    });
    
  });
  group(PetApiGetPetByIdResponse404, () {
    test('Unknown mime', () async {
      final codeExample = 
    404
;
      response = await PetApiGetPetByIdResponse.fromResponse(
        createFakeTextResponse(
          statusCode: codeExample,
          value: "value",
          contentType: "application/whatever",
        ),
        wireSerializationOptions: wireOptions,
        userContext: {},
      );
      expect(response.statusCode, codeExample);
      expect(response, isA<PetApiGetPetByIdResponse404>()
        .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
      );
    });
    
  });

});
    });
    // Update an existing pet
    //
    // 
    //
    group(r'updatePet', () {
      group(PetApiUpdatePetRequest, () {
    late PetApiUpdatePetRequest request;
    test(PetApiUpdatePetRequestUnsafe, () async {
        request = PetApiUpdatePetRequest.unsafe(
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
    
    test(PetApiUpdatePetRequestApplicationJson, () async {
        request = PetApiUpdatePetRequest.applicationJson(
            data: PetApiUpdatePetRequestApplicationJson.dataReflection.exampleFunction(exampleContext),
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
    
    test(PetApiUpdatePetRequestApplicationXml, () async {
        request = PetApiUpdatePetRequest.applicationXml(
            data: PetApiUpdatePetRequestApplicationXml.dataReflection.exampleFunction(exampleContext),
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

      group(PetApiUpdatePetResponse, () {
  late PetApiUpdatePetResponse response;
  test('Unkown status code', () async {
    response = await PetApiUpdatePetResponse.fromResponse(
      createFakeTextResponse(
        statusCode: 2,
        value: "value",
      ),
      wireSerializationOptions: wireOptions,
      userContext: {},
    );
    expect(response, isA<PetApiUpdatePetResponse>()
      .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
    );
    expect(response.statusCode, 2);
  });
  group(PetApiUpdatePetResponse400, () {
    test('Unknown mime', () async {
      final codeExample = 
    400
;
      response = await PetApiUpdatePetResponse.fromResponse(
        createFakeTextResponse(
          statusCode: codeExample,
          value: "value",
          contentType: "application/whatever",
        ),
        wireSerializationOptions: wireOptions,
        userContext: {},
      );
      expect(response.statusCode, codeExample);
      expect(response, isA<PetApiUpdatePetResponse400>()
        .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
      );
    });
    
  });
  group(PetApiUpdatePetResponse404, () {
    test('Unknown mime', () async {
      final codeExample = 
    404
;
      response = await PetApiUpdatePetResponse.fromResponse(
        createFakeTextResponse(
          statusCode: codeExample,
          value: "value",
          contentType: "application/whatever",
        ),
        wireSerializationOptions: wireOptions,
        userContext: {},
      );
      expect(response.statusCode, codeExample);
      expect(response, isA<PetApiUpdatePetResponse404>()
        .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
      );
    });
    
  });
  group(PetApiUpdatePetResponse405, () {
    test('Unknown mime', () async {
      final codeExample = 
    405
;
      response = await PetApiUpdatePetResponse.fromResponse(
        createFakeTextResponse(
          statusCode: codeExample,
          value: "value",
          contentType: "application/whatever",
        ),
        wireSerializationOptions: wireOptions,
        userContext: {},
      );
      expect(response.statusCode, codeExample);
      expect(response, isA<PetApiUpdatePetResponse405>()
        .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
      );
    });
    
  });

});
    });
    // Updates a pet in the store with form data
    //
    // 
    //
    group(r'updatePetWithForm', () {
      group(PetApiUpdatePetWithFormRequest, () {
    late PetApiUpdatePetWithFormRequest request;
    test(PetApiUpdatePetWithFormRequestUnsafe, () async {
        request = PetApiUpdatePetWithFormRequest.unsafe(
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

      group(PetApiUpdatePetWithFormResponse, () {
  late PetApiUpdatePetWithFormResponse response;
  test('Unkown status code', () async {
    response = await PetApiUpdatePetWithFormResponse.fromResponse(
      createFakeTextResponse(
        statusCode: 2,
        value: "value",
      ),
      wireSerializationOptions: wireOptions,
      userContext: {},
    );
    expect(response, isA<PetApiUpdatePetWithFormResponse>()
      .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
    );
    expect(response.statusCode, 2);
  });
  group(PetApiUpdatePetWithFormResponse405, () {
    test('Unknown mime', () async {
      final codeExample = 
    405
;
      response = await PetApiUpdatePetWithFormResponse.fromResponse(
        createFakeTextResponse(
          statusCode: codeExample,
          value: "value",
          contentType: "application/whatever",
        ),
        wireSerializationOptions: wireOptions,
        userContext: {},
      );
      expect(response.statusCode, codeExample);
      expect(response, isA<PetApiUpdatePetWithFormResponse405>()
        .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
      );
    });
    
  });

});
    });
    // uploads an image
    //
    // 
    //
    group(r'uploadFile', () {
      group(PetApiUploadFileRequest, () {
    late PetApiUploadFileRequest request;
    test(PetApiUploadFileRequestUnsafe, () async {
        request = PetApiUploadFileRequest.unsafe(
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

      group(PetApiUploadFileResponse, () {
  late PetApiUploadFileResponse response;
  test('Unkown status code', () async {
    response = await PetApiUploadFileResponse.fromResponse(
      createFakeTextResponse(
        statusCode: 2,
        value: "value",
      ),
      wireSerializationOptions: wireOptions,
      userContext: {},
    );
    expect(response, isA<PetApiUploadFileResponse>()
      .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
    );
    expect(response.statusCode, 2);
  });
  group(PetApiUploadFileResponse200, () {
    test('Unknown mime', () async {
      final codeExample = 
    200
;
      response = await PetApiUploadFileResponse.fromResponse(
        createFakeTextResponse(
          statusCode: codeExample,
          value: "value",
          contentType: "application/whatever",
        ),
        wireSerializationOptions: wireOptions,
        userContext: {},
      );
      expect(response.statusCode, codeExample);
      expect(response, isA<PetApiUploadFileResponse200>()
        .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
      );
    });
    
    test(PetApiUploadFileResponse200ApplicationJson, () async {
      final codeExample = 
    200
;
      final mimeExample = MediaType.parse(exampleContext.exampleMimeType(r'application/json'));
      final bodyReflection = PetApiUploadFileResponse200ApplicationJson.bodyReflection;
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

      response = await PetApiUploadFileResponse.fromResponse(
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
    // uploads an image (required)
    //
    // 
    //
    group(r'uploadFileWithRequiredFile', () {
      group(PetApiUploadFileWithRequiredFileRequest, () {
    late PetApiUploadFileWithRequiredFileRequest request;
    test(PetApiUploadFileWithRequiredFileRequestUnsafe, () async {
        request = PetApiUploadFileWithRequiredFileRequest.unsafe(
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

      group(PetApiUploadFileWithRequiredFileResponse, () {
  late PetApiUploadFileWithRequiredFileResponse response;
  test('Unkown status code', () async {
    response = await PetApiUploadFileWithRequiredFileResponse.fromResponse(
      createFakeTextResponse(
        statusCode: 2,
        value: "value",
      ),
      wireSerializationOptions: wireOptions,
      userContext: {},
    );
    expect(response, isA<PetApiUploadFileWithRequiredFileResponse>()
      .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
    );
    expect(response.statusCode, 2);
  });
  group(PetApiUploadFileWithRequiredFileResponse200, () {
    test('Unknown mime', () async {
      final codeExample = 
    200
;
      response = await PetApiUploadFileWithRequiredFileResponse.fromResponse(
        createFakeTextResponse(
          statusCode: codeExample,
          value: "value",
          contentType: "application/whatever",
        ),
        wireSerializationOptions: wireOptions,
        userContext: {},
      );
      expect(response.statusCode, codeExample);
      expect(response, isA<PetApiUploadFileWithRequiredFileResponse200>()
        .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
      );
    });
    
    test(PetApiUploadFileWithRequiredFileResponse200ApplicationJson, () async {
      final codeExample = 
    200
;
      final mimeExample = MediaType.parse(exampleContext.exampleMimeType(r'application/json'));
      final bodyReflection = PetApiUploadFileWithRequiredFileResponse200ApplicationJson.bodyReflection;
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

      response = await PetApiUploadFileWithRequiredFileResponse.fromResponse(
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
  });
}

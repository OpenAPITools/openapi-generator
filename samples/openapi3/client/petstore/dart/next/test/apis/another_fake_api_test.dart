import 'package:test/test.dart';
import 'package:petstore_api/_internal.dart';
import '../utils.dart';

/// tests for AnotherFakeApi
void main() {
  final wireOptions = WireSerializationOptions();
  final exampleContext = ExampleContext();
  group(AnotherFakeApi, () {
    final baseUrl = Uri.https("example.com", "/api");
    // To test special tags
    //
    // To test special tags and operation ID starting with number
    //
    group(r'$123testSpecialTags', () {
      group(AnotherFakeApi$123testSpecialTagsRequest, () {
    late AnotherFakeApi$123testSpecialTagsRequest request;
    test(AnotherFakeApi$123testSpecialTagsRequestUnsafe, () async {
        request = AnotherFakeApi$123testSpecialTagsRequest.unsafe(
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
    
    test(AnotherFakeApi$123testSpecialTagsRequestApplicationJson, () async {
        request = AnotherFakeApi$123testSpecialTagsRequest.applicationJson(
            data: AnotherFakeApi$123testSpecialTagsRequestApplicationJson.dataReflection.exampleFunction(exampleContext),
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

      group(AnotherFakeApi$123testSpecialTagsResponse, () {
  late AnotherFakeApi$123testSpecialTagsResponse response;
  test('Unkown status code', () async {
    response = await AnotherFakeApi$123testSpecialTagsResponse.fromResponse(
      createFakeTextResponse(
        statusCode: 2,
        value: "value",
      ),
      wireSerializationOptions: wireOptions,
      userContext: {},
    );
    expect(response, isA<AnotherFakeApi$123testSpecialTagsResponse>()
      .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
    );
    expect(response.statusCode, 2);
  });
  group(AnotherFakeApi$123testSpecialTagsResponse200, () {
    test('Unknown mime', () async {
      final codeExample = 
    200
;
      response = await AnotherFakeApi$123testSpecialTagsResponse.fromResponse(
        createFakeTextResponse(
          statusCode: codeExample,
          value: "value",
          contentType: "application/whatever",
        ),
        wireSerializationOptions: wireOptions,
        userContext: {},
      );
      expect(response.statusCode, codeExample);
      expect(response, isA<AnotherFakeApi$123testSpecialTagsResponse200>()
        .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
      );
    });
    
    test(AnotherFakeApi$123testSpecialTagsResponse200ApplicationJson, () async {
      final codeExample = 
    200
;
      final mimeExample = MediaType.parse(exampleContext.exampleMimeType(r'application/json'));
      final bodyReflection = AnotherFakeApi$123testSpecialTagsResponse200ApplicationJson.bodyReflection;
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

      response = await AnotherFakeApi$123testSpecialTagsResponse.fromResponse(
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
    // parameter array number default value
    //
    group(r'getParameterArrayNumber', () {
      group(AnotherFakeApiGetParameterArrayNumberRequest, () {
    late AnotherFakeApiGetParameterArrayNumberRequest request;
    test(r'No Body', () async {
        request = AnotherFakeApiGetParameterArrayNumberRequest(
            extraHeaders: {
                "hello":"world",
            },
            extraQueryParameters: {
                "welcome": "back",
            },
            extraCookies: {
                "nice": "work",
            },
            
array: XmlReflectionWrapper(
    xml: XmlReflection(
),
    
    ListReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forint
        
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

      group(AnotherFakeApiGetParameterArrayNumberResponse, () {
  late AnotherFakeApiGetParameterArrayNumberResponse response;
  test('Unkown status code', () async {
    response = await AnotherFakeApiGetParameterArrayNumberResponse.fromResponse(
      createFakeTextResponse(
        statusCode: 2,
        value: "value",
      ),
      wireSerializationOptions: wireOptions,
      userContext: {},
    );
    expect(response, isA<AnotherFakeApiGetParameterArrayNumberResponse>()
      .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
    );
    expect(response.statusCode, 2);
  });
  group(AnotherFakeApiGetParameterArrayNumberResponse200, () {
    test('Unknown mime', () async {
      final codeExample = 
    200
;
      response = await AnotherFakeApiGetParameterArrayNumberResponse.fromResponse(
        createFakeTextResponse(
          statusCode: codeExample,
          value: "value",
          contentType: "application/whatever",
        ),
        wireSerializationOptions: wireOptions,
        userContext: {},
      );
      expect(response.statusCode, codeExample);
      expect(response, isA<AnotherFakeApiGetParameterArrayNumberResponse200>()
        .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
      );
    });
    
  });

});
    });
    // parameter string number
    //
    group(r'getParameterStringNumber', () {
      group(AnotherFakeApiGetParameterStringNumberRequest, () {
    late AnotherFakeApiGetParameterStringNumberRequest request;
    test(r'No Body', () async {
        request = AnotherFakeApiGetParameterStringNumberRequest(
            extraHeaders: {
                "hello":"world",
            },
            extraQueryParameters: {
                "welcome": "back",
            },
            extraCookies: {
                "nice": "work",
            },
            
stringNumber: XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.fordouble
        
,
)
.exampleFunction(exampleContext),

        );
        final createdRequest = await request.createHttpRequest(baseUrl: baseUrl);
        expect(createdRequest, isNotNull);
    });
});

      group(AnotherFakeApiGetParameterStringNumberResponse, () {
  late AnotherFakeApiGetParameterStringNumberResponse response;
  test('Unkown status code', () async {
    response = await AnotherFakeApiGetParameterStringNumberResponse.fromResponse(
      createFakeTextResponse(
        statusCode: 2,
        value: "value",
      ),
      wireSerializationOptions: wireOptions,
      userContext: {},
    );
    expect(response, isA<AnotherFakeApiGetParameterStringNumberResponse>()
      .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
    );
    expect(response.statusCode, 2);
  });
  group(AnotherFakeApiGetParameterStringNumberResponse200, () {
    test('Unknown mime', () async {
      final codeExample = 
    200
;
      response = await AnotherFakeApiGetParameterStringNumberResponse.fromResponse(
        createFakeTextResponse(
          statusCode: codeExample,
          value: "value",
          contentType: "application/whatever",
        ),
        wireSerializationOptions: wireOptions,
        userContext: {},
      );
      expect(response.statusCode, codeExample);
      expect(response, isA<AnotherFakeApiGetParameterStringNumberResponse200>()
        .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
      );
    });
    
  });

});
    });
    // null request body
    //
    group(r'nullRequestBody', () {
      group(AnotherFakeApiNullRequestBodyRequest, () {
    late AnotherFakeApiNullRequestBodyRequest request;
    test(r'No Body', () async {
        request = AnotherFakeApiNullRequestBodyRequest(
            extraHeaders: {
                "hello":"world",
            },
            extraQueryParameters: {
                "welcome": "back",
            },
            extraCookies: {
                "nice": "work",
            },
            
acceptLanguage: UndefinedWrapperReflection(XmlReflectionWrapper(
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

      group(AnotherFakeApiNullRequestBodyResponse, () {
  late AnotherFakeApiNullRequestBodyResponse response;
  test('Unkown status code', () async {
    response = await AnotherFakeApiNullRequestBodyResponse.fromResponse(
      createFakeTextResponse(
        statusCode: 2,
        value: "value",
      ),
      wireSerializationOptions: wireOptions,
      userContext: {},
    );
    expect(response, isA<AnotherFakeApiNullRequestBodyResponse>()
      .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
    );
    expect(response.statusCode, 2);
  });
  group(AnotherFakeApiNullRequestBodyResponse200, () {
    test('Unknown mime', () async {
      final codeExample = 
    200
;
      response = await AnotherFakeApiNullRequestBodyResponse.fromResponse(
        createFakeTextResponse(
          statusCode: codeExample,
          value: "value",
          contentType: "application/whatever",
        ),
        wireSerializationOptions: wireOptions,
        userContext: {},
      );
      expect(response.statusCode, codeExample);
      expect(response, isA<AnotherFakeApiNullRequestBodyResponse200>()
        .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
      );
    });
    
  });

});
    });
  });
}

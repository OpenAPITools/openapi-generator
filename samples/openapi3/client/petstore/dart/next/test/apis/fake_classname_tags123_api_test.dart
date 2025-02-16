import 'package:test/test.dart';
import 'package:petstore_api/_internal.dart';
import '../utils.dart';

/// tests for FakeClassnameTags123Api
void main() {
  final wireOptions = WireSerializationOptions();
  final exampleContext = ExampleContext();
  group(FakeClassnameTags123Api, () {
    final baseUrl = Uri.https("example.com", "/api");
    // To test class name in snake case
    //
    // To test class name in snake case
    //
    group(r'testClassname', () {
      group(FakeClassnameTags123ApiTestClassnameRequest, () {
    late FakeClassnameTags123ApiTestClassnameRequest request;
    test(FakeClassnameTags123ApiTestClassnameRequestUnsafe, () async {
        request = FakeClassnameTags123ApiTestClassnameRequest.unsafe(
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
    
    test(FakeClassnameTags123ApiTestClassnameRequestApplicationJson, () async {
        request = FakeClassnameTags123ApiTestClassnameRequest.applicationJson(
            data: FakeClassnameTags123ApiTestClassnameRequestApplicationJson.dataReflection.exampleFunction(exampleContext),
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

      group(FakeClassnameTags123ApiTestClassnameResponse, () {
  late FakeClassnameTags123ApiTestClassnameResponse response;
  test('Unkown status code', () async {
    response = await FakeClassnameTags123ApiTestClassnameResponse.fromResponse(
      createFakeTextResponse(
        statusCode: 2,
        value: "value",
      ),
      wireSerializationOptions: wireOptions,
      userContext: {},
    );
    expect(response, isA<FakeClassnameTags123ApiTestClassnameResponse>()
      .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
    );
    expect(response.statusCode, 2);
  });
  group(FakeClassnameTags123ApiTestClassnameResponse200, () {
    test('Unknown mime', () async {
      final codeExample = 
    200
;
      response = await FakeClassnameTags123ApiTestClassnameResponse.fromResponse(
        createFakeTextResponse(
          statusCode: codeExample,
          value: "value",
          contentType: "application/whatever",
        ),
        wireSerializationOptions: wireOptions,
        userContext: {},
      );
      expect(response.statusCode, codeExample);
      expect(response, isA<FakeClassnameTags123ApiTestClassnameResponse200>()
        .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
      );
    });
    
    test(FakeClassnameTags123ApiTestClassnameResponse200ApplicationJson, () async {
      final codeExample = 
    200
;
      final mimeExample = MediaType.parse(exampleContext.exampleMimeType(r'application/json'));
      final bodyReflection = FakeClassnameTags123ApiTestClassnameResponse200ApplicationJson.bodyReflection;
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

      response = await FakeClassnameTags123ApiTestClassnameResponse.fromResponse(
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

import 'package:test/test.dart';
import 'package:petstore_api/_internal.dart';
import '../utils.dart';

/// tests for ValuesApi
void main() {
  final wireOptions = WireSerializationOptions();
  final exampleContext = ExampleContext();
  group(ValuesApi, () {
    final baseUrl = Uri.https("example.com", "/api");
    // Get some primitive variable values
    //
    // 
    //
    group(r'getSomeValues', () {
      group(ValuesApiGetSomeValuesRequest, () {
    late ValuesApiGetSomeValuesRequest request;
    test(r'No Body', () async {
        request = ValuesApiGetSomeValuesRequest(
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

      group(ValuesApiGetSomeValuesResponse, () {
  late ValuesApiGetSomeValuesResponse response;
  test('Unkown status code', () async {
    response = await ValuesApiGetSomeValuesResponse.fromResponse(
      createFakeTextResponse(
        statusCode: 2,
        value: "value",
      ),
      wireSerializationOptions: wireOptions,
      userContext: {},
    );
    expect(response, isA<ValuesApiGetSomeValuesResponse>()
      .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
    );
    expect(response.statusCode, 2);
  });
  group(ValuesApiGetSomeValuesResponse200, () {
    test('Unknown mime', () async {
      final codeExample = 
    200
;
      response = await ValuesApiGetSomeValuesResponse.fromResponse(
        createFakeTextResponse(
          statusCode: codeExample,
          value: "value",
          contentType: "application/whatever",
        ),
        wireSerializationOptions: wireOptions,
        userContext: {},
      );
      expect(response.statusCode, codeExample);
      expect(response, isA<ValuesApiGetSomeValuesResponse200>()
        .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
      );
    });
    
    test(ValuesApiGetSomeValuesResponse200ApplicationJson, () async {
      final codeExample = 
    200
;
      final mimeExample = MediaType.parse(exampleContext.exampleMimeType(r'application/json'));
      final bodyReflection = ValuesApiGetSomeValuesResponse200ApplicationJson.bodyReflection;
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

      response = await ValuesApiGetSomeValuesResponse.fromResponse(
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
  group(ValuesApiGetSomeValuesResponse400, () {
    test('Unknown mime', () async {
      final codeExample = 
    400
;
      response = await ValuesApiGetSomeValuesResponse.fromResponse(
        createFakeTextResponse(
          statusCode: codeExample,
          value: "value",
          contentType: "application/whatever",
        ),
        wireSerializationOptions: wireOptions,
        userContext: {},
      );
      expect(response.statusCode, codeExample);
      expect(response, isA<ValuesApiGetSomeValuesResponse400>()
        .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
      );
    });
    
  });

});
    });
  });
}

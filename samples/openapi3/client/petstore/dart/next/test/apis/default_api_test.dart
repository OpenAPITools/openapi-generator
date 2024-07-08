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

/// tests for DefaultApi
void main() {
  group(DefaultApi, () {
    final baseUrl = Uri.https("example.com", "/api");
    // Test route, this shouldn't cause a compiler error
    //
    group(r'fakeAnyOfWIthSameErasureGet', () {
      group(DefaultApiFakeAnyOfWIthSameErasureGetRequest, () {
    late DefaultApiFakeAnyOfWIthSameErasureGetRequest request;
    test(r'No Body', () async {
        request = DefaultApiFakeAnyOfWIthSameErasureGetRequest(
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

      group(DefaultApiFakeAnyOfWIthSameErasureGetResponse, () {
  late DefaultApiFakeAnyOfWIthSameErasureGetResponse response;
  test('Unkown status code', () async {
    response = await DefaultApiFakeAnyOfWIthSameErasureGetResponse.fromResponse(
      createFakeTextResponse(
        statusCode: 2,
        value: "value",
      ),
      context: {},
    );
    expect(response.statusCode, 2);
  });
  group(DefaultApiFakeAnyOfWIthSameErasureGetResponse200, () {
    test('Unkown mime', () async {

    });
    
    test(DefaultApiFakeAnyOfWIthSameErasureGetResponse200ApplicationJson, () async {

    });
    
  });

});
    });
    // Test route, this shouldn't cause a compiler error
    //
    group(r'fakeOneOfWIthSameErasureGet', () {
      group(DefaultApiFakeOneOfWIthSameErasureGetRequest, () {
    late DefaultApiFakeOneOfWIthSameErasureGetRequest request;
    test(r'No Body', () async {
        request = DefaultApiFakeOneOfWIthSameErasureGetRequest(
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

      group(DefaultApiFakeOneOfWIthSameErasureGetResponse, () {
  late DefaultApiFakeOneOfWIthSameErasureGetResponse response;
  test('Unkown status code', () async {
    response = await DefaultApiFakeOneOfWIthSameErasureGetResponse.fromResponse(
      createFakeTextResponse(
        statusCode: 2,
        value: "value",
      ),
      context: {},
    );
    expect(response.statusCode, 2);
  });
  group(DefaultApiFakeOneOfWIthSameErasureGetResponse200, () {
    test('Unkown mime', () async {

    });
    
    test(DefaultApiFakeOneOfWIthSameErasureGetResponse200ApplicationJson, () async {

    });
    
  });

});
    });
    group(r'fooGet', () {
      group(DefaultApiFooGetRequest, () {
    late DefaultApiFooGetRequest request;
    test(r'No Body', () async {
        request = DefaultApiFooGetRequest(
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

      group(DefaultApiFooGetResponse, () {
  late DefaultApiFooGetResponse response;
  test('Unkown status code', () async {
    response = await DefaultApiFooGetResponse.fromResponse(
      createFakeTextResponse(
        statusCode: 2,
        value: "value",
      ),
      context: {},
    );
    expect(response.statusCode, 2);
  });
  group(DefaultApiFooGetResponseDefault, () {
    test('Unkown mime', () async {

    });
    
    test(DefaultApiFooGetResponseDefaultApplicationJson, () async {

    });
    
  });

});
    });
    // Add a new pet
    //
    group(r'petsMulticontentTestPost', () {
      group(DefaultApiPetsMulticontentTestPostRequest, () {
    late DefaultApiPetsMulticontentTestPostRequest request;
    test(DefaultApiPetsMulticontentTestPostRequestUnsafe, () async {
        request = DefaultApiPetsMulticontentTestPostRequest.unsafe(
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
    
    test(DefaultApiPetsMulticontentTestPostRequestApplicationJson, () async {
        request = DefaultApiPetsMulticontentTestPostRequest.applicationJson(
            data: 


            
            


    PetReflection.instance.example()
    


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
    
    test(DefaultApiPetsMulticontentTestPostRequestApplicationXml, () async {
        request = DefaultApiPetsMulticontentTestPostRequest.applicationXml(
            data: 


            
            


    NewPetReflection.instance.example()
    


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
    
    test(DefaultApiPetsMulticontentTestPostRequestApplicationXWwwFormUrlencoded, () async {
        request = DefaultApiPetsMulticontentTestPostRequest.applicationXWwwFormUrlencoded(
            data: 


            
            


    TriangleReflection.instance.example()
    


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
    
    test(DefaultApiPetsMulticontentTestPostRequestTextPlain, () async {
        request = DefaultApiPetsMulticontentTestPostRequest.textPlain(
            data: 


            
            


    
    exampleint()


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
    
    test(DefaultApiPetsMulticontentTestPostRequestAnyAny, () async {
        request = DefaultApiPetsMulticontentTestPostRequest.anyAny(
            data: exampleNullable(() =>

exampleObject()



 ) ,
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
    
    test(DefaultApiPetsMulticontentTestPostRequestTextAny, () async {
        request = DefaultApiPetsMulticontentTestPostRequest.textAny(
            data: 


            
            


    
    exampleString()


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
    
    test(DefaultApiPetsMulticontentTestPostRequestMultipartFormData, () async {
        request = DefaultApiPetsMulticontentTestPostRequest.multipartFormData(
            data: 


            
            


    PetsMulticontentTestPostRequestReflection.instance.example()
    


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

      group(DefaultApiPetsMulticontentTestPostResponse, () {
  late DefaultApiPetsMulticontentTestPostResponse response;
  test('Unkown status code', () async {
    response = await DefaultApiPetsMulticontentTestPostResponse.fromResponse(
      createFakeTextResponse(
        statusCode: 2,
        value: "value",
      ),
      context: {},
    );
    expect(response.statusCode, 2);
  });
  group(DefaultApiPetsMulticontentTestPostResponse200, () {
    test('Unkown mime', () async {

    });
    
    test(DefaultApiPetsMulticontentTestPostResponse200TextPlain, () async {

    });
    
    test(DefaultApiPetsMulticontentTestPostResponse200AnyAny, () async {

    });
    
    test(DefaultApiPetsMulticontentTestPostResponse200TextAny, () async {

    });
    
    test(DefaultApiPetsMulticontentTestPostResponse200MultipartFormData, () async {

    });
    
  });
  group(DefaultApiPetsMulticontentTestPostResponse201, () {
    test('Unkown mime', () async {

    });
    
    test(DefaultApiPetsMulticontentTestPostResponse201ApplicationJson, () async {

    });
    
    test(DefaultApiPetsMulticontentTestPostResponse201ApplicationXml, () async {

    });
    
    test(DefaultApiPetsMulticontentTestPostResponse201ApplicationXWwwFormUrlencoded, () async {

    });
    
  });
  group(DefaultApiPetsMulticontentTestPostResponse2XX, () {
    test('Unkown mime', () async {

    });
    
    test(DefaultApiPetsMulticontentTestPostResponse2XXApplicationJson, () async {

    });
    
  });
  group(DefaultApiPetsMulticontentTestPostResponseDefault, () {
    test('Unkown mime', () async {

    });
    
    test(DefaultApiPetsMulticontentTestPostResponseDefaultApplicationJson, () async {

    });
    
    test(DefaultApiPetsMulticontentTestPostResponseDefaultApplicationXWwwFormUrlencoded, () async {

    });
    
  });

});
    });
  });
}

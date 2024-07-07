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

/// tests for AnotherFakeApi
void main() {
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
            data: 


            
            


    Client.$reflection.example()
    


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

      group(AnotherFakeApi$123testSpecialTagsResponse, () {
  late AnotherFakeApi$123testSpecialTagsResponse response;
  test('Unkown status code', () async {
    response = await AnotherFakeApi$123testSpecialTagsResponse.fromResponse(
      createFakeTextResponse(
        statusCode: 2,
        value: "value",
      ),
      context: {},
    );
    expect(response.statusCode, 2);
  });
  group(AnotherFakeApi$123testSpecialTagsResponse200, () {
    test('Unkown mime', () async {

    });
    
    test(AnotherFakeApi$123testSpecialTagsResponse200ApplicationJson, () async {

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
            
array:  


    exampleList(() { _partReflection = _partReflection?.itemsReflection; return 


            
            


    
    exampleint()


; })



,

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
      context: {},
    );
    expect(response.statusCode, 2);
  });
  group(AnotherFakeApiGetParameterArrayNumberResponse200, () {
    test('Unkown mime', () async {

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
            
stringNumber:  


            
            


    
    exampledouble()


,

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
      context: {},
    );
    expect(response.statusCode, 2);
  });
  group(AnotherFakeApiGetParameterStringNumberResponse200, () {
    test('Unkown mime', () async {

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
            
acceptLanguage: UndefinedWrapper( 


            
            


    
    exampleString()


),

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
      context: {},
    );
    expect(response.statusCode, 2);
  });
  group(AnotherFakeApiNullRequestBodyResponse200, () {
    test('Unkown mime', () async {

    });
    
  });

});
    });
  });
}

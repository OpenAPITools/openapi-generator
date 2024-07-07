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

/// tests for FakeClassnameTags123Api
void main() {
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

      group(FakeClassnameTags123ApiTestClassnameResponse, () {
  late FakeClassnameTags123ApiTestClassnameResponse response;
  test('Unkown status code', () async {
    response = await FakeClassnameTags123ApiTestClassnameResponse.fromResponse(
      createFakeTextResponse(
        statusCode: 2,
        value: "value",
      ),
      context: {},
    );
    expect(response.statusCode, 2);
  });
  group(FakeClassnameTags123ApiTestClassnameResponse200, () {
    test('Unkown mime', () async {

    });
    
    test(FakeClassnameTags123ApiTestClassnameResponse200ApplicationJson, () async {

    });
    
  });

});
    });
  });
}

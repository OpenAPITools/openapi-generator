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

/// tests for ValuesApi
void main() {
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
      context: {},
    );
    expect(response.statusCode, 2);
  });
  group(ValuesApiGetSomeValuesResponse200, () {
    test('Unkown mime', () async {

    });
    
    test(ValuesApiGetSomeValuesResponse200ApplicationJson, () async {

    });
    
  });
  group(ValuesApiGetSomeValuesResponse400, () {
    test('Unkown mime', () async {

    });
    
  });

});
    });
  });
}

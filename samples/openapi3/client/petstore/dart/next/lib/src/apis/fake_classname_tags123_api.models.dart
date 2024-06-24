part of 'fake_classname_tags123_api.dart';


abstract class FakeClassnameTags123ApiTestClassnameRequest {
  static const pathTemplate = r'/fake_classname_test';
  static String method = r'PATCH';

  String get contentType;
  final Map<String, String> extraHeaders;
  final Map<String, String> extraCookies;
  final Map<String, Object /* String | List<String> */> extraQueryParameters;

  

  const factory FakeClassnameTags123ApiTestClassnameRequest.unsafe({
    
    Map<String, String> extraHeaders,
    Map<String, Object> extraQueryParameters,
    Map<String, String> extraCookies,
    Stream<Uint8List>? body,
  }) = FakeClassnameTags123ApiTestClassnameRequestUnsafe;

  const FakeClassnameTags123ApiTestClassnameRequest({

    this.extraHeaders = const {},
    this.extraQueryParameters = const {},
    this.extraCookies = const {},
  });

  Future<Uri> getResolvedUri({
    required Uri baseUrl,
    Map<String, dynamic> context = const {},
  }) async {
    var resolvedPath = pathTemplate;
    var methodUri = Uri(path: resolvedPath);

    return baseUrl.replace(
      pathSegments: [
        ...baseUrl.pathSegments,
        ...methodUri.pathSegments,
      ],
      queryParameters: {
        ...baseUrl.queryParameters,
        ...methodUri.queryParameters,
        ...extraQueryParameters,
      },
    );
  }

  Future<Map<String, String>> getResolvedHeaders({
    Map<String, dynamic> context = const {},
  }) async {

    final cookieParts = <String,String>{
      ...extraCookies,
    };

    return CaseInsensitiveMap<String>.from(<String,String>{
      'Content-Type': contentType,
      if (cookieParts.isNotEmpty)
        'Cookie': cookieParts.entries.map((e) => '${e.key}=${e.value}').join('; '),
      ...extraHeaders,
    });
  }


  Stream<List<int>> getResolvedBody({
    required MediaType resolvedMediaType,
    Map<String, dynamic> context = const {},
  });

  Future<HttpRequestBase> createHttpRequest({
    required Uri baseUrl,
    Map<String, dynamic> context = const {},
  }) async {
    final futures = [
      getResolvedUri(
        context: context,
        baseUrl: baseUrl,
      ),
      getResolvedHeaders(context: context),
    ];
    final futureResults = await Future.wait(futures);
    final headers = futureResults[1] as Map<String, String>;
    final contentType = headers['Content-Type']!;
    final parsedContentType = MediaType.parse(contentType).fillDefaults();
    return HttpRequestBase.stream(
      url: futureResults[0] as Uri,
      headers: headers,
      method: method,
      bodyBytesStream: getResolvedBody(context: context, resolvedMediaType: parsedContentType),
      context: context,
    );
  }
}

/// A version of [FakeClassnameTags123ApiTestClassnameRequest], where you can send arbitrary bytes in the body.
class FakeClassnameTags123ApiTestClassnameRequestUnsafe extends FakeClassnameTags123ApiTestClassnameRequest {
  final Stream<Uint8List>? body;

  @override
  final String contentType;

  const FakeClassnameTags123ApiTestClassnameRequestUnsafe({
    this.body,
    this.contentType = 'application/octet-stream',
    
    super.extraHeaders,
    super.extraQueryParameters,
    super.extraCookies,
  });

  Stream<List<int>> getResolvedBody({
    required MediaType resolvedMediaType,
    Map<String, dynamic> context = const {},
  }) async* {
    final body = this.body;
    if (body == null) {
      return;
    }
    yield* body;
  }
}


class FakeClassnameTags123ApiTestClassnameRequestApplicationJson extends FakeClassnameTags123ApiTestClassnameRequest {
  static const specMediaType = r'application/json';

  @override
  String get contentType => specMediaType;

  final 
            Client
 data;

  const FakeClassnameTags123ApiTestClassnameRequestApplicationJson({
    required this.data,
    
    super.extraHeaders,
    super.extraQueryParameters,
    super.extraCookies,
  });

  @override
  Stream<List<int>> getResolvedBody({
    required MediaType resolvedMediaType,
    Map<String, dynamic> context = const {},
  }) async* {
    //TODO: serialize model, then encode it according to media type.
    final v = data;
    var serialized = v.serialize();
    // serialized is guaranteed to be a dart primitive (String, int, List, Map, Uint8List, XFile, XMLElement, etc...)
    final encoded = json.encode(serialized);
    //final bytes = ;
  }
}


class FakeClassnameTags123ApiTestClassnameResponse {
}


// ignore_for_file: unnecessary_type_check

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
    Stream<Uint8List>? bodyBytesStream,
  }) = FakeClassnameTags123ApiTestClassnameRequestUnsafe;

  
  const factory FakeClassnameTags123ApiTestClassnameRequest.applicationJson({
    required 
            Client
 data,
    UnknownMediaTypeHandler? handleUnkownMediaType,
    
    
    Map<String, String> extraHeaders,
    Map<String, Object> extraQueryParameters,
    Map<String, String> extraCookies,
  }) = FakeClassnameTags123ApiTestClassnameRequestApplicationJson;
  

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

    var headers = CaseInsensitiveMap<String>.from(<String,String>{
      'Content-Type': this.contentType,
      if (cookieParts.isNotEmpty)
        'Cookie': cookieParts.entries.map((e) => '${e.key}=${e.value}').join('; '),
      ...extraHeaders,
    });

    var contentType = headers['content-type'];
    if (contentType != null) {
      var parsedContentType = MediaType.parse(contentType).fillDefaults();
      if (parsedContentType.type == 'multipart' && parsedContentType.parameters['boundary'] == null) {
        parsedContentType = parsedContentType.change(
          parameters: {
            ...parsedContentType.parameters,
            'boundary': MultiPartBodySerializer.getRandomBoundaryString(Random()),
          }
        );
      }
      headers['content-type'] = parsedContentType.toString();
    }
    return headers;
  }


  Stream<List<int>> getResolvedBody({
    required Map<String, String> headers,
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
    return HttpRequestBase.stream(
      url: futureResults[0] as Uri,
      headers: headers,
      method: method,
      bodyBytesStream: getResolvedBody(context: context, headers: headers),
      context: context,
    );
  }
}

/// A version of [FakeClassnameTags123ApiTestClassnameRequest], where you can send arbitrary bytes in the body.
class FakeClassnameTags123ApiTestClassnameRequestUnsafe extends FakeClassnameTags123ApiTestClassnameRequest {
  final Stream<Uint8List>? bodyBytesStream;

  @override
  final String contentType;

  const FakeClassnameTags123ApiTestClassnameRequestUnsafe({
    this.bodyBytesStream,
    this.contentType = 'application/octet-stream',
    
    super.extraHeaders,
    super.extraQueryParameters,
    super.extraCookies,
  });

  Stream<List<int>> getResolvedBody({
    required Map<String, String> headers,
    Map<String, dynamic> context = const {},
  }) async* {
    final body = this.bodyBytesStream;
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

  /// Pass this to handle serialization and encoding of unkown media types yourself.
  final UnknownMediaTypeHandler? handleUnkownMediaType;


  


  const FakeClassnameTags123ApiTestClassnameRequestApplicationJson({
    required this.data,
    this.handleUnkownMediaType,
    
    
    super.extraHeaders,
    super.extraQueryParameters,
    super.extraCookies,
  });

  @override
  Stream<List<int>> getResolvedBody({
    required Map<String, String> headers,
    Map<String, dynamic> context = const {},
  }) async* {
    //TODO: serialize model, then encode it according to media type.
    final contentType = headers['Content-Type']!;
    final resolvedMediaType = MediaType.parse(contentType);

    final v = data;
    var serialized = v.serialize();
    final encoding = OASNetworkingUtils.getEncodingOrDefault(resolvedMediaType);
    Stream<List<int>> _stringResult(String src) {
      return Stream.value(encoding.encode(src));
    }
    final encodingRules = <String, PropertyEncodingRule>{
      
    };

    // Since the user can override mime type at runtime, we need to check the
    // mime type and serialize the model accordingly.
    switch (resolvedMediaType) {
      case MediaType(type: 'application', subtype: 'json'):
        yield* _stringResult(json.encode(serialized));
      default:
        final handleUnkownMediaType = this.handleUnkownMediaType;
        if (handleUnkownMediaType != null) {
          yield* handleUnkownMediaType(resolvedMediaType, serialized, encoding, encodingRules);
          return;
        }
        yield* _stringResult(serialized.toString());
    }
  }
}


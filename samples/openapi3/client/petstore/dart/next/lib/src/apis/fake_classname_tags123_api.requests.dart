// ignore_for_file: unnecessary_type_check

part of 'fake_classname_tags123_api.dart';








abstract class FakeClassnameTags123ApiTestClassnameRequest {
  static const pathTemplate = r'/fake_classname_test';
  static String method = r'PATCH';

  String get contentType;
  final Map<String, String> extraHeaders;
  final Map<String, String> extraCookies;
  final Map<String, Object /* String | List<String> */> extraQueryParameters;
  final WireSerializationOptions wireSerializationOptions;
  

  const factory FakeClassnameTags123ApiTestClassnameRequest.unsafe({
    
    Map<String, String> extraHeaders,
    Map<String, Object> extraQueryParameters,
    Map<String, String> extraCookies,
    WireSerializationOptions wireSerializationOptions,
    Stream<Uint8List>? bodyBytesStream,
  }) = FakeClassnameTags123ApiTestClassnameRequestUnsafe;

  
  const factory FakeClassnameTags123ApiTestClassnameRequest.applicationJson({
    required 
            Client
 data,
    
    WireSerializationOptions wireSerializationOptions,
    Map<String, String> extraHeaders,
    Map<String, Object> extraQueryParameters,
    Map<String, String> extraCookies,
  }) = FakeClassnameTags123ApiTestClassnameRequestApplicationJson;
  

  const FakeClassnameTags123ApiTestClassnameRequest({
    
    this.wireSerializationOptions = const WireSerializationOptions(),
    this.extraHeaders = const {},
    this.extraQueryParameters = const {},
    this.extraCookies = const {},
  });

  Future<Uri> getResolvedUri({
    required Uri baseUrl,
    Map<String, dynamic> userContext = const {},
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
    Map<String, dynamic> userContext = const {},
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
    Map<String, dynamic> userContext = const {},
  });

  Future<HttpRequestBase> createHttpRequest({
    required Uri baseUrl,
    Map<String, dynamic> userContext = const {},
  }) async {
    final futures = [
      getResolvedUri(
        userContext: userContext,
        baseUrl: baseUrl,
      ),
      getResolvedHeaders(userContext: userContext),
    ];
    final futureResults = await Future.wait(futures);
    final headers = futureResults[1] as Map<String, String>;
    return HttpRequestBase.stream(
      url: futureResults[0] as Uri,
      headers: headers,
      method: method,
      bodyBytesStream: getResolvedBody(userContext: userContext, headers: headers),
      context: userContext,
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
    super.wireSerializationOptions,
  });

  Stream<List<int>> getResolvedBody({
    required Map<String, String> headers,
    Map<String, dynamic> userContext = const {},
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
  static const dataReflection = XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                Client.$reflection
        
,
)
;


  const FakeClassnameTags123ApiTestClassnameRequestApplicationJson({
    required this.data,
    
    super.extraHeaders,
    super.extraQueryParameters,
    super.extraCookies,
    super.wireSerializationOptions,
  });

  Map<String, PropertyEncodingRule> get encodingRules => <String, PropertyEncodingRule>{
    
  };

  @override
  Stream<List<int>> getResolvedBody({
    required Map<String, String> headers,
    Map<String, dynamic> userContext = const {},
  }) {
    final contentType = headers['Content-Type']!;
    final resolvedMediaType = MediaType.parse(contentType);
    final wireSerializationOptions = this.wireSerializationOptions.withEncodingRules({...encodingRules, ...this.wireSerializationOptions.encodingRules});
    final context = wireSerializationOptions.createSerializationContext(resolvedMediaType);
    final v = data;
    var serialized = dataReflection.serialize(v, context);
    return wireSerializationOptions.getBodyFromSerialized(
      headers: headers,
      serialized: serialized,
      resolvedMediaType: resolvedMediaType,
    );
  }
}


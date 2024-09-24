// ignore_for_file: unnecessary_type_check

part of 'another_fake_api.dart';








abstract class AnotherFakeApi$123testSpecialTagsRequest {
  static const pathTemplate = r'/another-fake/dummy';
  static String method = r'PATCH';

  String get contentType;
  final Map<String, String> extraHeaders;
  final Map<String, String> extraCookies;
  final Map<String, Object /* String | List<String> */> extraQueryParameters;
  final WireSerializationOptions wireSerializationOptions;
  

  const factory AnotherFakeApi$123testSpecialTagsRequest.unsafe({
    
    Map<String, String> extraHeaders,
    Map<String, Object> extraQueryParameters,
    Map<String, String> extraCookies,
    WireSerializationOptions wireSerializationOptions,
    Stream<Uint8List>? bodyBytesStream,
  }) = AnotherFakeApi$123testSpecialTagsRequestUnsafe;

  
  const factory AnotherFakeApi$123testSpecialTagsRequest.applicationJson({
    required 
            Client
 data,
    
    WireSerializationOptions wireSerializationOptions,
    Map<String, String> extraHeaders,
    Map<String, Object> extraQueryParameters,
    Map<String, String> extraCookies,
  }) = AnotherFakeApi$123testSpecialTagsRequestApplicationJson;
  

  const AnotherFakeApi$123testSpecialTagsRequest({
    
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

/// A version of [AnotherFakeApi$123testSpecialTagsRequest], where you can send arbitrary bytes in the body.
class AnotherFakeApi$123testSpecialTagsRequestUnsafe extends AnotherFakeApi$123testSpecialTagsRequest {
  final Stream<Uint8List>? bodyBytesStream;

  @override
  final String contentType;

  const AnotherFakeApi$123testSpecialTagsRequestUnsafe({
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






class AnotherFakeApi$123testSpecialTagsRequestApplicationJson extends AnotherFakeApi$123testSpecialTagsRequest {
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


  const AnotherFakeApi$123testSpecialTagsRequestApplicationJson({
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










 class AnotherFakeApiGetParameterArrayNumberRequest {
  static const pathTemplate = r'/fake/parameter-array-number';
  static String method = r'GET';

  final Map<String, String> extraHeaders;
  final Map<String, String> extraCookies;
  final Map<String, Object /* String | List<String> */> extraQueryParameters;
  final WireSerializationOptions wireSerializationOptions;
  
  /// array integer
  /// spec name: array
  final 
    List<
        
            int
>
 array;
  


  const AnotherFakeApiGetParameterArrayNumberRequest({
    
     this.array    =
        const
        [1]
        
,
    
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
      if (cookieParts.isNotEmpty)
        'Cookie': cookieParts.entries.map((e) => '${e.key}=${e.value}').join('; '),
      
  r'array': OpenApiParameterSerializationHeader(parameterName: r'array',explode: false).serialize(array),
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
      bodyBytesStream: Stream.empty(),
      context: userContext,
    );
  }
}










 class AnotherFakeApiGetParameterStringNumberRequest {
  static const pathTemplate = r'/fake/parameter-string-number';
  static String method = r'GET';

  final Map<String, String> extraHeaders;
  final Map<String, String> extraCookies;
  final Map<String, Object /* String | List<String> */> extraQueryParameters;
  final WireSerializationOptions wireSerializationOptions;
  
  /// string number
  /// spec name: string_number
  final 
            double
 stringNumber;
  


  const AnotherFakeApiGetParameterStringNumberRequest({
    
    required this.stringNumber    ,
    
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
      if (cookieParts.isNotEmpty)
        'Cookie': cookieParts.entries.map((e) => '${e.key}=${e.value}').join('; '),
      
  r'string_number': OpenApiParameterSerializationHeader(parameterName: r'string_number',explode: false).serialize(stringNumber),
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
      bodyBytesStream: Stream.empty(),
      context: userContext,
    );
  }
}














 class AnotherFakeApiNullRequestBodyRequest {
  static const pathTemplate = r'/fake/null-request-body';
  static String method = r'GET';

  final Map<String, String> extraHeaders;
  final Map<String, String> extraCookies;
  final Map<String, Object /* String | List<String> */> extraQueryParameters;
  final WireSerializationOptions wireSerializationOptions;
  
  /// 
  /// spec name: Accept-Language
  final UndefinedWrapper<
            String
> acceptLanguage;
  
  


  const AnotherFakeApiNullRequestBodyRequest({
    
     this.acceptLanguage= const UndefinedWrapper
        .undefined()
,
    
    
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
      if (cookieParts.isNotEmpty)
        'Cookie': cookieParts.entries.map((e) => '${e.key}=${e.value}').join('; '),
      if (acceptLanguage.isDefined)
  r'Accept-Language': OpenApiParameterSerializationHeader(parameterName: r'Accept-Language',explode: false).serialize(acceptLanguage.valueRequired),
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
      bodyBytesStream: Stream.empty(),
      context: userContext,
    );
  }
}




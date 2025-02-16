// ignore_for_file: unnecessary_type_check

part of 'default_api.dart';




 class DefaultApiFakeAnyOfWIthSameErasureGetRequest {
  static const pathTemplate = r'/fake/anyOfWIthSameErasure';
  static String method = r'GET';

  final Map<String, String> extraHeaders;
  final Map<String, String> extraCookies;
  final Map<String, Object /* String | List<String> */> extraQueryParameters;
  final WireSerializationOptions wireSerializationOptions;


  const DefaultApiFakeAnyOfWIthSameErasureGetRequest({
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






 class DefaultApiFakeOneOfWIthSameErasureGetRequest {
  static const pathTemplate = r'/fake/oneOfWIthSameErasure';
  static String method = r'GET';

  final Map<String, String> extraHeaders;
  final Map<String, String> extraCookies;
  final Map<String, Object /* String | List<String> */> extraQueryParameters;
  final WireSerializationOptions wireSerializationOptions;


  const DefaultApiFakeOneOfWIthSameErasureGetRequest({
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






 class DefaultApiFooGetRequest {
  static const pathTemplate = r'/foo';
  static String method = r'GET';

  final Map<String, String> extraHeaders;
  final Map<String, String> extraCookies;
  final Map<String, Object /* String | List<String> */> extraQueryParameters;
  final WireSerializationOptions wireSerializationOptions;


  const DefaultApiFooGetRequest({
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










abstract class DefaultApiPetsMulticontentTestPostRequest {
  static const pathTemplate = r'/pets/multicontent-test';
  static String method = r'POST';

  String get contentType;
  final Map<String, String> extraHeaders;
  final Map<String, String> extraCookies;
  final Map<String, Object /* String | List<String> */> extraQueryParameters;
  final WireSerializationOptions wireSerializationOptions;
  

  const factory DefaultApiPetsMulticontentTestPostRequest.unsafe({
    
    Map<String, String> extraHeaders,
    Map<String, Object> extraQueryParameters,
    Map<String, String> extraCookies,
    WireSerializationOptions wireSerializationOptions,
    Stream<Uint8List>? bodyBytesStream,
  }) = DefaultApiPetsMulticontentTestPostRequestUnsafe;

  
  const factory DefaultApiPetsMulticontentTestPostRequest.applicationJson({
    required 
            Pet
 data,
    
    WireSerializationOptions wireSerializationOptions,
    Map<String, String> extraHeaders,
    Map<String, Object> extraQueryParameters,
    Map<String, String> extraCookies,
  }) = DefaultApiPetsMulticontentTestPostRequestApplicationJson;
  
  const factory DefaultApiPetsMulticontentTestPostRequest.applicationXml({
    required 
            NewPet
 data,
    
    WireSerializationOptions wireSerializationOptions,
    Map<String, String> extraHeaders,
    Map<String, Object> extraQueryParameters,
    Map<String, String> extraCookies,
  }) = DefaultApiPetsMulticontentTestPostRequestApplicationXml;
  
  const factory DefaultApiPetsMulticontentTestPostRequest.applicationXWwwFormUrlencoded({
    required 
            Triangle
 data,
    
    WireSerializationOptions wireSerializationOptions,
    Map<String, String> extraHeaders,
    Map<String, Object> extraQueryParameters,
    Map<String, String> extraCookies,
  }) = DefaultApiPetsMulticontentTestPostRequestApplicationXWwwFormUrlencoded;
  
  const factory DefaultApiPetsMulticontentTestPostRequest.textPlain({
    required 
            int
 data,
    
    WireSerializationOptions wireSerializationOptions,
    Map<String, String> extraHeaders,
    Map<String, Object> extraQueryParameters,
    Map<String, String> extraCookies,
  }) = DefaultApiPetsMulticontentTestPostRequestTextPlain;
  
  const factory DefaultApiPetsMulticontentTestPostRequest.anyAny({
    required Object
? data,
    
    WireSerializationOptions wireSerializationOptions,
    Map<String, String> extraHeaders,
    Map<String, Object> extraQueryParameters,
    Map<String, String> extraCookies,
  }) = DefaultApiPetsMulticontentTestPostRequestAnyAny;
  
  const factory DefaultApiPetsMulticontentTestPostRequest.textAny({
    required 
            String
 data,
    
    WireSerializationOptions wireSerializationOptions,
    Map<String, String> extraHeaders,
    Map<String, Object> extraQueryParameters,
    Map<String, String> extraCookies,
  }) = DefaultApiPetsMulticontentTestPostRequestTextAny;
  
  const factory DefaultApiPetsMulticontentTestPostRequest.multipartFormData({
    required 
            PetsMulticontentTestPostRequest
 data,
    UndefinedWrapper<
            String
> profileImagesHelloHeader,
    
    WireSerializationOptions wireSerializationOptions,
    Map<String, String> extraHeaders,
    Map<String, Object> extraQueryParameters,
    Map<String, String> extraCookies,
  }) = DefaultApiPetsMulticontentTestPostRequestMultipartFormData;
  

  const DefaultApiPetsMulticontentTestPostRequest({
    
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

/// A version of [DefaultApiPetsMulticontentTestPostRequest], where you can send arbitrary bytes in the body.
class DefaultApiPetsMulticontentTestPostRequestUnsafe extends DefaultApiPetsMulticontentTestPostRequest {
  final Stream<Uint8List>? bodyBytesStream;

  @override
  final String contentType;

  const DefaultApiPetsMulticontentTestPostRequestUnsafe({
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






class DefaultApiPetsMulticontentTestPostRequestApplicationJson extends DefaultApiPetsMulticontentTestPostRequest {
  static const specMediaType = r'application/json';

  @override
  String get contentType => specMediaType;

  final 
            Pet
 data;
  static const dataReflection = XmlReflectionWrapper(
    xml: XmlReflection(
    xmlName: r'Pet',
),
    
            
        
        
            
                Pet.$reflection
        
,
)
;


  const DefaultApiPetsMulticontentTestPostRequestApplicationJson({
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


class DefaultApiPetsMulticontentTestPostRequestApplicationXml extends DefaultApiPetsMulticontentTestPostRequest {
  static const specMediaType = r'application/xml';

  @override
  String get contentType => specMediaType;

  final 
            NewPet
 data;
  static const dataReflection = XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                NewPet.$reflection
        
,
)
;


  const DefaultApiPetsMulticontentTestPostRequestApplicationXml({
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


class DefaultApiPetsMulticontentTestPostRequestApplicationXWwwFormUrlencoded extends DefaultApiPetsMulticontentTestPostRequest {
  static const specMediaType = r'application/x-www-form-urlencoded';

  @override
  String get contentType => specMediaType;

  final 
            Triangle
 data;
  static const dataReflection = XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                Triangle.$reflection
        
,
)
;


  const DefaultApiPetsMulticontentTestPostRequestApplicationXWwwFormUrlencoded({
    required this.data,
    
    super.extraHeaders,
    super.extraQueryParameters,
    super.extraCookies,
    super.wireSerializationOptions,
  });

  Map<String, PropertyEncodingRule> get encodingRules => <String, PropertyEncodingRule>{
    
    r'name': PropertyEncodingRule(
      style: r'form',
      explode: true,
      
      
      
    ),
    
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


class DefaultApiPetsMulticontentTestPostRequestTextPlain extends DefaultApiPetsMulticontentTestPostRequest {
  static const specMediaType = r'text/plain';

  @override
  String get contentType => specMediaType;

  final 
            int
 data;
  static const dataReflection = XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forint
        
,
)
;


  const DefaultApiPetsMulticontentTestPostRequestTextPlain({
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


class DefaultApiPetsMulticontentTestPostRequestAnyAny extends DefaultApiPetsMulticontentTestPostRequest {
  static const specMediaType = r'*/*';

  @override
  String get contentType => specMediaType;

  final Object
? data;
  static const dataReflection = XmlReflectionWrapper(
    xml: XmlReflection(
),
    NullableReflection(ObjectReflection()
),
)
;


  const DefaultApiPetsMulticontentTestPostRequestAnyAny({
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


class DefaultApiPetsMulticontentTestPostRequestTextAny extends DefaultApiPetsMulticontentTestPostRequest {
  static const specMediaType = r'text/*';

  @override
  String get contentType => specMediaType;

  final 
            String
 data;
  static const dataReflection = XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forString
        
,
)
;


  const DefaultApiPetsMulticontentTestPostRequestTextAny({
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







class DefaultApiPetsMulticontentTestPostRequestMultipartFormData extends DefaultApiPetsMulticontentTestPostRequest {
  static const specMediaType = r'multipart/form-data';

  @override
  String get contentType => specMediaType;

  final 
            PetsMulticontentTestPostRequest
 data;
  static const dataReflection = XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PetsMulticontentTestPostRequest.$reflection
        
,
)
;

  final UndefinedWrapper<
            String
> profileImagesHelloHeader;

  const DefaultApiPetsMulticontentTestPostRequestMultipartFormData({
    required this.data,
     this.profileImagesHelloHeader= const UndefinedWrapper
        .undefined()
,
    
    super.extraHeaders,
    super.extraQueryParameters,
    super.extraCookies,
    super.wireSerializationOptions,
  });

  Map<String, PropertyEncodingRule> get encodingRules => <String, PropertyEncodingRule>{
    
    r'address': PropertyEncodingRule(
      style: r'form',
      
      
      contentType: MediaType.parse(r'application/json'),
      
    ),
    r'profileImages': PropertyEncodingRule(
      style: r'form',
      
      
      
      
      headers: <String, String>{
      if (profileImagesHelloHeader.isDefined)
  r'hello': OpenApiParameterSerializationHeader(parameterName: r'hello',explode: false).serialize(profileImagesHelloHeader.valueRequired),
      },
      
    ),
    
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


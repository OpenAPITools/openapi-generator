// ignore_for_file: unnecessary_type_check

part of 'pet_api.dart';








abstract class PetApiAddPetRequest {
  static const pathTemplate = r'/pet';
  static String method = r'POST';

  String get contentType;
  final Map<String, String> extraHeaders;
  final Map<String, String> extraCookies;
  final Map<String, Object /* String | List<String> */> extraQueryParameters;
  final WireSerializationOptions wireSerializationOptions;
  

  const factory PetApiAddPetRequest.unsafe({
    
    Map<String, String> extraHeaders,
    Map<String, Object> extraQueryParameters,
    Map<String, String> extraCookies,
    WireSerializationOptions wireSerializationOptions,
    Stream<Uint8List>? bodyBytesStream,
  }) = PetApiAddPetRequestUnsafe;

  
  const factory PetApiAddPetRequest.applicationJson({
    required 
            Pet
 data,
    
    WireSerializationOptions wireSerializationOptions,
    Map<String, String> extraHeaders,
    Map<String, Object> extraQueryParameters,
    Map<String, String> extraCookies,
  }) = PetApiAddPetRequestApplicationJson;
  
  const factory PetApiAddPetRequest.applicationXml({
    required 
            Pet
 data,
    
    WireSerializationOptions wireSerializationOptions,
    Map<String, String> extraHeaders,
    Map<String, Object> extraQueryParameters,
    Map<String, String> extraCookies,
  }) = PetApiAddPetRequestApplicationXml;
  

  const PetApiAddPetRequest({
    
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

/// A version of [PetApiAddPetRequest], where you can send arbitrary bytes in the body.
class PetApiAddPetRequestUnsafe extends PetApiAddPetRequest {
  final Stream<Uint8List>? bodyBytesStream;

  @override
  final String contentType;

  const PetApiAddPetRequestUnsafe({
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






class PetApiAddPetRequestApplicationJson extends PetApiAddPetRequest {
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


  const PetApiAddPetRequestApplicationJson({
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


class PetApiAddPetRequestApplicationXml extends PetApiAddPetRequest {
  static const specMediaType = r'application/xml';

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


  const PetApiAddPetRequestApplicationXml({
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












 class PetApiDeletePetRequest {
  static const pathTemplate = r'/pet/{petId}';
  static String method = r'DELETE';

  final Map<String, String> extraHeaders;
  final Map<String, String> extraCookies;
  final Map<String, Object /* String | List<String> */> extraQueryParameters;
  final WireSerializationOptions wireSerializationOptions;
  
  /// Pet id to delete
  /// spec name: petId
  final 
            int
 petId;
  
  
  /// 
  /// spec name: api_key
  final UndefinedWrapper<
            String
> apiKey;
  


  const PetApiDeletePetRequest({
    
    required this.petId    ,
    
    
     this.apiKey= const UndefinedWrapper
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
    resolvedPath = OpenApiParameterSerializationPath.fromStyle(r'simple', explode: false, parameterName: r'petId',).expand(resolvedPath, petId);
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
      if (apiKey.isDefined)
  r'api_key': OpenApiParameterSerializationHeader(parameterName: r'api_key',explode: false).serialize(apiKey.valueRequired),
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






extension type const StatusEnum._(String value) implements String {
      const StatusEnum.available() : this._(r'available');
      const StatusEnum.pending() : this._(r'pending');
      const StatusEnum.sold() : this._(r'sold');

  /// Creates a [StatusEnum] enum from a value and safely checking if it exists.
  factory StatusEnum.$safe(String value) {
    final res = values.where((element) => element.value == value).firstOrNull;
    if (res == null) {
      throw 'Invalid enum value $value';
    }
    return res;
  }

  static const $reflection = EnumReflection<StatusEnum, String>(
    PrimitiveReflection.forString,
    members: [
      
        EnumMemberReflection(dartName: r'available', oasValue: r'available', value: StatusEnum.available()),
      
        EnumMemberReflection(dartName: r'pending', oasValue: r'pending', value: StatusEnum.pending()),
      
        EnumMemberReflection(dartName: r'sold', oasValue: r'sold', value: StatusEnum.sold()),
      
    ],
  );

  factory StatusEnum.deserialize(Object? value, [SerializationContext context = const SerializationContext.json()]) {
    return $reflection.deserializeFunction(value, context);
  }

  static bool canDeserialize(Object? value, [SerializationContext context = const SerializationContext.json()]) {
    return $reflection.canDeserializeFunction(value,context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json()]) {
    return $reflection.serializeFunction(this, context);
  }

  /// Creates a [StatusEnum] enum from a value without checking if it exists.
  const StatusEnum.$unsafe(String value) : this._(value);

  /// All possible values of the enum.
  static const List<StatusEnum> values = [
    StatusEnum.available(),
    StatusEnum.pending(),
    StatusEnum.sold(),
    
  ];
}





 class PetApiFindPetsByStatusRequest {
  static const pathTemplate = r'/pet/findByStatus';
  static String method = r'GET';

  final Map<String, String> extraHeaders;
  final Map<String, String> extraCookies;
  final Map<String, Object /* String | List<String> */> extraQueryParameters;
  final WireSerializationOptions wireSerializationOptions;
  
  /// Status values that need to be considered for filter
  /// spec name: status
  final 
    List<
        
            StatusEnum
>
 status;
  


  const PetApiFindPetsByStatusRequest({
    
    required this.status    ,
    
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
      methodUri = OpenApiParameterSerializationQuery.fromStyle(r'form', explode: false, parameterName: r'status', allowEmptyValue: false,).expandUri(methodUri, status);

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












@Deprecated('This operation has been deprecated')
 class PetApiFindPetsByTagsRequest {
  static const pathTemplate = r'/pet/findByTags';
  static String method = r'GET';

  final Map<String, String> extraHeaders;
  final Map<String, String> extraCookies;
  final Map<String, Object /* String | List<String> */> extraQueryParameters;
  final WireSerializationOptions wireSerializationOptions;
  
  /// Tags to filter by
  /// spec name: tags
  final 
    List<
        
            String
>
 tags;
  


  const PetApiFindPetsByTagsRequest({
    
    required this.tags    ,
    
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
      methodUri = OpenApiParameterSerializationQuery.fromStyle(r'form', explode: false, parameterName: r'tags', allowEmptyValue: false,).expandUri(methodUri, tags);

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










 class PetApiGetPetByIdRequest {
  static const pathTemplate = r'/pet/{petId}';
  static String method = r'GET';

  final Map<String, String> extraHeaders;
  final Map<String, String> extraCookies;
  final Map<String, Object /* String | List<String> */> extraQueryParameters;
  final WireSerializationOptions wireSerializationOptions;
  
  /// ID of pet to return
  /// spec name: petId
  final 
            int
 petId;
  


  const PetApiGetPetByIdRequest({
    
    required this.petId    ,
    
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
    resolvedPath = OpenApiParameterSerializationPath.fromStyle(r'simple', explode: false, parameterName: r'petId',).expand(resolvedPath, petId);
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










abstract class PetApiUpdatePetRequest {
  static const pathTemplate = r'/pet';
  static String method = r'PUT';

  String get contentType;
  final Map<String, String> extraHeaders;
  final Map<String, String> extraCookies;
  final Map<String, Object /* String | List<String> */> extraQueryParameters;
  final WireSerializationOptions wireSerializationOptions;
  

  const factory PetApiUpdatePetRequest.unsafe({
    
    Map<String, String> extraHeaders,
    Map<String, Object> extraQueryParameters,
    Map<String, String> extraCookies,
    WireSerializationOptions wireSerializationOptions,
    Stream<Uint8List>? bodyBytesStream,
  }) = PetApiUpdatePetRequestUnsafe;

  
  const factory PetApiUpdatePetRequest.applicationJson({
    required 
            Pet
 data,
    
    WireSerializationOptions wireSerializationOptions,
    Map<String, String> extraHeaders,
    Map<String, Object> extraQueryParameters,
    Map<String, String> extraCookies,
  }) = PetApiUpdatePetRequestApplicationJson;
  
  const factory PetApiUpdatePetRequest.applicationXml({
    required 
            Pet
 data,
    
    WireSerializationOptions wireSerializationOptions,
    Map<String, String> extraHeaders,
    Map<String, Object> extraQueryParameters,
    Map<String, String> extraCookies,
  }) = PetApiUpdatePetRequestApplicationXml;
  

  const PetApiUpdatePetRequest({
    
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

/// A version of [PetApiUpdatePetRequest], where you can send arbitrary bytes in the body.
class PetApiUpdatePetRequestUnsafe extends PetApiUpdatePetRequest {
  final Stream<Uint8List>? bodyBytesStream;

  @override
  final String contentType;

  const PetApiUpdatePetRequestUnsafe({
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






class PetApiUpdatePetRequestApplicationJson extends PetApiUpdatePetRequest {
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


  const PetApiUpdatePetRequestApplicationJson({
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


class PetApiUpdatePetRequestApplicationXml extends PetApiUpdatePetRequest {
  static const specMediaType = r'application/xml';

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


  const PetApiUpdatePetRequestApplicationXml({
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
















abstract class PetApiUpdatePetWithFormRequest {
  static const pathTemplate = r'/pet/{petId}';
  static String method = r'POST';

  String get contentType;
  final Map<String, String> extraHeaders;
  final Map<String, String> extraCookies;
  final Map<String, Object /* String | List<String> */> extraQueryParameters;
  final WireSerializationOptions wireSerializationOptions;
  
  /// ID of pet that needs to be updated
  /// spec name: petId
  final 
            int
 petId;
  
  
  

  const factory PetApiUpdatePetWithFormRequest.unsafe({
    
    required 
            int
 petId,
    
    Map<String, String> extraHeaders,
    Map<String, Object> extraQueryParameters,
    Map<String, String> extraCookies,
    WireSerializationOptions wireSerializationOptions,
    Stream<Uint8List>? bodyBytesStream,
  }) = PetApiUpdatePetWithFormRequestUnsafe;

  

  const PetApiUpdatePetWithFormRequest({
    
    required this.petId    ,
    
    
    
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
    resolvedPath = OpenApiParameterSerializationPath.fromStyle(r'simple', explode: false, parameterName: r'petId',).expand(resolvedPath, petId);
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

/// A version of [PetApiUpdatePetWithFormRequest], where you can send arbitrary bytes in the body.
class PetApiUpdatePetWithFormRequestUnsafe extends PetApiUpdatePetWithFormRequest {
  final Stream<Uint8List>? bodyBytesStream;

  @override
  final String contentType;

  const PetApiUpdatePetWithFormRequestUnsafe({
    this.bodyBytesStream,
    this.contentType = 'application/octet-stream',
    
    required super.petId,
    
    
    
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




















abstract class PetApiUploadFileRequest {
  static const pathTemplate = r'/pet/{petId}/uploadImage';
  static String method = r'POST';

  String get contentType;
  final Map<String, String> extraHeaders;
  final Map<String, String> extraCookies;
  final Map<String, Object /* String | List<String> */> extraQueryParameters;
  final WireSerializationOptions wireSerializationOptions;
  
  /// ID of pet to update
  /// spec name: petId
  final 
            int
 petId;
  
  
  

  const factory PetApiUploadFileRequest.unsafe({
    
    required 
            int
 petId,
    
    Map<String, String> extraHeaders,
    Map<String, Object> extraQueryParameters,
    Map<String, String> extraCookies,
    WireSerializationOptions wireSerializationOptions,
    Stream<Uint8List>? bodyBytesStream,
  }) = PetApiUploadFileRequestUnsafe;

  

  const PetApiUploadFileRequest({
    
    required this.petId    ,
    
    
    
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
    resolvedPath = OpenApiParameterSerializationPath.fromStyle(r'simple', explode: false, parameterName: r'petId',).expand(resolvedPath, petId);
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

/// A version of [PetApiUploadFileRequest], where you can send arbitrary bytes in the body.
class PetApiUploadFileRequestUnsafe extends PetApiUploadFileRequest {
  final Stream<Uint8List>? bodyBytesStream;

  @override
  final String contentType;

  const PetApiUploadFileRequestUnsafe({
    this.bodyBytesStream,
    this.contentType = 'application/octet-stream',
    
    required super.petId,
    
    
    
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




















abstract class PetApiUploadFileWithRequiredFileRequest {
  static const pathTemplate = r'/fake/{petId}/uploadImageWithRequiredFile';
  static String method = r'POST';

  String get contentType;
  final Map<String, String> extraHeaders;
  final Map<String, String> extraCookies;
  final Map<String, Object /* String | List<String> */> extraQueryParameters;
  final WireSerializationOptions wireSerializationOptions;
  
  /// ID of pet to update
  /// spec name: petId
  final 
            int
 petId;
  
  
  

  const factory PetApiUploadFileWithRequiredFileRequest.unsafe({
    
    required 
            int
 petId,
    
    Map<String, String> extraHeaders,
    Map<String, Object> extraQueryParameters,
    Map<String, String> extraCookies,
    WireSerializationOptions wireSerializationOptions,
    Stream<Uint8List>? bodyBytesStream,
  }) = PetApiUploadFileWithRequiredFileRequestUnsafe;

  

  const PetApiUploadFileWithRequiredFileRequest({
    
    required this.petId    ,
    
    
    
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
    resolvedPath = OpenApiParameterSerializationPath.fromStyle(r'simple', explode: false, parameterName: r'petId',).expand(resolvedPath, petId);
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

/// A version of [PetApiUploadFileWithRequiredFileRequest], where you can send arbitrary bytes in the body.
class PetApiUploadFileWithRequiredFileRequestUnsafe extends PetApiUploadFileWithRequiredFileRequest {
  final Stream<Uint8List>? bodyBytesStream;

  @override
  final String contentType;

  const PetApiUploadFileWithRequiredFileRequestUnsafe({
    this.bodyBytesStream,
    this.contentType = 'application/octet-stream',
    
    required super.petId,
    
    
    
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






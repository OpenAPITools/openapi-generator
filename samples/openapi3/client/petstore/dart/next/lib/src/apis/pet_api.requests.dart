// ignore_for_file: unnecessary_type_check

part of 'pet_api.dart';





abstract class PetApiAddPetRequest {
  static const pathTemplate = r'/pet';
  static String method = r'POST';

  String get contentType;
  final Map<String, String> extraHeaders;
  final Map<String, String> extraCookies;
  final Map<String, Object /* String | List<String> */> extraQueryParameters;

  

  const factory PetApiAddPetRequest.unsafe({
    
    Map<String, String> extraHeaders,
    Map<String, Object> extraQueryParameters,
    Map<String, String> extraCookies,
    Stream<Uint8List>? bodyBytesStream,
  }) = PetApiAddPetRequestUnsafe;

  
  const factory PetApiAddPetRequest.applicationJson({
    required 
            Pet
 data,
    UnknownMediaTypeHandler? handleUnkownMediaType,
    
    
    Map<String, String> extraHeaders,
    Map<String, Object> extraQueryParameters,
    Map<String, String> extraCookies,
  }) = PetApiAddPetRequestApplicationJson;
  
  const factory PetApiAddPetRequest.applicationXml({
    required 
            Pet
 data,
    UnknownMediaTypeHandler? handleUnkownMediaType,
    
    
    Map<String, String> extraHeaders,
    Map<String, Object> extraQueryParameters,
    Map<String, String> extraCookies,
  }) = PetApiAddPetRequestApplicationXml;
  

  const PetApiAddPetRequest({
    
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




class PetApiAddPetRequestApplicationJson extends PetApiAddPetRequest {
  static const specMediaType = r'application/json';

  @override
  String get contentType => specMediaType;

  final 
            Pet
 data;

  /// Pass this to handle serialization and encoding of unkown media types yourself.
  final UnknownMediaTypeHandler? handleUnkownMediaType;


  


  const PetApiAddPetRequestApplicationJson({
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
class PetApiAddPetRequestApplicationXml extends PetApiAddPetRequest {
  static const specMediaType = r'application/xml';

  @override
  String get contentType => specMediaType;

  final 
            Pet
 data;

  /// Pass this to handle serialization and encoding of unkown media types yourself.
  final UnknownMediaTypeHandler? handleUnkownMediaType;


  


  const PetApiAddPetRequestApplicationXml({
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
      case MediaType(type: 'application', subtype: 'xml'):
        break;
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





 class PetApiDeletePetRequest {
  static const pathTemplate = r'/pet/{petId}';
  static String method = r'DELETE';

  final Map<String, String> extraHeaders;
  final Map<String, String> extraCookies;
  final Map<String, Object /* String | List<String> */> extraQueryParameters;

  
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
    
    this.extraHeaders = const {},
    this.extraQueryParameters = const {},
    this.extraCookies = const {},
  });

  Future<Uri> getResolvedUri({
    required Uri baseUrl,
    Map<String, dynamic> context = const {},
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
    Map<String, dynamic> context = const {},
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
      bodyBytesStream: Stream.empty(),
      context: context,
    );
  }
}






extension type const StatusEnum._(String value) {
  /// Status values that need to be considered for filter
      const StatusEnum.available() : this._(r'available');
  /// Status values that need to be considered for filter
      const StatusEnum.pending() : this._(r'pending');
  /// Status values that need to be considered for filter
      const StatusEnum.sold() : this._(r'sold');

  /// Creates a [StatusEnum] enum from a value and safely checking if it exists.
  factory StatusEnum.$safe(String value) {
    final res = values.where((element) => element.value == value).firstOrNull;
    if (res == null) {
      throw 'Invalid enum value $value';
    }
    return res;
  }

  static bool canDeserialize(Object? value) {
    return value is String && values.where((element) => element.value == value).firstOrNull != null;
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

  
  /// Status values that need to be considered for filter
  /// spec name: status
  final 
    List<
        
            StatusEnum
>
 status;
  


  const PetApiFindPetsByStatusRequest({
    
    required this.status    ,
    
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
    Map<String, dynamic> context = const {},
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
      bodyBytesStream: Stream.empty(),
      context: context,
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

  
  /// Tags to filter by
  /// spec name: tags
  final 
    List<
        
            String
>
 tags;
  


  const PetApiFindPetsByTagsRequest({
    
    required this.tags    ,
    
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
    Map<String, dynamic> context = const {},
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
      bodyBytesStream: Stream.empty(),
      context: context,
    );
  }
}







 class PetApiGetPetByIdRequest {
  static const pathTemplate = r'/pet/{petId}';
  static String method = r'GET';

  final Map<String, String> extraHeaders;
  final Map<String, String> extraCookies;
  final Map<String, Object /* String | List<String> */> extraQueryParameters;

  
  /// ID of pet to return
  /// spec name: petId
  final 
            int
 petId;
  


  const PetApiGetPetByIdRequest({
    
    required this.petId    ,
    
    this.extraHeaders = const {},
    this.extraQueryParameters = const {},
    this.extraCookies = const {},
  });

  Future<Uri> getResolvedUri({
    required Uri baseUrl,
    Map<String, dynamic> context = const {},
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
    Map<String, dynamic> context = const {},
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
      bodyBytesStream: Stream.empty(),
      context: context,
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

  

  const factory PetApiUpdatePetRequest.unsafe({
    
    Map<String, String> extraHeaders,
    Map<String, Object> extraQueryParameters,
    Map<String, String> extraCookies,
    Stream<Uint8List>? bodyBytesStream,
  }) = PetApiUpdatePetRequestUnsafe;

  
  const factory PetApiUpdatePetRequest.applicationJson({
    required 
            Pet
 data,
    UnknownMediaTypeHandler? handleUnkownMediaType,
    
    
    Map<String, String> extraHeaders,
    Map<String, Object> extraQueryParameters,
    Map<String, String> extraCookies,
  }) = PetApiUpdatePetRequestApplicationJson;
  
  const factory PetApiUpdatePetRequest.applicationXml({
    required 
            Pet
 data,
    UnknownMediaTypeHandler? handleUnkownMediaType,
    
    
    Map<String, String> extraHeaders,
    Map<String, Object> extraQueryParameters,
    Map<String, String> extraCookies,
  }) = PetApiUpdatePetRequestApplicationXml;
  

  const PetApiUpdatePetRequest({
    
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




class PetApiUpdatePetRequestApplicationJson extends PetApiUpdatePetRequest {
  static const specMediaType = r'application/json';

  @override
  String get contentType => specMediaType;

  final 
            Pet
 data;

  /// Pass this to handle serialization and encoding of unkown media types yourself.
  final UnknownMediaTypeHandler? handleUnkownMediaType;


  


  const PetApiUpdatePetRequestApplicationJson({
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
class PetApiUpdatePetRequestApplicationXml extends PetApiUpdatePetRequest {
  static const specMediaType = r'application/xml';

  @override
  String get contentType => specMediaType;

  final 
            Pet
 data;

  /// Pass this to handle serialization and encoding of unkown media types yourself.
  final UnknownMediaTypeHandler? handleUnkownMediaType;


  


  const PetApiUpdatePetRequestApplicationXml({
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
      case MediaType(type: 'application', subtype: 'xml'):
        break;
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





abstract class PetApiUpdatePetWithFormRequest {
  static const pathTemplate = r'/pet/{petId}';
  static String method = r'POST';

  String get contentType;
  final Map<String, String> extraHeaders;
  final Map<String, String> extraCookies;
  final Map<String, Object /* String | List<String> */> extraQueryParameters;

  
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
    Stream<Uint8List>? bodyBytesStream,
  }) = PetApiUpdatePetWithFormRequestUnsafe;

  

  const PetApiUpdatePetWithFormRequest({
    
    required this.petId    ,
    
    
    
    this.extraHeaders = const {},
    this.extraQueryParameters = const {},
    this.extraCookies = const {},
  });

  Future<Uri> getResolvedUri({
    required Uri baseUrl,
    Map<String, dynamic> context = const {},
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









abstract class PetApiUploadFileRequest {
  static const pathTemplate = r'/pet/{petId}/uploadImage';
  static String method = r'POST';

  String get contentType;
  final Map<String, String> extraHeaders;
  final Map<String, String> extraCookies;
  final Map<String, Object /* String | List<String> */> extraQueryParameters;

  
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
    Stream<Uint8List>? bodyBytesStream,
  }) = PetApiUploadFileRequestUnsafe;

  

  const PetApiUploadFileRequest({
    
    required this.petId    ,
    
    
    
    this.extraHeaders = const {},
    this.extraQueryParameters = const {},
    this.extraCookies = const {},
  });

  Future<Uri> getResolvedUri({
    required Uri baseUrl,
    Map<String, dynamic> context = const {},
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









abstract class PetApiUploadFileWithRequiredFileRequest {
  static const pathTemplate = r'/fake/{petId}/uploadImageWithRequiredFile';
  static String method = r'POST';

  String get contentType;
  final Map<String, String> extraHeaders;
  final Map<String, String> extraCookies;
  final Map<String, Object /* String | List<String> */> extraQueryParameters;

  
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
    Stream<Uint8List>? bodyBytesStream,
  }) = PetApiUploadFileWithRequiredFileRequestUnsafe;

  

  const PetApiUploadFileWithRequiredFileRequest({
    
    required this.petId    ,
    
    
    
    this.extraHeaders = const {},
    this.extraQueryParameters = const {},
    this.extraCookies = const {},
  });

  Future<Uri> getResolvedUri({
    required Uri baseUrl,
    Map<String, dynamic> context = const {},
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






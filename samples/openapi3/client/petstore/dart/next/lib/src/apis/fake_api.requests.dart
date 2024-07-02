// ignore_for_file: unnecessary_type_check

part of 'fake_api.dart';





 class FakeApiFakeGetFreeFormObjectGetRequest {
  static const pathTemplate = r'/fake/get-free-form-object';
  static String method = r'GET';

  final Map<String, String> extraHeaders;
  final Map<String, String> extraCookies;
  final Map<String, Object /* String | List<String> */> extraQueryParameters;



  const FakeApiFakeGetFreeFormObjectGetRequest({
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







abstract class FakeApiFakeOuterBooleanSerializeRequest {
  static const pathTemplate = r'/fake/outer/boolean';
  static String method = r'POST';

  String get contentType;
  final Map<String, String> extraHeaders;
  final Map<String, String> extraCookies;
  final Map<String, Object /* String | List<String> */> extraQueryParameters;

  

  const factory FakeApiFakeOuterBooleanSerializeRequest.unsafe({
    
    Map<String, String> extraHeaders,
    Map<String, Object> extraQueryParameters,
    Map<String, String> extraCookies,
    Stream<Uint8List>? body,
  }) = FakeApiFakeOuterBooleanSerializeRequestUnsafe;

  const FakeApiFakeOuterBooleanSerializeRequest({
    
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

/// A version of [FakeApiFakeOuterBooleanSerializeRequest], where you can send arbitrary bytes in the body.
class FakeApiFakeOuterBooleanSerializeRequestUnsafe extends FakeApiFakeOuterBooleanSerializeRequest {
  final Stream<Uint8List>? body;

  @override
  final String contentType;

  const FakeApiFakeOuterBooleanSerializeRequestUnsafe({
    this.body,
    this.contentType = 'application/octet-stream',
    
    super.extraHeaders,
    super.extraQueryParameters,
    super.extraCookies,
  });

  Stream<List<int>> getResolvedBody({
    required Map<String, String> headers,
    Map<String, dynamic> context = const {},
  }) async* {
    final body = this.body;
    if (body == null) {
      return;
    }
    yield* body;
  }
}




class FakeApiFakeOuterBooleanSerializeRequestApplicationJson extends FakeApiFakeOuterBooleanSerializeRequest {
  static const specMediaType = r'application/json';

  @override
  String get contentType => specMediaType;

  final 
            bool
 data;

  /// Pass this to handle serialization and encoding of unkown media types yourself.
  final UnknownMediaTypeHandler? handleUnkownMediaType;


  


  const FakeApiFakeOuterBooleanSerializeRequestApplicationJson({
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
    var serialized = v;
    final charset = resolvedMediaType.parameters['charset'] ?? 'utf-8';
    final encoding = Encoding.getByName(charset) ?? utf8;
    Stream<List<int>> _stringResult(String src) {
      return encoding.encoder.bind(Stream.value(src));
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
        if (handleUnkownMediaType!=null) {
          yield* handleUnkownMediaType(resolvedMediaType, serialized, encoding, encodingRules);
          return;
        }
        yield* _stringResult(serialized.toString());
    }
  }
}





abstract class FakeApiFakeOuterCompositeSerializeRequest {
  static const pathTemplate = r'/fake/outer/composite';
  static String method = r'POST';

  String get contentType;
  final Map<String, String> extraHeaders;
  final Map<String, String> extraCookies;
  final Map<String, Object /* String | List<String> */> extraQueryParameters;

  

  const factory FakeApiFakeOuterCompositeSerializeRequest.unsafe({
    
    Map<String, String> extraHeaders,
    Map<String, Object> extraQueryParameters,
    Map<String, String> extraCookies,
    Stream<Uint8List>? body,
  }) = FakeApiFakeOuterCompositeSerializeRequestUnsafe;

  const FakeApiFakeOuterCompositeSerializeRequest({
    
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

/// A version of [FakeApiFakeOuterCompositeSerializeRequest], where you can send arbitrary bytes in the body.
class FakeApiFakeOuterCompositeSerializeRequestUnsafe extends FakeApiFakeOuterCompositeSerializeRequest {
  final Stream<Uint8List>? body;

  @override
  final String contentType;

  const FakeApiFakeOuterCompositeSerializeRequestUnsafe({
    this.body,
    this.contentType = 'application/octet-stream',
    
    super.extraHeaders,
    super.extraQueryParameters,
    super.extraCookies,
  });

  Stream<List<int>> getResolvedBody({
    required Map<String, String> headers,
    Map<String, dynamic> context = const {},
  }) async* {
    final body = this.body;
    if (body == null) {
      return;
    }
    yield* body;
  }
}




class FakeApiFakeOuterCompositeSerializeRequestApplicationJson extends FakeApiFakeOuterCompositeSerializeRequest {
  static const specMediaType = r'application/json';

  @override
  String get contentType => specMediaType;

  final 
            OuterComposite
 data;

  /// Pass this to handle serialization and encoding of unkown media types yourself.
  final UnknownMediaTypeHandler? handleUnkownMediaType;


  


  const FakeApiFakeOuterCompositeSerializeRequestApplicationJson({
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
    final charset = resolvedMediaType.parameters['charset'] ?? 'utf-8';
    final encoding = Encoding.getByName(charset) ?? utf8;
    Stream<List<int>> _stringResult(String src) {
      return encoding.encoder.bind(Stream.value(src));
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
        if (handleUnkownMediaType!=null) {
          yield* handleUnkownMediaType(resolvedMediaType, serialized, encoding, encodingRules);
          return;
        }
        yield* _stringResult(serialized.toString());
    }
  }
}





abstract class FakeApiFakeOuterNumberSerializeRequest {
  static const pathTemplate = r'/fake/outer/number';
  static String method = r'POST';

  String get contentType;
  final Map<String, String> extraHeaders;
  final Map<String, String> extraCookies;
  final Map<String, Object /* String | List<String> */> extraQueryParameters;

  

  const factory FakeApiFakeOuterNumberSerializeRequest.unsafe({
    
    Map<String, String> extraHeaders,
    Map<String, Object> extraQueryParameters,
    Map<String, String> extraCookies,
    Stream<Uint8List>? body,
  }) = FakeApiFakeOuterNumberSerializeRequestUnsafe;

  const FakeApiFakeOuterNumberSerializeRequest({
    
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

/// A version of [FakeApiFakeOuterNumberSerializeRequest], where you can send arbitrary bytes in the body.
class FakeApiFakeOuterNumberSerializeRequestUnsafe extends FakeApiFakeOuterNumberSerializeRequest {
  final Stream<Uint8List>? body;

  @override
  final String contentType;

  const FakeApiFakeOuterNumberSerializeRequestUnsafe({
    this.body,
    this.contentType = 'application/octet-stream',
    
    super.extraHeaders,
    super.extraQueryParameters,
    super.extraCookies,
  });

  Stream<List<int>> getResolvedBody({
    required Map<String, String> headers,
    Map<String, dynamic> context = const {},
  }) async* {
    final body = this.body;
    if (body == null) {
      return;
    }
    yield* body;
  }
}




class FakeApiFakeOuterNumberSerializeRequestApplicationJson extends FakeApiFakeOuterNumberSerializeRequest {
  static const specMediaType = r'application/json';

  @override
  String get contentType => specMediaType;

  final 
            num
 data;

  /// Pass this to handle serialization and encoding of unkown media types yourself.
  final UnknownMediaTypeHandler? handleUnkownMediaType;


  


  const FakeApiFakeOuterNumberSerializeRequestApplicationJson({
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
    var serialized = v;
    final charset = resolvedMediaType.parameters['charset'] ?? 'utf-8';
    final encoding = Encoding.getByName(charset) ?? utf8;
    Stream<List<int>> _stringResult(String src) {
      return encoding.encoder.bind(Stream.value(src));
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
        if (handleUnkownMediaType!=null) {
          yield* handleUnkownMediaType(resolvedMediaType, serialized, encoding, encodingRules);
          return;
        }
        yield* _stringResult(serialized.toString());
    }
  }
}





abstract class FakeApiFakeOuterStringSerializeRequest {
  static const pathTemplate = r'/fake/outer/string';
  static String method = r'POST';

  String get contentType;
  final Map<String, String> extraHeaders;
  final Map<String, String> extraCookies;
  final Map<String, Object /* String | List<String> */> extraQueryParameters;

  

  const factory FakeApiFakeOuterStringSerializeRequest.unsafe({
    
    Map<String, String> extraHeaders,
    Map<String, Object> extraQueryParameters,
    Map<String, String> extraCookies,
    Stream<Uint8List>? body,
  }) = FakeApiFakeOuterStringSerializeRequestUnsafe;

  const FakeApiFakeOuterStringSerializeRequest({
    
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

/// A version of [FakeApiFakeOuterStringSerializeRequest], where you can send arbitrary bytes in the body.
class FakeApiFakeOuterStringSerializeRequestUnsafe extends FakeApiFakeOuterStringSerializeRequest {
  final Stream<Uint8List>? body;

  @override
  final String contentType;

  const FakeApiFakeOuterStringSerializeRequestUnsafe({
    this.body,
    this.contentType = 'application/octet-stream',
    
    super.extraHeaders,
    super.extraQueryParameters,
    super.extraCookies,
  });

  Stream<List<int>> getResolvedBody({
    required Map<String, String> headers,
    Map<String, dynamic> context = const {},
  }) async* {
    final body = this.body;
    if (body == null) {
      return;
    }
    yield* body;
  }
}




class FakeApiFakeOuterStringSerializeRequestApplicationJson extends FakeApiFakeOuterStringSerializeRequest {
  static const specMediaType = r'application/json';

  @override
  String get contentType => specMediaType;

  final 
            String
 data;

  /// Pass this to handle serialization and encoding of unkown media types yourself.
  final UnknownMediaTypeHandler? handleUnkownMediaType;


  


  const FakeApiFakeOuterStringSerializeRequestApplicationJson({
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
    var serialized = v;
    final charset = resolvedMediaType.parameters['charset'] ?? 'utf-8';
    final encoding = Encoding.getByName(charset) ?? utf8;
    Stream<List<int>> _stringResult(String src) {
      return encoding.encoder.bind(Stream.value(src));
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
        if (handleUnkownMediaType!=null) {
          yield* handleUnkownMediaType(resolvedMediaType, serialized, encoding, encodingRules);
          return;
        }
        yield* _stringResult(serialized.toString());
    }
  }
}





abstract class FakeApiFakeUploadRefRequestBodiesRequest {
  static const pathTemplate = r'/fake/pet/{petId}/uploadImage';
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
  
  
  

  const factory FakeApiFakeUploadRefRequestBodiesRequest.unsafe({
    
    required 
            int
 petId,
    
    Map<String, String> extraHeaders,
    Map<String, Object> extraQueryParameters,
    Map<String, String> extraCookies,
    Stream<Uint8List>? body,
  }) = FakeApiFakeUploadRefRequestBodiesRequestUnsafe;

  const FakeApiFakeUploadRefRequestBodiesRequest({
    
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

/// A version of [FakeApiFakeUploadRefRequestBodiesRequest], where you can send arbitrary bytes in the body.
class FakeApiFakeUploadRefRequestBodiesRequestUnsafe extends FakeApiFakeUploadRefRequestBodiesRequest {
  final Stream<Uint8List>? body;

  @override
  final String contentType;

  const FakeApiFakeUploadRefRequestBodiesRequestUnsafe({
    this.body,
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
    final body = this.body;
    if (body == null) {
      return;
    }
    yield* body;
  }
}









 class FakeApiGetFakeArrayofenumsRequest {
  static const pathTemplate = r'/fake/array-of-enums';
  static String method = r'GET';

  final Map<String, String> extraHeaders;
  final Map<String, String> extraCookies;
  final Map<String, Object /* String | List<String> */> extraQueryParameters;



  const FakeApiGetFakeArrayofenumsRequest({
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







 class FakeApiGetFakeHealthRequest {
  static const pathTemplate = r'/fake/health';
  static String method = r'GET';

  final Map<String, String> extraHeaders;
  final Map<String, String> extraCookies;
  final Map<String, Object /* String | List<String> */> extraQueryParameters;



  const FakeApiGetFakeHealthRequest({
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







 class FakeApiGetParameterNameMappingRequest {
  static const pathTemplate = r'/fake/parameter-name-mapping';
  static String method = r'GET';

  final Map<String, String> extraHeaders;
  final Map<String, String> extraCookies;
  final Map<String, Object /* String | List<String> */> extraQueryParameters;

  
  /// _type
  /// spec name: _type
  final 
            int
 $type;
  
  
  /// type
  /// spec name: type
  final 
            String
 type;
  
  
  /// type_
  /// spec name: type_
  final 
            String
 type$;
  


  const FakeApiGetParameterNameMappingRequest({
    
    required this.$type    ,
    
    
    required this.type    ,
    
    
    required this.type$    ,
    
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
      methodUri = OpenApiParameterSerializationQuery.fromStyle(r'form', explode: true, parameterName: r'type', allowEmptyValue: false,).expandUri(methodUri, type);

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
      
  r'_type': OpenApiParameterSerializationHeader(parameterName: r'_type',explode: false).serialize($type),
      
  r'type_': OpenApiParameterSerializationHeader(parameterName: r'type_',explode: false).serialize(type$),
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







abstract class FakeApiTestAdditionalPropertiesReferenceRequest {
  static const pathTemplate = r'/fake/additionalProperties-reference';
  static String method = r'POST';

  String get contentType;
  final Map<String, String> extraHeaders;
  final Map<String, String> extraCookies;
  final Map<String, Object /* String | List<String> */> extraQueryParameters;

  

  const factory FakeApiTestAdditionalPropertiesReferenceRequest.unsafe({
    
    Map<String, String> extraHeaders,
    Map<String, Object> extraQueryParameters,
    Map<String, String> extraCookies,
    Stream<Uint8List>? body,
  }) = FakeApiTestAdditionalPropertiesReferenceRequestUnsafe;

  const FakeApiTestAdditionalPropertiesReferenceRequest({
    
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

/// A version of [FakeApiTestAdditionalPropertiesReferenceRequest], where you can send arbitrary bytes in the body.
class FakeApiTestAdditionalPropertiesReferenceRequestUnsafe extends FakeApiTestAdditionalPropertiesReferenceRequest {
  final Stream<Uint8List>? body;

  @override
  final String contentType;

  const FakeApiTestAdditionalPropertiesReferenceRequestUnsafe({
    this.body,
    this.contentType = 'application/octet-stream',
    
    super.extraHeaders,
    super.extraQueryParameters,
    super.extraCookies,
  });

  Stream<List<int>> getResolvedBody({
    required Map<String, String> headers,
    Map<String, dynamic> context = const {},
  }) async* {
    final body = this.body;
    if (body == null) {
      return;
    }
    yield* body;
  }
}




class FakeApiTestAdditionalPropertiesReferenceRequestApplicationJson extends FakeApiTestAdditionalPropertiesReferenceRequest {
  static const specMediaType = r'application/json';

  @override
  String get contentType => specMediaType;

  final 
    Map<String, 
        Object
?>
 data;

  /// Pass this to handle serialization and encoding of unkown media types yourself.
  final UnknownMediaTypeHandler? handleUnkownMediaType;


  


  const FakeApiTestAdditionalPropertiesReferenceRequestApplicationJson({
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
    var serialized = v;
    final charset = resolvedMediaType.parameters['charset'] ?? 'utf-8';
    final encoding = Encoding.getByName(charset) ?? utf8;
    Stream<List<int>> _stringResult(String src) {
      return encoding.encoder.bind(Stream.value(src));
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
        if (handleUnkownMediaType!=null) {
          yield* handleUnkownMediaType(resolvedMediaType, serialized, encoding, encodingRules);
          return;
        }
        yield* _stringResult(serialized.toString());
    }
  }
}





abstract class FakeApiTestBodyWithFileSchemaRequest {
  static const pathTemplate = r'/fake/body-with-file-schema';
  static String method = r'PUT';

  String get contentType;
  final Map<String, String> extraHeaders;
  final Map<String, String> extraCookies;
  final Map<String, Object /* String | List<String> */> extraQueryParameters;

  

  const factory FakeApiTestBodyWithFileSchemaRequest.unsafe({
    
    Map<String, String> extraHeaders,
    Map<String, Object> extraQueryParameters,
    Map<String, String> extraCookies,
    Stream<Uint8List>? body,
  }) = FakeApiTestBodyWithFileSchemaRequestUnsafe;

  const FakeApiTestBodyWithFileSchemaRequest({
    
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

/// A version of [FakeApiTestBodyWithFileSchemaRequest], where you can send arbitrary bytes in the body.
class FakeApiTestBodyWithFileSchemaRequestUnsafe extends FakeApiTestBodyWithFileSchemaRequest {
  final Stream<Uint8List>? body;

  @override
  final String contentType;

  const FakeApiTestBodyWithFileSchemaRequestUnsafe({
    this.body,
    this.contentType = 'application/octet-stream',
    
    super.extraHeaders,
    super.extraQueryParameters,
    super.extraCookies,
  });

  Stream<List<int>> getResolvedBody({
    required Map<String, String> headers,
    Map<String, dynamic> context = const {},
  }) async* {
    final body = this.body;
    if (body == null) {
      return;
    }
    yield* body;
  }
}




class FakeApiTestBodyWithFileSchemaRequestApplicationJson extends FakeApiTestBodyWithFileSchemaRequest {
  static const specMediaType = r'application/json';

  @override
  String get contentType => specMediaType;

  final 
            FileSchemaTestClass
 data;

  /// Pass this to handle serialization and encoding of unkown media types yourself.
  final UnknownMediaTypeHandler? handleUnkownMediaType;


  


  const FakeApiTestBodyWithFileSchemaRequestApplicationJson({
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
    final charset = resolvedMediaType.parameters['charset'] ?? 'utf-8';
    final encoding = Encoding.getByName(charset) ?? utf8;
    Stream<List<int>> _stringResult(String src) {
      return encoding.encoder.bind(Stream.value(src));
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
        if (handleUnkownMediaType!=null) {
          yield* handleUnkownMediaType(resolvedMediaType, serialized, encoding, encodingRules);
          return;
        }
        yield* _stringResult(serialized.toString());
    }
  }
}





abstract class FakeApiTestBodyWithQueryParamsRequest {
  static const pathTemplate = r'/fake/body-with-query-params';
  static String method = r'PUT';

  String get contentType;
  final Map<String, String> extraHeaders;
  final Map<String, String> extraCookies;
  final Map<String, Object /* String | List<String> */> extraQueryParameters;

  
  /// 
  /// spec name: query
  final 
            String
 query;
  
  

  const factory FakeApiTestBodyWithQueryParamsRequest.unsafe({
    
    required 
            String
 query,
    
    Map<String, String> extraHeaders,
    Map<String, Object> extraQueryParameters,
    Map<String, String> extraCookies,
    Stream<Uint8List>? body,
  }) = FakeApiTestBodyWithQueryParamsRequestUnsafe;

  const FakeApiTestBodyWithQueryParamsRequest({
    
    required this.query    ,
    
    
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
      methodUri = OpenApiParameterSerializationQuery.fromStyle(r'form', explode: true, parameterName: r'query', allowEmptyValue: false,).expandUri(methodUri, query);

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

/// A version of [FakeApiTestBodyWithQueryParamsRequest], where you can send arbitrary bytes in the body.
class FakeApiTestBodyWithQueryParamsRequestUnsafe extends FakeApiTestBodyWithQueryParamsRequest {
  final Stream<Uint8List>? body;

  @override
  final String contentType;

  const FakeApiTestBodyWithQueryParamsRequestUnsafe({
    this.body,
    this.contentType = 'application/octet-stream',
    
    required super.query,
    
    
    super.extraHeaders,
    super.extraQueryParameters,
    super.extraCookies,
  });

  Stream<List<int>> getResolvedBody({
    required Map<String, String> headers,
    Map<String, dynamic> context = const {},
  }) async* {
    final body = this.body;
    if (body == null) {
      return;
    }
    yield* body;
  }
}




class FakeApiTestBodyWithQueryParamsRequestApplicationJson extends FakeApiTestBodyWithQueryParamsRequest {
  static const specMediaType = r'application/json';

  @override
  String get contentType => specMediaType;

  final 
            User
 data;

  /// Pass this to handle serialization and encoding of unkown media types yourself.
  final UnknownMediaTypeHandler? handleUnkownMediaType;


  


  const FakeApiTestBodyWithQueryParamsRequestApplicationJson({
    required this.data,
    this.handleUnkownMediaType,
    
    required super.query,
    
    
    
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
    final charset = resolvedMediaType.parameters['charset'] ?? 'utf-8';
    final encoding = Encoding.getByName(charset) ?? utf8;
    Stream<List<int>> _stringResult(String src) {
      return encoding.encoder.bind(Stream.value(src));
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
        if (handleUnkownMediaType!=null) {
          yield* handleUnkownMediaType(resolvedMediaType, serialized, encoding, encodingRules);
          return;
        }
        yield* _stringResult(serialized.toString());
    }
  }
}





abstract class FakeApiTestClientModelRequest {
  static const pathTemplate = r'/fake';
  static String method = r'PATCH';

  String get contentType;
  final Map<String, String> extraHeaders;
  final Map<String, String> extraCookies;
  final Map<String, Object /* String | List<String> */> extraQueryParameters;

  

  const factory FakeApiTestClientModelRequest.unsafe({
    
    Map<String, String> extraHeaders,
    Map<String, Object> extraQueryParameters,
    Map<String, String> extraCookies,
    Stream<Uint8List>? body,
  }) = FakeApiTestClientModelRequestUnsafe;

  const FakeApiTestClientModelRequest({
    
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

/// A version of [FakeApiTestClientModelRequest], where you can send arbitrary bytes in the body.
class FakeApiTestClientModelRequestUnsafe extends FakeApiTestClientModelRequest {
  final Stream<Uint8List>? body;

  @override
  final String contentType;

  const FakeApiTestClientModelRequestUnsafe({
    this.body,
    this.contentType = 'application/octet-stream',
    
    super.extraHeaders,
    super.extraQueryParameters,
    super.extraCookies,
  });

  Stream<List<int>> getResolvedBody({
    required Map<String, String> headers,
    Map<String, dynamic> context = const {},
  }) async* {
    final body = this.body;
    if (body == null) {
      return;
    }
    yield* body;
  }
}




class FakeApiTestClientModelRequestApplicationJson extends FakeApiTestClientModelRequest {
  static const specMediaType = r'application/json';

  @override
  String get contentType => specMediaType;

  final 
            Client
 data;

  /// Pass this to handle serialization and encoding of unkown media types yourself.
  final UnknownMediaTypeHandler? handleUnkownMediaType;


  


  const FakeApiTestClientModelRequestApplicationJson({
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
    final charset = resolvedMediaType.parameters['charset'] ?? 'utf-8';
    final encoding = Encoding.getByName(charset) ?? utf8;
    Stream<List<int>> _stringResult(String src) {
      return encoding.encoder.bind(Stream.value(src));
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
        if (handleUnkownMediaType!=null) {
          yield* handleUnkownMediaType(resolvedMediaType, serialized, encoding, encodingRules);
          return;
        }
        yield* _stringResult(serialized.toString());
    }
  }
}





abstract class FakeApiTestEndpointParametersRequest {
  static const pathTemplate = r'/fake';
  static String method = r'POST';

  String get contentType;
  final Map<String, String> extraHeaders;
  final Map<String, String> extraCookies;
  final Map<String, Object /* String | List<String> */> extraQueryParameters;

  
  
  
  
  
  
  
  
  
  
  
  
  
  

  const factory FakeApiTestEndpointParametersRequest.unsafe({
    
    Map<String, String> extraHeaders,
    Map<String, Object> extraQueryParameters,
    Map<String, String> extraCookies,
    Stream<Uint8List>? body,
  }) = FakeApiTestEndpointParametersRequestUnsafe;

  const FakeApiTestEndpointParametersRequest({
    
    
    
    
    
    
    
    
    
    
    
    
    
    
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

/// A version of [FakeApiTestEndpointParametersRequest], where you can send arbitrary bytes in the body.
class FakeApiTestEndpointParametersRequestUnsafe extends FakeApiTestEndpointParametersRequest {
  final Stream<Uint8List>? body;

  @override
  final String contentType;

  const FakeApiTestEndpointParametersRequestUnsafe({
    this.body,
    this.contentType = 'application/octet-stream',
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    super.extraHeaders,
    super.extraQueryParameters,
    super.extraCookies,
  });

  Stream<List<int>> getResolvedBody({
    required Map<String, String> headers,
    Map<String, dynamic> context = const {},
  }) async* {
    final body = this.body;
    if (body == null) {
      return;
    }
    yield* body;
  }
}








extension type const EnumHeaderStringArrayEnum._(String value) {
  /// Header parameter enum test (string array)
      const EnumHeaderStringArrayEnum.greaterThan() : this._(r'>');
  /// Header parameter enum test (string array)
      const EnumHeaderStringArrayEnum.value() : this._(r'$');

  /// Creates a [EnumHeaderStringArrayEnum] enum from a value and safely checking if it exists.
  factory EnumHeaderStringArrayEnum.$safe(String value) {
    final res = values.where((element) => element.value == value).firstOrNull;
    if (res == null) {
      throw 'Invalid enum value $value';
    }
    return res;
  }

  /// Creates a [EnumHeaderStringArrayEnum] enum from a value without checking if it exists.
  const EnumHeaderStringArrayEnum.$unsafe(String value) : this._(value);

  /// All possible values of the enum.
  static const List<EnumHeaderStringArrayEnum> values = [
    EnumHeaderStringArrayEnum.greaterThan(),
    EnumHeaderStringArrayEnum.value(),
    
  ];
}

extension type const EnumHeaderStringEnum._(String value) {
  /// Header parameter enum test (string)
      const EnumHeaderStringEnum.abc() : this._(r'_abc');
  /// Header parameter enum test (string)
      const EnumHeaderStringEnum.efg() : this._(r'-efg');
  /// Header parameter enum test (string)
      const EnumHeaderStringEnum.xyz() : this._(r'(xyz)');

  /// Creates a [EnumHeaderStringEnum] enum from a value and safely checking if it exists.
  factory EnumHeaderStringEnum.$safe(String value) {
    final res = values.where((element) => element.value == value).firstOrNull;
    if (res == null) {
      throw 'Invalid enum value $value';
    }
    return res;
  }

  /// Creates a [EnumHeaderStringEnum] enum from a value without checking if it exists.
  const EnumHeaderStringEnum.$unsafe(String value) : this._(value);

  /// All possible values of the enum.
  static const List<EnumHeaderStringEnum> values = [
    EnumHeaderStringEnum.abc(),
    EnumHeaderStringEnum.efg(),
    EnumHeaderStringEnum.xyz(),
    
  ];
}

extension type const EnumQueryStringArrayEnum._(String value) {
  /// Query parameter enum test (string array)
      const EnumQueryStringArrayEnum.greaterThan() : this._(r'>');
  /// Query parameter enum test (string array)
      const EnumQueryStringArrayEnum.value() : this._(r'$');

  /// Creates a [EnumQueryStringArrayEnum] enum from a value and safely checking if it exists.
  factory EnumQueryStringArrayEnum.$safe(String value) {
    final res = values.where((element) => element.value == value).firstOrNull;
    if (res == null) {
      throw 'Invalid enum value $value';
    }
    return res;
  }

  /// Creates a [EnumQueryStringArrayEnum] enum from a value without checking if it exists.
  const EnumQueryStringArrayEnum.$unsafe(String value) : this._(value);

  /// All possible values of the enum.
  static const List<EnumQueryStringArrayEnum> values = [
    EnumQueryStringArrayEnum.greaterThan(),
    EnumQueryStringArrayEnum.value(),
    
  ];
}

extension type const EnumQueryStringEnum._(String value) {
  /// Query parameter enum test (string)
      const EnumQueryStringEnum.abc() : this._(r'_abc');
  /// Query parameter enum test (string)
      const EnumQueryStringEnum.efg() : this._(r'-efg');
  /// Query parameter enum test (string)
      const EnumQueryStringEnum.xyz() : this._(r'(xyz)');

  /// Creates a [EnumQueryStringEnum] enum from a value and safely checking if it exists.
  factory EnumQueryStringEnum.$safe(String value) {
    final res = values.where((element) => element.value == value).firstOrNull;
    if (res == null) {
      throw 'Invalid enum value $value';
    }
    return res;
  }

  /// Creates a [EnumQueryStringEnum] enum from a value without checking if it exists.
  const EnumQueryStringEnum.$unsafe(String value) : this._(value);

  /// All possible values of the enum.
  static const List<EnumQueryStringEnum> values = [
    EnumQueryStringEnum.abc(),
    EnumQueryStringEnum.efg(),
    EnumQueryStringEnum.xyz(),
    
  ];
}

extension type const EnumQueryIntegerEnum._(int value) {
  /// Query parameter enum test (double)
      const EnumQueryIntegerEnum.number1() : this._(1);
  /// Query parameter enum test (double)
      const EnumQueryIntegerEnum.numberNegative2() : this._(-2);

  /// Creates a [EnumQueryIntegerEnum] enum from a value and safely checking if it exists.
  factory EnumQueryIntegerEnum.$safe(int value) {
    final res = values.where((element) => element.value == value).firstOrNull;
    if (res == null) {
      throw 'Invalid enum value $value';
    }
    return res;
  }

  /// Creates a [EnumQueryIntegerEnum] enum from a value without checking if it exists.
  const EnumQueryIntegerEnum.$unsafe(int value) : this._(value);

  /// All possible values of the enum.
  static const List<EnumQueryIntegerEnum> values = [
    EnumQueryIntegerEnum.number1(),
    EnumQueryIntegerEnum.numberNegative2(),
    
  ];
}

extension type const EnumQueryDoubleEnum._(double value) {
  /// Query parameter enum test (double)
      const EnumQueryDoubleEnum.number11() : this._(1.1);
  /// Query parameter enum test (double)
      const EnumQueryDoubleEnum.numberNegative12() : this._(-1.2);

  /// Creates a [EnumQueryDoubleEnum] enum from a value and safely checking if it exists.
  factory EnumQueryDoubleEnum.$safe(double value) {
    final res = values.where((element) => element.value == value).firstOrNull;
    if (res == null) {
      throw 'Invalid enum value $value';
    }
    return res;
  }

  /// Creates a [EnumQueryDoubleEnum] enum from a value without checking if it exists.
  const EnumQueryDoubleEnum.$unsafe(double value) : this._(value);

  /// All possible values of the enum.
  static const List<EnumQueryDoubleEnum> values = [
    EnumQueryDoubleEnum.number11(),
    EnumQueryDoubleEnum.numberNegative12(),
    
  ];
}

extension type const EnumFormStringArrayEnum._(String value) {
  /// Form parameter enum test (string array)
      const EnumFormStringArrayEnum.greaterThan() : this._(r'>');
  /// Form parameter enum test (string array)
      const EnumFormStringArrayEnum.value() : this._(r'$');

  /// Creates a [EnumFormStringArrayEnum] enum from a value and safely checking if it exists.
  factory EnumFormStringArrayEnum.$safe(String value) {
    final res = values.where((element) => element.value == value).firstOrNull;
    if (res == null) {
      throw 'Invalid enum value $value';
    }
    return res;
  }

  /// Creates a [EnumFormStringArrayEnum] enum from a value without checking if it exists.
  const EnumFormStringArrayEnum.$unsafe(String value) : this._(value);

  /// All possible values of the enum.
  static const List<EnumFormStringArrayEnum> values = [
    EnumFormStringArrayEnum.greaterThan(),
    EnumFormStringArrayEnum.value(),
    
  ];
}

extension type const EnumFormStringEnum._(String value) {
  /// Form parameter enum test (string)
      const EnumFormStringEnum.abc() : this._(r'_abc');
  /// Form parameter enum test (string)
      const EnumFormStringEnum.efg() : this._(r'-efg');
  /// Form parameter enum test (string)
      const EnumFormStringEnum.xyz() : this._(r'(xyz)');

  /// Creates a [EnumFormStringEnum] enum from a value and safely checking if it exists.
  factory EnumFormStringEnum.$safe(String value) {
    final res = values.where((element) => element.value == value).firstOrNull;
    if (res == null) {
      throw 'Invalid enum value $value';
    }
    return res;
  }

  /// Creates a [EnumFormStringEnum] enum from a value without checking if it exists.
  const EnumFormStringEnum.$unsafe(String value) : this._(value);

  /// All possible values of the enum.
  static const List<EnumFormStringEnum> values = [
    EnumFormStringEnum.abc(),
    EnumFormStringEnum.efg(),
    EnumFormStringEnum.xyz(),
    
  ];
}


 class FakeApiTestEnumParametersRequest {
  static const pathTemplate = r'/fake';
  static String method = r'GET';

  final Map<String, String> extraHeaders;
  final Map<String, String> extraCookies;
  final Map<String, Object /* String | List<String> */> extraQueryParameters;

  
  /// Header parameter enum test (string array)
  /// spec name: enum_header_string_array
  final UndefinedWrapper<
    List<
        
            EnumHeaderStringArrayEnum
>
> enumHeaderStringArray;
  
  
  /// Header parameter enum test (string)
  /// spec name: enum_header_string
  final UndefinedWrapper<
            EnumHeaderStringEnum
> enumHeaderString;
  
  
  /// Query parameter enum test (string array)
  /// spec name: enum_query_string_array
  final UndefinedWrapper<
    List<
        
            EnumQueryStringArrayEnum
>
> enumQueryStringArray;
  
  
  /// Query parameter enum test (string)
  /// spec name: enum_query_string
  final UndefinedWrapper<
            EnumQueryStringEnum
> enumQueryString;
  
  
  /// Query parameter enum test (double)
  /// spec name: enum_query_integer
  final UndefinedWrapper<
            EnumQueryIntegerEnum
> enumQueryInteger;
  
  
  /// Query parameter enum test (double)
  /// spec name: enum_query_double
  final UndefinedWrapper<
            EnumQueryDoubleEnum
> enumQueryDouble;
  
  
  


  const FakeApiTestEnumParametersRequest({
    
     this.enumHeaderStringArray= const UndefinedWrapper
        .undefined()
,
    
    
     this.enumHeaderString= const UndefinedWrapper
    (
        EnumHeaderStringEnum.$unsafe('-efg')
        
    )
    
,
    
    
     this.enumQueryStringArray= const UndefinedWrapper
        .undefined()
,
    
    
     this.enumQueryString= const UndefinedWrapper
    (
        EnumQueryStringEnum.$unsafe('-efg')
        
    )
    
,
    
    
     this.enumQueryInteger= const UndefinedWrapper
        .undefined()
,
    
    
     this.enumQueryDouble= const UndefinedWrapper
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
    var methodUri = Uri(path: resolvedPath);
    if (enumQueryStringArray.isDefined) {
      methodUri = OpenApiParameterSerializationQuery.fromStyle(r'form', explode: true, parameterName: r'enum_query_string_array', allowEmptyValue: false,).expandUri(methodUri, enumQueryStringArray.valueRequired);
    }
    if (enumQueryString.isDefined) {
      methodUri = OpenApiParameterSerializationQuery.fromStyle(r'form', explode: true, parameterName: r'enum_query_string', allowEmptyValue: false,).expandUri(methodUri, enumQueryString.valueRequired);
    }
    if (enumQueryInteger.isDefined) {
      methodUri = OpenApiParameterSerializationQuery.fromStyle(r'form', explode: true, parameterName: r'enum_query_integer', allowEmptyValue: false,).expandUri(methodUri, enumQueryInteger.valueRequired);
    }
    if (enumQueryDouble.isDefined) {
      methodUri = OpenApiParameterSerializationQuery.fromStyle(r'form', explode: true, parameterName: r'enum_query_double', allowEmptyValue: false,).expandUri(methodUri, enumQueryDouble.valueRequired);
    }

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
      if (enumHeaderStringArray.isDefined)
  r'enum_header_string_array': OpenApiParameterSerializationHeader(parameterName: r'enum_header_string_array',explode: false).serialize(enumHeaderStringArray.valueRequired),
      if (enumHeaderString.isDefined)
  r'enum_header_string': OpenApiParameterSerializationHeader(parameterName: r'enum_header_string',explode: false).serialize(enumHeaderString.valueRequired),
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







 class FakeApiTestGroupParametersRequest {
  static const pathTemplate = r'/fake';
  static String method = r'DELETE';

  final Map<String, String> extraHeaders;
  final Map<String, String> extraCookies;
  final Map<String, Object /* String | List<String> */> extraQueryParameters;

  
  /// Required String in group parameters
  /// spec name: required_string_group
  final 
            int
 requiredStringGroup;
  
  
  /// Required Boolean in group parameters
  /// spec name: required_boolean_group
  final 
            bool
 requiredBooleanGroup;
  
  
  /// Required Integer in group parameters
  /// spec name: required_int64_group
  final 
            int
 requiredInt64Group;
  
  
  /// String in group parameters
  /// spec name: string_group
  final UndefinedWrapper<
            int
> stringGroup;
  
  
  /// Boolean in group parameters
  /// spec name: boolean_group
  final UndefinedWrapper<
            bool
> booleanGroup;
  
  
  /// Integer in group parameters
  /// spec name: int64_group
  final UndefinedWrapper<
            int
> int64Group;
  


  const FakeApiTestGroupParametersRequest({
    
    required this.requiredStringGroup    ,
    
    
    required this.requiredBooleanGroup    ,
    
    
    required this.requiredInt64Group    ,
    
    
     this.stringGroup= const UndefinedWrapper
        .undefined()
,
    
    
     this.booleanGroup= const UndefinedWrapper
        .undefined()
,
    
    
     this.int64Group= const UndefinedWrapper
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
    var methodUri = Uri(path: resolvedPath);
      methodUri = OpenApiParameterSerializationQuery.fromStyle(r'form', explode: true, parameterName: r'required_string_group', allowEmptyValue: false,).expandUri(methodUri, requiredStringGroup);
      methodUri = OpenApiParameterSerializationQuery.fromStyle(r'form', explode: true, parameterName: r'required_int64_group', allowEmptyValue: false,).expandUri(methodUri, requiredInt64Group);
    if (stringGroup.isDefined) {
      methodUri = OpenApiParameterSerializationQuery.fromStyle(r'form', explode: true, parameterName: r'string_group', allowEmptyValue: false,).expandUri(methodUri, stringGroup.valueRequired);
    }
    if (int64Group.isDefined) {
      methodUri = OpenApiParameterSerializationQuery.fromStyle(r'form', explode: true, parameterName: r'int64_group', allowEmptyValue: false,).expandUri(methodUri, int64Group.valueRequired);
    }

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
      
  r'required_boolean_group': OpenApiParameterSerializationHeader(parameterName: r'required_boolean_group',explode: false).serialize(requiredBooleanGroup),
      if (booleanGroup.isDefined)
  r'boolean_group': OpenApiParameterSerializationHeader(parameterName: r'boolean_group',explode: false).serialize(booleanGroup.valueRequired),
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







abstract class FakeApiTestInlineAdditionalPropertiesRequest {
  static const pathTemplate = r'/fake/inline-additionalProperties';
  static String method = r'POST';

  String get contentType;
  final Map<String, String> extraHeaders;
  final Map<String, String> extraCookies;
  final Map<String, Object /* String | List<String> */> extraQueryParameters;

  

  const factory FakeApiTestInlineAdditionalPropertiesRequest.unsafe({
    
    Map<String, String> extraHeaders,
    Map<String, Object> extraQueryParameters,
    Map<String, String> extraCookies,
    Stream<Uint8List>? body,
  }) = FakeApiTestInlineAdditionalPropertiesRequestUnsafe;

  const FakeApiTestInlineAdditionalPropertiesRequest({
    
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

/// A version of [FakeApiTestInlineAdditionalPropertiesRequest], where you can send arbitrary bytes in the body.
class FakeApiTestInlineAdditionalPropertiesRequestUnsafe extends FakeApiTestInlineAdditionalPropertiesRequest {
  final Stream<Uint8List>? body;

  @override
  final String contentType;

  const FakeApiTestInlineAdditionalPropertiesRequestUnsafe({
    this.body,
    this.contentType = 'application/octet-stream',
    
    super.extraHeaders,
    super.extraQueryParameters,
    super.extraCookies,
  });

  Stream<List<int>> getResolvedBody({
    required Map<String, String> headers,
    Map<String, dynamic> context = const {},
  }) async* {
    final body = this.body;
    if (body == null) {
      return;
    }
    yield* body;
  }
}




class FakeApiTestInlineAdditionalPropertiesRequestApplicationJson extends FakeApiTestInlineAdditionalPropertiesRequest {
  static const specMediaType = r'application/json';

  @override
  String get contentType => specMediaType;

  final 
    Map<String, 
        
            String
>
 data;

  /// Pass this to handle serialization and encoding of unkown media types yourself.
  final UnknownMediaTypeHandler? handleUnkownMediaType;


  


  const FakeApiTestInlineAdditionalPropertiesRequestApplicationJson({
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
    var serialized = v.map((k,v) => MapEntry(k, v));
    final charset = resolvedMediaType.parameters['charset'] ?? 'utf-8';
    final encoding = Encoding.getByName(charset) ?? utf8;
    Stream<List<int>> _stringResult(String src) {
      return encoding.encoder.bind(Stream.value(src));
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
        if (handleUnkownMediaType!=null) {
          yield* handleUnkownMediaType(resolvedMediaType, serialized, encoding, encodingRules);
          return;
        }
        yield* _stringResult(serialized.toString());
    }
  }
}





abstract class FakeApiTestInlineFreeformAdditionalPropertiesRequest {
  static const pathTemplate = r'/fake/inline-freeform-additionalProperties';
  static String method = r'POST';

  String get contentType;
  final Map<String, String> extraHeaders;
  final Map<String, String> extraCookies;
  final Map<String, Object /* String | List<String> */> extraQueryParameters;

  

  const factory FakeApiTestInlineFreeformAdditionalPropertiesRequest.unsafe({
    
    Map<String, String> extraHeaders,
    Map<String, Object> extraQueryParameters,
    Map<String, String> extraCookies,
    Stream<Uint8List>? body,
  }) = FakeApiTestInlineFreeformAdditionalPropertiesRequestUnsafe;

  const FakeApiTestInlineFreeformAdditionalPropertiesRequest({
    
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

/// A version of [FakeApiTestInlineFreeformAdditionalPropertiesRequest], where you can send arbitrary bytes in the body.
class FakeApiTestInlineFreeformAdditionalPropertiesRequestUnsafe extends FakeApiTestInlineFreeformAdditionalPropertiesRequest {
  final Stream<Uint8List>? body;

  @override
  final String contentType;

  const FakeApiTestInlineFreeformAdditionalPropertiesRequestUnsafe({
    this.body,
    this.contentType = 'application/octet-stream',
    
    super.extraHeaders,
    super.extraQueryParameters,
    super.extraCookies,
  });

  Stream<List<int>> getResolvedBody({
    required Map<String, String> headers,
    Map<String, dynamic> context = const {},
  }) async* {
    final body = this.body;
    if (body == null) {
      return;
    }
    yield* body;
  }
}




class FakeApiTestInlineFreeformAdditionalPropertiesRequestApplicationJson extends FakeApiTestInlineFreeformAdditionalPropertiesRequest {
  static const specMediaType = r'application/json';

  @override
  String get contentType => specMediaType;

  final 
            TestInlineFreeformAdditionalPropertiesRequest
 data;

  /// Pass this to handle serialization and encoding of unkown media types yourself.
  final UnknownMediaTypeHandler? handleUnkownMediaType;


  


  const FakeApiTestInlineFreeformAdditionalPropertiesRequestApplicationJson({
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
    var serialized = v;
    final charset = resolvedMediaType.parameters['charset'] ?? 'utf-8';
    final encoding = Encoding.getByName(charset) ?? utf8;
    Stream<List<int>> _stringResult(String src) {
      return encoding.encoder.bind(Stream.value(src));
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
        if (handleUnkownMediaType!=null) {
          yield* handleUnkownMediaType(resolvedMediaType, serialized, encoding, encodingRules);
          return;
        }
        yield* _stringResult(serialized.toString());
    }
  }
}





 class FakeApiTestJsonFormDataRequest {
  static const pathTemplate = r'/fake/jsonFormData';
  static String method = r'GET';

  final Map<String, String> extraHeaders;
  final Map<String, String> extraCookies;
  final Map<String, Object /* String | List<String> */> extraQueryParameters;

  
  


  const FakeApiTestJsonFormDataRequest({
    
    
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







abstract class FakeApiTestQueryParameterCollectionFormatRequest {
  static const pathTemplate = r'/fake/test-query-parameters';
  static String method = r'PUT';

  String get contentType;
  final Map<String, String> extraHeaders;
  final Map<String, String> extraCookies;
  final Map<String, Object /* String | List<String> */> extraQueryParameters;

  
  /// 
  /// spec name: pipe
  final 
    List<
        
            String
>
 pipe;
  
  
  /// 
  /// spec name: ioutil
  final 
    List<
        
            String
>
 ioutil;
  
  
  /// 
  /// spec name: http
  final 
    List<
        
            String
>
 http;
  
  
  /// 
  /// spec name: url
  final 
    List<
        
            String
>
 url;
  
  
  /// 
  /// spec name: context
  final 
    List<
        
            String
>
 context;
  

  const factory FakeApiTestQueryParameterCollectionFormatRequest.unsafe({
    
    required 
    List<
        
            String
>
 pipe,
    
    required 
    List<
        
            String
>
 ioutil,
    
    required 
    List<
        
            String
>
 http,
    
    required 
    List<
        
            String
>
 url,
    
    required 
    List<
        
            String
>
 context,
    
    Map<String, String> extraHeaders,
    Map<String, Object> extraQueryParameters,
    Map<String, String> extraCookies,
    Stream<Uint8List>? body,
  }) = FakeApiTestQueryParameterCollectionFormatRequestUnsafe;

  const FakeApiTestQueryParameterCollectionFormatRequest({
    
    required this.pipe    ,
    
    
    required this.ioutil    ,
    
    
    required this.http    ,
    
    
    required this.url    ,
    
    
    required this.context    ,
    
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
      methodUri = OpenApiParameterSerializationQuery.fromStyle(r'form', explode: true, parameterName: r'pipe', allowEmptyValue: false,).expandUri(methodUri, pipe);
      methodUri = OpenApiParameterSerializationQuery.fromStyle(r'form', explode: false, parameterName: r'ioutil', allowEmptyValue: false,).expandUri(methodUri, ioutil);
      methodUri = OpenApiParameterSerializationQuery.fromStyle(r'spaceDelimited', explode: false, parameterName: r'http', allowEmptyValue: false,).expandUri(methodUri, http);
      methodUri = OpenApiParameterSerializationQuery.fromStyle(r'form', explode: false, parameterName: r'url', allowEmptyValue: false,).expandUri(methodUri, url);
      methodUri = OpenApiParameterSerializationQuery.fromStyle(r'form', explode: true, parameterName: r'context', allowEmptyValue: false,).expandUri(methodUri, context);

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

/// A version of [FakeApiTestQueryParameterCollectionFormatRequest], where you can send arbitrary bytes in the body.
class FakeApiTestQueryParameterCollectionFormatRequestUnsafe extends FakeApiTestQueryParameterCollectionFormatRequest {
  final Stream<Uint8List>? body;

  @override
  final String contentType;

  const FakeApiTestQueryParameterCollectionFormatRequestUnsafe({
    this.body,
    this.contentType = 'application/octet-stream',
    
    required super.pipe,
    
    
    required super.ioutil,
    
    
    required super.http,
    
    
    required super.url,
    
    
    required super.context,
    
    super.extraHeaders,
    super.extraQueryParameters,
    super.extraCookies,
  });

  Stream<List<int>> getResolvedBody({
    required Map<String, String> headers,
    Map<String, dynamic> context = const {},
  }) async* {
    final body = this.body;
    if (body == null) {
      return;
    }
    yield* body;
  }
}









abstract class FakeApiTestStringMapReferenceRequest {
  static const pathTemplate = r'/fake/stringMap-reference';
  static String method = r'POST';

  String get contentType;
  final Map<String, String> extraHeaders;
  final Map<String, String> extraCookies;
  final Map<String, Object /* String | List<String> */> extraQueryParameters;

  

  const factory FakeApiTestStringMapReferenceRequest.unsafe({
    
    Map<String, String> extraHeaders,
    Map<String, Object> extraQueryParameters,
    Map<String, String> extraCookies,
    Stream<Uint8List>? body,
  }) = FakeApiTestStringMapReferenceRequestUnsafe;

  const FakeApiTestStringMapReferenceRequest({
    
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

/// A version of [FakeApiTestStringMapReferenceRequest], where you can send arbitrary bytes in the body.
class FakeApiTestStringMapReferenceRequestUnsafe extends FakeApiTestStringMapReferenceRequest {
  final Stream<Uint8List>? body;

  @override
  final String contentType;

  const FakeApiTestStringMapReferenceRequestUnsafe({
    this.body,
    this.contentType = 'application/octet-stream',
    
    super.extraHeaders,
    super.extraQueryParameters,
    super.extraCookies,
  });

  Stream<List<int>> getResolvedBody({
    required Map<String, String> headers,
    Map<String, dynamic> context = const {},
  }) async* {
    final body = this.body;
    if (body == null) {
      return;
    }
    yield* body;
  }
}




class FakeApiTestStringMapReferenceRequestApplicationJson extends FakeApiTestStringMapReferenceRequest {
  static const specMediaType = r'application/json';

  @override
  String get contentType => specMediaType;

  final 
    Map<String, 
        
            String
>
 data;

  /// Pass this to handle serialization and encoding of unkown media types yourself.
  final UnknownMediaTypeHandler? handleUnkownMediaType;


  


  const FakeApiTestStringMapReferenceRequestApplicationJson({
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
    var serialized = v.map((k,v) => MapEntry(k, v));
    final charset = resolvedMediaType.parameters['charset'] ?? 'utf-8';
    final encoding = Encoding.getByName(charset) ?? utf8;
    Stream<List<int>> _stringResult(String src) {
      return encoding.encoder.bind(Stream.value(src));
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
        if (handleUnkownMediaType!=null) {
          yield* handleUnkownMediaType(resolvedMediaType, serialized, encoding, encodingRules);
          return;
        }
        yield* _stringResult(serialized.toString());
    }
  }
}


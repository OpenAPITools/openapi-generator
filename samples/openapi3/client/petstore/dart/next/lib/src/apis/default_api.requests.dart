// ignore_for_file: unnecessary_type_check

part of 'default_api.dart';





 class DefaultApiFakeAnyOfWIthSameErasureGetRequest {
  static const pathTemplate = r'/fake/anyOfWIthSameErasure';
  static String method = r'GET';

  final Map<String, String> extraHeaders;
  final Map<String, String> extraCookies;
  final Map<String, Object /* String | List<String> */> extraQueryParameters;



  const DefaultApiFakeAnyOfWIthSameErasureGetRequest({
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







 class DefaultApiFakeOneOfWIthSameErasureGetRequest {
  static const pathTemplate = r'/fake/oneOfWIthSameErasure';
  static String method = r'GET';

  final Map<String, String> extraHeaders;
  final Map<String, String> extraCookies;
  final Map<String, Object /* String | List<String> */> extraQueryParameters;



  const DefaultApiFakeOneOfWIthSameErasureGetRequest({
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







 class DefaultApiFooGetRequest {
  static const pathTemplate = r'/foo';
  static String method = r'GET';

  final Map<String, String> extraHeaders;
  final Map<String, String> extraCookies;
  final Map<String, Object /* String | List<String> */> extraQueryParameters;



  const DefaultApiFooGetRequest({
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







abstract class DefaultApiPetsMulticontentTestPostRequest {
  static const pathTemplate = r'/pets/multicontent-test';
  static String method = r'POST';

  String get contentType;
  final Map<String, String> extraHeaders;
  final Map<String, String> extraCookies;
  final Map<String, Object /* String | List<String> */> extraQueryParameters;

  

  const factory DefaultApiPetsMulticontentTestPostRequest.unsafe({
    
    Map<String, String> extraHeaders,
    Map<String, Object> extraQueryParameters,
    Map<String, String> extraCookies,
    Stream<Uint8List>? bodyBytesStream,
  }) = DefaultApiPetsMulticontentTestPostRequestUnsafe;

  
  const factory DefaultApiPetsMulticontentTestPostRequest.applicationJson({
    required 
            Pet
 data,
    UnknownMediaTypeHandler? handleUnkownMediaType,
    
    
    Map<String, String> extraHeaders,
    Map<String, Object> extraQueryParameters,
    Map<String, String> extraCookies,
  }) = DefaultApiPetsMulticontentTestPostRequestApplicationJson;
  
  const factory DefaultApiPetsMulticontentTestPostRequest.applicationXml({
    required 
            NewPet
 data,
    UnknownMediaTypeHandler? handleUnkownMediaType,
    
    
    Map<String, String> extraHeaders,
    Map<String, Object> extraQueryParameters,
    Map<String, String> extraCookies,
  }) = DefaultApiPetsMulticontentTestPostRequestApplicationXml;
  
  const factory DefaultApiPetsMulticontentTestPostRequest.applicationXWwwFormUrlencoded({
    required 
            Triangle
 data,
    UnknownMediaTypeHandler? handleUnkownMediaType,
    
    
    Map<String, String> extraHeaders,
    Map<String, Object> extraQueryParameters,
    Map<String, String> extraCookies,
  }) = DefaultApiPetsMulticontentTestPostRequestApplicationXWwwFormUrlencoded;
  
  const factory DefaultApiPetsMulticontentTestPostRequest.textPlain({
    required 
            int
 data,
    UnknownMediaTypeHandler? handleUnkownMediaType,
    
    
    Map<String, String> extraHeaders,
    Map<String, Object> extraQueryParameters,
    Map<String, String> extraCookies,
  }) = DefaultApiPetsMulticontentTestPostRequestTextPlain;
  
  const factory DefaultApiPetsMulticontentTestPostRequest.anyAny({
    required Object
? data,
    UnknownMediaTypeHandler? handleUnkownMediaType,
    
    
    Map<String, String> extraHeaders,
    Map<String, Object> extraQueryParameters,
    Map<String, String> extraCookies,
  }) = DefaultApiPetsMulticontentTestPostRequestAnyAny;
  
  const factory DefaultApiPetsMulticontentTestPostRequest.textAny({
    required 
            String
 data,
    UnknownMediaTypeHandler? handleUnkownMediaType,
    
    
    Map<String, String> extraHeaders,
    Map<String, Object> extraQueryParameters,
    Map<String, String> extraCookies,
  }) = DefaultApiPetsMulticontentTestPostRequestTextAny;
  
  const factory DefaultApiPetsMulticontentTestPostRequest.multipartFormData({
    required 
            PetsMulticontentTestPostRequest
 data,
    UnknownMediaTypeHandler? handleUnkownMediaType,
    AppendFormDataPartHandler? appendUnkownFormDataPart,
    
    UndefinedWrapper<
            String
> profileImagesHelloHeader,
    
    Map<String, String> extraHeaders,
    Map<String, Object> extraQueryParameters,
    Map<String, String> extraCookies,
  }) = DefaultApiPetsMulticontentTestPostRequestMultipartFormData;
  

  const DefaultApiPetsMulticontentTestPostRequest({
    
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




class DefaultApiPetsMulticontentTestPostRequestApplicationJson extends DefaultApiPetsMulticontentTestPostRequest {
  static const specMediaType = r'application/json';

  @override
  String get contentType => specMediaType;

  final 
            Pet
 data;

  /// Pass this to handle serialization and encoding of unkown media types yourself.
  final UnknownMediaTypeHandler? handleUnkownMediaType;


  


  const DefaultApiPetsMulticontentTestPostRequestApplicationJson({
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
class DefaultApiPetsMulticontentTestPostRequestApplicationXml extends DefaultApiPetsMulticontentTestPostRequest {
  static const specMediaType = r'application/xml';

  @override
  String get contentType => specMediaType;

  final 
            NewPet
 data;

  /// Pass this to handle serialization and encoding of unkown media types yourself.
  final UnknownMediaTypeHandler? handleUnkownMediaType;


  


  const DefaultApiPetsMulticontentTestPostRequestApplicationXml({
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
class DefaultApiPetsMulticontentTestPostRequestApplicationXWwwFormUrlencoded extends DefaultApiPetsMulticontentTestPostRequest {
  static const specMediaType = r'application/x-www-form-urlencoded';

  @override
  String get contentType => specMediaType;

  final 
            Triangle
 data;

  /// Pass this to handle serialization and encoding of unkown media types yourself.
  final UnknownMediaTypeHandler? handleUnkownMediaType;


  


  const DefaultApiPetsMulticontentTestPostRequestApplicationXWwwFormUrlencoded({
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
      
      r'name': PropertyEncodingRule(
        style: r'form',
        explode: true,
        
        
        
      ),
      
    };

    // Since the user can override mime type at runtime, we need to check the
    // mime type and serialize the model accordingly.
    switch (resolvedMediaType) {
      case MediaType(type: 'application', subtype: 'x-www-form-urlencoded'):
        if (serialized is! Map<String, dynamic>) {
          yield* _stringResult(serialized.toString());
          return;
        }
        var result = Uri();

        for (var e in serialized.entries) {
          final rule = encodingRules[e.key];
          final style = rule?.style ?? 'form';
          final explode = rule?.explode ?? (style == 'form');
          result = OpenApiParameterSerializationQuery.fromStyle(style, explode: explode, parameterName: e.key, allowEmptyValue: false,).expandUri(result, e.value);
        }
        var resultString = result.query.toString();
        if (resultString.startsWith('?')) {
          resultString = resultString.substring(1);
        }
        yield* _stringResult(resultString);
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
class DefaultApiPetsMulticontentTestPostRequestTextPlain extends DefaultApiPetsMulticontentTestPostRequest {
  static const specMediaType = r'text/plain';

  @override
  String get contentType => specMediaType;

  final 
            int
 data;

  /// Pass this to handle serialization and encoding of unkown media types yourself.
  final UnknownMediaTypeHandler? handleUnkownMediaType;


  


  const DefaultApiPetsMulticontentTestPostRequestTextPlain({
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
    final encoding = OASNetworkingUtils.getEncodingOrDefault(resolvedMediaType);
    Stream<List<int>> _stringResult(String src) {
      return Stream.value(encoding.encode(src));
    }
    final encodingRules = <String, PropertyEncodingRule>{
      
    };

    // Since the user can override mime type at runtime, we need to check the
    // mime type and serialize the model accordingly.
    switch (resolvedMediaType) {
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
class DefaultApiPetsMulticontentTestPostRequestAnyAny extends DefaultApiPetsMulticontentTestPostRequest {
  static const specMediaType = r'*/*';

  @override
  String get contentType => specMediaType;

  final Object
? data;

  /// Pass this to handle serialization and encoding of unkown media types yourself.
  final UnknownMediaTypeHandler? handleUnkownMediaType;


  


  const DefaultApiPetsMulticontentTestPostRequestAnyAny({
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
    final encoding = OASNetworkingUtils.getEncodingOrDefault(resolvedMediaType);
    Stream<List<int>> _stringResult(String src) {
      return Stream.value(encoding.encode(src));
    }
    final encodingRules = <String, PropertyEncodingRule>{
      
    };

    // Since the user can override mime type at runtime, we need to check the
    // mime type and serialize the model accordingly.
    switch (resolvedMediaType) {
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
class DefaultApiPetsMulticontentTestPostRequestTextAny extends DefaultApiPetsMulticontentTestPostRequest {
  static const specMediaType = r'text/*';

  @override
  String get contentType => specMediaType;

  final 
            String
 data;

  /// Pass this to handle serialization and encoding of unkown media types yourself.
  final UnknownMediaTypeHandler? handleUnkownMediaType;


  


  const DefaultApiPetsMulticontentTestPostRequestTextAny({
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
    final encoding = OASNetworkingUtils.getEncodingOrDefault(resolvedMediaType);
    Stream<List<int>> _stringResult(String src) {
      return Stream.value(encoding.encode(src));
    }
    final encodingRules = <String, PropertyEncodingRule>{
      
    };

    // Since the user can override mime type at runtime, we need to check the
    // mime type and serialize the model accordingly.
    switch (resolvedMediaType) {
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
class DefaultApiPetsMulticontentTestPostRequestMultipartFormData extends DefaultApiPetsMulticontentTestPostRequest {
  static const specMediaType = r'multipart/form-data';

  @override
  String get contentType => specMediaType;

  final 
            PetsMulticontentTestPostRequest
 data;

  /// Pass this to handle serialization and encoding of unkown media types yourself.
  final UnknownMediaTypeHandler? handleUnkownMediaType;

  /// Pass this to handle serialization of unkown form data values yourself.
  /// A value can result in one or more parts.
  /// returning an empty iterable will skip the field entirely.
  ///
  /// The default behavior is to call [defaultAppendFormDataPart] which will call `.toString`
  /// to stringify the value.
  final AppendFormDataPartHandler? appendUnkownFormDataPart;

  

  final UndefinedWrapper<
            String
> profileImagesHelloHeader;

  const DefaultApiPetsMulticontentTestPostRequestMultipartFormData({
    required this.data,
    this.handleUnkownMediaType,
     this.profileImagesHelloHeader= const UndefinedWrapper
        .undefined()
,
    
    this.appendUnkownFormDataPart,
    
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

    // Since the user can override mime type at runtime, we need to check the
    // mime type and serialize the model accordingly.
    switch (resolvedMediaType) {
      case MediaType(type: 'multipart'):
        if (serialized is! Map<String, dynamic>) {
          throw ArgumentError('The serialized data must be a map in a multipart request.');
        }

        Stream<HttpPacketMixin> getParts() async* {
          switch (resolvedMediaType.subtype) {
            case 'form-data':
              for (final e in serialized.entries) {
                final fieldName = e.key;
                final rule = encodingRules[e.key];
                final value = e.value;
                final contentType = rule?.contentType;
                final headers = rule?.headers;
                yield* appendFormDataValue(fieldName, value, contentType, headers, unkownHandler: this.appendUnkownFormDataPart ?? defaultAppendFormDataPart);
              }
            default:
            
          }
        }

        final bodySerializer = MultiPartBodySerializer(
          boundary: resolvedMediaType.parameters['boundary'],
          parts: getParts(),
        );
        final result = await bodySerializer.serialize();
        yield* result.bodyBytesStream;
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


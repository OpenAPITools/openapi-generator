// ignore_for_file: unnecessary_type_check

part of 'user_api.dart';








abstract class UserApiCreateUserRequest {
  static const pathTemplate = r'/user';
  static String method = r'POST';

  String get contentType;
  final Map<String, String> extraHeaders;
  final Map<String, String> extraCookies;
  final Map<String, Object /* String | List<String> */> extraQueryParameters;
  final WireSerializationOptions wireSerializationOptions;
  

  const factory UserApiCreateUserRequest.unsafe({
    
    Map<String, String> extraHeaders,
    Map<String, Object> extraQueryParameters,
    Map<String, String> extraCookies,
    WireSerializationOptions wireSerializationOptions,
    Stream<Uint8List>? bodyBytesStream,
  }) = UserApiCreateUserRequestUnsafe;

  
  const factory UserApiCreateUserRequest.applicationJson({
    required 
            User
 data,
    
    WireSerializationOptions wireSerializationOptions,
    Map<String, String> extraHeaders,
    Map<String, Object> extraQueryParameters,
    Map<String, String> extraCookies,
  }) = UserApiCreateUserRequestApplicationJson;
  

  const UserApiCreateUserRequest({
    
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

/// A version of [UserApiCreateUserRequest], where you can send arbitrary bytes in the body.
class UserApiCreateUserRequestUnsafe extends UserApiCreateUserRequest {
  final Stream<Uint8List>? bodyBytesStream;

  @override
  final String contentType;

  const UserApiCreateUserRequestUnsafe({
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






class UserApiCreateUserRequestApplicationJson extends UserApiCreateUserRequest {
  static const specMediaType = r'application/json';

  @override
  String get contentType => specMediaType;

  final 
            User
 data;
  static const dataReflection = XmlReflectionWrapper(
    xml: XmlReflection(
    xmlName: r'User',
),
    
            
        
        
            
                User.$reflection
        
,
)
;


  const UserApiCreateUserRequestApplicationJson({
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










abstract class UserApiCreateUsersWithArrayInputRequest {
  static const pathTemplate = r'/user/createWithArray';
  static String method = r'POST';

  String get contentType;
  final Map<String, String> extraHeaders;
  final Map<String, String> extraCookies;
  final Map<String, Object /* String | List<String> */> extraQueryParameters;
  final WireSerializationOptions wireSerializationOptions;
  

  const factory UserApiCreateUsersWithArrayInputRequest.unsafe({
    
    Map<String, String> extraHeaders,
    Map<String, Object> extraQueryParameters,
    Map<String, String> extraCookies,
    WireSerializationOptions wireSerializationOptions,
    Stream<Uint8List>? bodyBytesStream,
  }) = UserApiCreateUsersWithArrayInputRequestUnsafe;

  
  const factory UserApiCreateUsersWithArrayInputRequest.applicationJson({
    required 
    List<
        
            User
>
 data,
    
    WireSerializationOptions wireSerializationOptions,
    Map<String, String> extraHeaders,
    Map<String, Object> extraQueryParameters,
    Map<String, String> extraCookies,
  }) = UserApiCreateUsersWithArrayInputRequestApplicationJson;
  

  const UserApiCreateUsersWithArrayInputRequest({
    
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

/// A version of [UserApiCreateUsersWithArrayInputRequest], where you can send arbitrary bytes in the body.
class UserApiCreateUsersWithArrayInputRequestUnsafe extends UserApiCreateUsersWithArrayInputRequest {
  final Stream<Uint8List>? bodyBytesStream;

  @override
  final String contentType;

  const UserApiCreateUsersWithArrayInputRequestUnsafe({
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






class UserApiCreateUsersWithArrayInputRequestApplicationJson extends UserApiCreateUsersWithArrayInputRequest {
  static const specMediaType = r'application/json';

  @override
  String get contentType => specMediaType;

  final 
    List<
        
            User
>
 data;
  static const dataReflection = XmlReflectionWrapper(
    xml: XmlReflection(
),
    
    ListReflection(XmlReflectionWrapper(
    xml: XmlReflection(
    xmlName: r'User',
),
    
            
        
        
            
                User.$reflection
        
,
)
)
,
)
;


  const UserApiCreateUsersWithArrayInputRequestApplicationJson({
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










abstract class UserApiCreateUsersWithListInputRequest {
  static const pathTemplate = r'/user/createWithList';
  static String method = r'POST';

  String get contentType;
  final Map<String, String> extraHeaders;
  final Map<String, String> extraCookies;
  final Map<String, Object /* String | List<String> */> extraQueryParameters;
  final WireSerializationOptions wireSerializationOptions;
  

  const factory UserApiCreateUsersWithListInputRequest.unsafe({
    
    Map<String, String> extraHeaders,
    Map<String, Object> extraQueryParameters,
    Map<String, String> extraCookies,
    WireSerializationOptions wireSerializationOptions,
    Stream<Uint8List>? bodyBytesStream,
  }) = UserApiCreateUsersWithListInputRequestUnsafe;

  
  const factory UserApiCreateUsersWithListInputRequest.applicationJson({
    required 
    List<
        
            User
>
 data,
    
    WireSerializationOptions wireSerializationOptions,
    Map<String, String> extraHeaders,
    Map<String, Object> extraQueryParameters,
    Map<String, String> extraCookies,
  }) = UserApiCreateUsersWithListInputRequestApplicationJson;
  

  const UserApiCreateUsersWithListInputRequest({
    
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

/// A version of [UserApiCreateUsersWithListInputRequest], where you can send arbitrary bytes in the body.
class UserApiCreateUsersWithListInputRequestUnsafe extends UserApiCreateUsersWithListInputRequest {
  final Stream<Uint8List>? bodyBytesStream;

  @override
  final String contentType;

  const UserApiCreateUsersWithListInputRequestUnsafe({
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






class UserApiCreateUsersWithListInputRequestApplicationJson extends UserApiCreateUsersWithListInputRequest {
  static const specMediaType = r'application/json';

  @override
  String get contentType => specMediaType;

  final 
    List<
        
            User
>
 data;
  static const dataReflection = XmlReflectionWrapper(
    xml: XmlReflection(
),
    
    ListReflection(XmlReflectionWrapper(
    xml: XmlReflection(
    xmlName: r'User',
),
    
            
        
        
            
                User.$reflection
        
,
)
)
,
)
;


  const UserApiCreateUsersWithListInputRequestApplicationJson({
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








 class UserApiDeleteUserRequest {
  static const pathTemplate = r'/user/{username}';
  static String method = r'DELETE';

  final Map<String, String> extraHeaders;
  final Map<String, String> extraCookies;
  final Map<String, Object /* String | List<String> */> extraQueryParameters;
  final WireSerializationOptions wireSerializationOptions;
  
  /// The name that needs to be deleted
  /// spec name: username
  final 
            String
 username;
  


  const UserApiDeleteUserRequest({
    
    required this.username    ,
    
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
    resolvedPath = OpenApiParameterSerializationPath.fromStyle(r'simple', explode: false, parameterName: r'username',).expand(resolvedPath, username);
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










 class UserApiGetUserByNameRequest {
  static const pathTemplate = r'/user/{username}';
  static String method = r'GET';

  final Map<String, String> extraHeaders;
  final Map<String, String> extraCookies;
  final Map<String, Object /* String | List<String> */> extraQueryParameters;
  final WireSerializationOptions wireSerializationOptions;
  
  /// The name that needs to be fetched. Use user1 for testing.
  /// spec name: username
  final 
            String
 username;
  


  const UserApiGetUserByNameRequest({
    
    required this.username    ,
    
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
    resolvedPath = OpenApiParameterSerializationPath.fromStyle(r'simple', explode: false, parameterName: r'username',).expand(resolvedPath, username);
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














 class UserApiLoginUserRequest {
  static const pathTemplate = r'/user/login';
  static String method = r'GET';

  final Map<String, String> extraHeaders;
  final Map<String, String> extraCookies;
  final Map<String, Object /* String | List<String> */> extraQueryParameters;
  final WireSerializationOptions wireSerializationOptions;
  
  /// The user name for login
  /// spec name: username
  final 
            String
 username;
  
  
  /// The password for login in clear text
  /// spec name: password
  final 
            String
 password;
  


  const UserApiLoginUserRequest({
    
    required this.username    ,
    
    
    required this.password    ,
    
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
      methodUri = OpenApiParameterSerializationQuery.fromStyle(r'form', explode: true, parameterName: r'username', allowEmptyValue: false,).expandUri(methodUri, username);
      methodUri = OpenApiParameterSerializationQuery.fromStyle(r'form', explode: true, parameterName: r'password', allowEmptyValue: false,).expandUri(methodUri, password);

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






 class UserApiLogoutUserRequest {
  static const pathTemplate = r'/user/logout';
  static String method = r'GET';

  final Map<String, String> extraHeaders;
  final Map<String, String> extraCookies;
  final Map<String, Object /* String | List<String> */> extraQueryParameters;
  final WireSerializationOptions wireSerializationOptions;


  const UserApiLogoutUserRequest({
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














abstract class UserApiUpdateUserRequest {
  static const pathTemplate = r'/user/{username}';
  static String method = r'PUT';

  String get contentType;
  final Map<String, String> extraHeaders;
  final Map<String, String> extraCookies;
  final Map<String, Object /* String | List<String> */> extraQueryParameters;
  final WireSerializationOptions wireSerializationOptions;
  
  /// name that need to be deleted
  /// spec name: username
  final 
            String
 username;
  
  

  const factory UserApiUpdateUserRequest.unsafe({
    
    required 
            String
 username,
    
    Map<String, String> extraHeaders,
    Map<String, Object> extraQueryParameters,
    Map<String, String> extraCookies,
    WireSerializationOptions wireSerializationOptions,
    Stream<Uint8List>? bodyBytesStream,
  }) = UserApiUpdateUserRequestUnsafe;

  
  const factory UserApiUpdateUserRequest.applicationJson({
    required 
            User
 data,
    
    required 
            String
 username,
    
    
    WireSerializationOptions wireSerializationOptions,
    Map<String, String> extraHeaders,
    Map<String, Object> extraQueryParameters,
    Map<String, String> extraCookies,
  }) = UserApiUpdateUserRequestApplicationJson;
  

  const UserApiUpdateUserRequest({
    
    required this.username    ,
    
    
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
    resolvedPath = OpenApiParameterSerializationPath.fromStyle(r'simple', explode: false, parameterName: r'username',).expand(resolvedPath, username);
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

/// A version of [UserApiUpdateUserRequest], where you can send arbitrary bytes in the body.
class UserApiUpdateUserRequestUnsafe extends UserApiUpdateUserRequest {
  final Stream<Uint8List>? bodyBytesStream;

  @override
  final String contentType;

  const UserApiUpdateUserRequestUnsafe({
    this.bodyBytesStream,
    this.contentType = 'application/octet-stream',
    
    required super.username,
    
    
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






class UserApiUpdateUserRequestApplicationJson extends UserApiUpdateUserRequest {
  static const specMediaType = r'application/json';

  @override
  String get contentType => specMediaType;

  final 
            User
 data;
  static const dataReflection = XmlReflectionWrapper(
    xml: XmlReflection(
    xmlName: r'User',
),
    
            
        
        
            
                User.$reflection
        
,
)
;


  const UserApiUpdateUserRequestApplicationJson({
    required this.data,
    
    required super.username,
    
    
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


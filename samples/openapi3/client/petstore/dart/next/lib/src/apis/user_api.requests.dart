// ignore_for_file: unnecessary_type_check

part of 'user_api.dart';





abstract class UserApiCreateUserRequest {
  static const pathTemplate = r'/user';
  static String method = r'POST';

  String get contentType;
  final Map<String, String> extraHeaders;
  final Map<String, String> extraCookies;
  final Map<String, Object /* String | List<String> */> extraQueryParameters;

  

  const factory UserApiCreateUserRequest.unsafe({
    
    Map<String, String> extraHeaders,
    Map<String, Object> extraQueryParameters,
    Map<String, String> extraCookies,
    Stream<Uint8List>? bodyBytesStream,
  }) = UserApiCreateUserRequestUnsafe;

  
  const factory UserApiCreateUserRequest.applicationJson({
    required 
            User

 data,
    UnknownMediaTypeHandler? handleUnkownMediaType,
    
    
    Map<String, String> extraHeaders,
    Map<String, Object> extraQueryParameters,
    Map<String, String> extraCookies,
  }) = UserApiCreateUserRequestApplicationJson;
  

  const UserApiCreateUserRequest({
    
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




class UserApiCreateUserRequestApplicationJson extends UserApiCreateUserRequest {
  static const specMediaType = r'application/json';

  @override
  String get contentType => specMediaType;

  final 
            User

 data;

  /// Pass this to handle serialization and encoding of unkown media types yourself.
  final UnknownMediaTypeHandler? handleUnkownMediaType;


  


  const UserApiCreateUserRequestApplicationJson({
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





abstract class UserApiCreateUsersWithArrayInputRequest {
  static const pathTemplate = r'/user/createWithArray';
  static String method = r'POST';

  String get contentType;
  final Map<String, String> extraHeaders;
  final Map<String, String> extraCookies;
  final Map<String, Object /* String | List<String> */> extraQueryParameters;

  

  const factory UserApiCreateUsersWithArrayInputRequest.unsafe({
    
    Map<String, String> extraHeaders,
    Map<String, Object> extraQueryParameters,
    Map<String, String> extraCookies,
    Stream<Uint8List>? bodyBytesStream,
  }) = UserApiCreateUsersWithArrayInputRequestUnsafe;

  
  const factory UserApiCreateUsersWithArrayInputRequest.applicationJson({
    required 
    List<
        
            User

>

 data,
    UnknownMediaTypeHandler? handleUnkownMediaType,
    
    
    Map<String, String> extraHeaders,
    Map<String, Object> extraQueryParameters,
    Map<String, String> extraCookies,
  }) = UserApiCreateUsersWithArrayInputRequestApplicationJson;
  

  const UserApiCreateUsersWithArrayInputRequest({
    
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




class UserApiCreateUsersWithArrayInputRequestApplicationJson extends UserApiCreateUsersWithArrayInputRequest {
  static const specMediaType = r'application/json';

  @override
  String get contentType => specMediaType;

  final 
    List<
        
            User

>

 data;

  /// Pass this to handle serialization and encoding of unkown media types yourself.
  final UnknownMediaTypeHandler? handleUnkownMediaType;


  


  const UserApiCreateUsersWithArrayInputRequestApplicationJson({
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
    var serialized = v.map((v) => v.serialize()).toList();
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





abstract class UserApiCreateUsersWithListInputRequest {
  static const pathTemplate = r'/user/createWithList';
  static String method = r'POST';

  String get contentType;
  final Map<String, String> extraHeaders;
  final Map<String, String> extraCookies;
  final Map<String, Object /* String | List<String> */> extraQueryParameters;

  

  const factory UserApiCreateUsersWithListInputRequest.unsafe({
    
    Map<String, String> extraHeaders,
    Map<String, Object> extraQueryParameters,
    Map<String, String> extraCookies,
    Stream<Uint8List>? bodyBytesStream,
  }) = UserApiCreateUsersWithListInputRequestUnsafe;

  
  const factory UserApiCreateUsersWithListInputRequest.applicationJson({
    required 
    List<
        
            User

>

 data,
    UnknownMediaTypeHandler? handleUnkownMediaType,
    
    
    Map<String, String> extraHeaders,
    Map<String, Object> extraQueryParameters,
    Map<String, String> extraCookies,
  }) = UserApiCreateUsersWithListInputRequestApplicationJson;
  

  const UserApiCreateUsersWithListInputRequest({
    
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




class UserApiCreateUsersWithListInputRequestApplicationJson extends UserApiCreateUsersWithListInputRequest {
  static const specMediaType = r'application/json';

  @override
  String get contentType => specMediaType;

  final 
    List<
        
            User

>

 data;

  /// Pass this to handle serialization and encoding of unkown media types yourself.
  final UnknownMediaTypeHandler? handleUnkownMediaType;


  


  const UserApiCreateUsersWithListInputRequestApplicationJson({
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
    var serialized = v.map((v) => v.serialize()).toList();
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





 class UserApiDeleteUserRequest {
  static const pathTemplate = r'/user/{username}';
  static String method = r'DELETE';

  final Map<String, String> extraHeaders;
  final Map<String, String> extraCookies;
  final Map<String, Object /* String | List<String> */> extraQueryParameters;

  
  /// The name that needs to be deleted
  /// spec name: username
  final 
            String

 username;
  


  const UserApiDeleteUserRequest({
    
    required this.username    ,
    
    this.extraHeaders = const {},
    this.extraQueryParameters = const {},
    this.extraCookies = const {},
  });

  Future<Uri> getResolvedUri({
    required Uri baseUrl,
    Map<String, dynamic> context = const {},
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







 class UserApiGetUserByNameRequest {
  static const pathTemplate = r'/user/{username}';
  static String method = r'GET';

  final Map<String, String> extraHeaders;
  final Map<String, String> extraCookies;
  final Map<String, Object /* String | List<String> */> extraQueryParameters;

  
  /// The name that needs to be fetched. Use user1 for testing.
  /// spec name: username
  final 
            String

 username;
  


  const UserApiGetUserByNameRequest({
    
    required this.username    ,
    
    this.extraHeaders = const {},
    this.extraQueryParameters = const {},
    this.extraCookies = const {},
  });

  Future<Uri> getResolvedUri({
    required Uri baseUrl,
    Map<String, dynamic> context = const {},
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







 class UserApiLoginUserRequest {
  static const pathTemplate = r'/user/login';
  static String method = r'GET';

  final Map<String, String> extraHeaders;
  final Map<String, String> extraCookies;
  final Map<String, Object /* String | List<String> */> extraQueryParameters;

  
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







 class UserApiLogoutUserRequest {
  static const pathTemplate = r'/user/logout';
  static String method = r'GET';

  final Map<String, String> extraHeaders;
  final Map<String, String> extraCookies;
  final Map<String, Object /* String | List<String> */> extraQueryParameters;



  const UserApiLogoutUserRequest({
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







abstract class UserApiUpdateUserRequest {
  static const pathTemplate = r'/user/{username}';
  static String method = r'PUT';

  String get contentType;
  final Map<String, String> extraHeaders;
  final Map<String, String> extraCookies;
  final Map<String, Object /* String | List<String> */> extraQueryParameters;

  
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
    Stream<Uint8List>? bodyBytesStream,
  }) = UserApiUpdateUserRequestUnsafe;

  
  const factory UserApiUpdateUserRequest.applicationJson({
    required 
            User

 data,
    UnknownMediaTypeHandler? handleUnkownMediaType,
    
    
    required 
            String

 username,
    
    
    Map<String, String> extraHeaders,
    Map<String, Object> extraQueryParameters,
    Map<String, String> extraCookies,
  }) = UserApiUpdateUserRequestApplicationJson;
  

  const UserApiUpdateUserRequest({
    
    required this.username    ,
    
    
    this.extraHeaders = const {},
    this.extraQueryParameters = const {},
    this.extraCookies = const {},
  });

  Future<Uri> getResolvedUri({
    required Uri baseUrl,
    Map<String, dynamic> context = const {},
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




class UserApiUpdateUserRequestApplicationJson extends UserApiUpdateUserRequest {
  static const specMediaType = r'application/json';

  @override
  String get contentType => specMediaType;

  final 
            User

 data;

  /// Pass this to handle serialization and encoding of unkown media types yourself.
  final UnknownMediaTypeHandler? handleUnkownMediaType;


  


  const UserApiUpdateUserRequestApplicationJson({
    required this.data,
    this.handleUnkownMediaType,
    
    required super.username,
    
    
    
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


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
    Stream<Uint8List>? body,
  }) = UserApiCreateUserRequestUnsafe;

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

    return CaseInsensitiveMap<String>.from(<String,String>{
      'Content-Type': contentType,
      if (cookieParts.isNotEmpty)
        'Cookie': cookieParts.entries.map((e) => '${e.key}=${e.value}').join('; '),
      ...extraHeaders,
    });
  }


  Stream<List<int>> getResolvedBody({
    required MediaType resolvedMediaType,
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
    final contentType = headers['Content-Type']!;
    final parsedContentType = MediaType.parse(contentType).fillDefaults();
    return HttpRequestBase.stream(
      url: futureResults[0] as Uri,
      headers: headers,
      method: method,
      bodyBytesStream: getResolvedBody(context: context, resolvedMediaType: parsedContentType),
      context: context,
    );
  }
}

/// A version of [UserApiCreateUserRequest], where you can send arbitrary bytes in the body.
class UserApiCreateUserRequestUnsafe extends UserApiCreateUserRequest {
  final Stream<Uint8List>? body;

  @override
  final String contentType;

  const UserApiCreateUserRequestUnsafe({
    this.body,
    this.contentType = 'application/octet-stream',
    
    super.extraHeaders,
    super.extraQueryParameters,
    super.extraCookies,
  });

  Stream<List<int>> getResolvedBody({
    required MediaType resolvedMediaType,
    Map<String, dynamic> context = const {},
  }) async* {
    final body = this.body;
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

  const UserApiCreateUserRequestApplicationJson({
    required this.data,
    
    super.extraHeaders,
    super.extraQueryParameters,
    super.extraCookies,
  });

  @override
  Stream<List<int>> getResolvedBody({
    required MediaType resolvedMediaType,
    Map<String, dynamic> context = const {},
  }) {
    //TODO: serialize model, then encode it according to media type.
    final v = data;
    var serialized = v.serialize();
    final charset = resolvedMediaType.parameters['charset'] ?? 'utf8';
    final encoding = Encoding.getByName(charset) ?? utf8;
    Stream<List<int>> _stringResult(String src) {
      return encoding.encoder.bind(Stream.value(src));
    }
    // Since the user can override mime type at runtime, we need to check the
    // mime type and serialize the model accordingly.
    switch (resolvedMediaType) {
      case MediaType(type: 'application', subtype: 'json'):
        return _stringResult(json.encode(serialized));
      case MediaType(type: 'application', subtype: 'x-www-form-urlencoded'):
        var serialized = v.serialize();
        //_stringResult();
        break;
      case MediaType(type: 'application', subtype: 'xml'):
        break;
      case MediaType(type: 'application', subtype: 'octet-stream'):
        break;
      case MediaType(type: 'multipart'):
        List<HttpPacketMixin> parts;
        if (resolvedMediaType.subtype == 'form-data') {
          //final memberEncodings = ;
          parts = MultiPartBodySerializer.getFormDataParts(
            fields: {
            },
            files: []
          );
        } else {
          parts = [];
        }
        final bodySerializer = MultiPartBodySerializer(
          parts: parts,
        );
        return bodySerializer.bodyBytesStream;
        break;
      default:

    }
    //var serialized = v.serialize();
    // serialized is guaranteed to be a dart primitive (String, int, List, Map, Uint8List, XFile, XMLElement, etc...)
    //final encoded = json.encode(serialized);
    //final bytes = ;
  }

}


class UserApiCreateUserResponse {
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
    Stream<Uint8List>? body,
  }) = UserApiCreateUsersWithArrayInputRequestUnsafe;

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

    return CaseInsensitiveMap<String>.from(<String,String>{
      'Content-Type': contentType,
      if (cookieParts.isNotEmpty)
        'Cookie': cookieParts.entries.map((e) => '${e.key}=${e.value}').join('; '),
      ...extraHeaders,
    });
  }


  Stream<List<int>> getResolvedBody({
    required MediaType resolvedMediaType,
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
    final contentType = headers['Content-Type']!;
    final parsedContentType = MediaType.parse(contentType).fillDefaults();
    return HttpRequestBase.stream(
      url: futureResults[0] as Uri,
      headers: headers,
      method: method,
      bodyBytesStream: getResolvedBody(context: context, resolvedMediaType: parsedContentType),
      context: context,
    );
  }
}

/// A version of [UserApiCreateUsersWithArrayInputRequest], where you can send arbitrary bytes in the body.
class UserApiCreateUsersWithArrayInputRequestUnsafe extends UserApiCreateUsersWithArrayInputRequest {
  final Stream<Uint8List>? body;

  @override
  final String contentType;

  const UserApiCreateUsersWithArrayInputRequestUnsafe({
    this.body,
    this.contentType = 'application/octet-stream',
    
    super.extraHeaders,
    super.extraQueryParameters,
    super.extraCookies,
  });

  Stream<List<int>> getResolvedBody({
    required MediaType resolvedMediaType,
    Map<String, dynamic> context = const {},
  }) async* {
    final body = this.body;
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

  const UserApiCreateUsersWithArrayInputRequestApplicationJson({
    required this.data,
    
    super.extraHeaders,
    super.extraQueryParameters,
    super.extraCookies,
  });

  @override
  Stream<List<int>> getResolvedBody({
    required MediaType resolvedMediaType,
    Map<String, dynamic> context = const {},
  }) {
    //TODO: serialize model, then encode it according to media type.
    final v = data;
    var serialized = v.map((v) => v.serialize()).toList();
    final charset = resolvedMediaType.parameters['charset'] ?? 'utf8';
    final encoding = Encoding.getByName(charset) ?? utf8;
    Stream<List<int>> _stringResult(String src) {
      return encoding.encoder.bind(Stream.value(src));
    }
    // Since the user can override mime type at runtime, we need to check the
    // mime type and serialize the model accordingly.
    switch (resolvedMediaType) {
      case MediaType(type: 'application', subtype: 'json'):
        return _stringResult(json.encode(serialized));
      case MediaType(type: 'application', subtype: 'x-www-form-urlencoded'):
        var serialized = v.map((v) => v.serialize()).toList();
        //_stringResult();
        break;
      case MediaType(type: 'application', subtype: 'xml'):
        break;
      case MediaType(type: 'application', subtype: 'octet-stream'):
        break;
      case MediaType(type: 'multipart'):
        List<HttpPacketMixin> parts;
        if (resolvedMediaType.subtype == 'form-data') {
          //final memberEncodings = ;
          parts = MultiPartBodySerializer.getFormDataParts(
            fields: {
            },
            files: []
          );
        } else {
          parts = [];
        }
        final bodySerializer = MultiPartBodySerializer(
          parts: parts,
        );
        return bodySerializer.bodyBytesStream;
        break;
      default:

    }
    //var serialized = v.map((v) => v.serialize()).toList();
    // serialized is guaranteed to be a dart primitive (String, int, List, Map, Uint8List, XFile, XMLElement, etc...)
    //final encoded = json.encode(serialized);
    //final bytes = ;
  }

}


class UserApiCreateUsersWithArrayInputResponse {
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
    Stream<Uint8List>? body,
  }) = UserApiCreateUsersWithListInputRequestUnsafe;

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

    return CaseInsensitiveMap<String>.from(<String,String>{
      'Content-Type': contentType,
      if (cookieParts.isNotEmpty)
        'Cookie': cookieParts.entries.map((e) => '${e.key}=${e.value}').join('; '),
      ...extraHeaders,
    });
  }


  Stream<List<int>> getResolvedBody({
    required MediaType resolvedMediaType,
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
    final contentType = headers['Content-Type']!;
    final parsedContentType = MediaType.parse(contentType).fillDefaults();
    return HttpRequestBase.stream(
      url: futureResults[0] as Uri,
      headers: headers,
      method: method,
      bodyBytesStream: getResolvedBody(context: context, resolvedMediaType: parsedContentType),
      context: context,
    );
  }
}

/// A version of [UserApiCreateUsersWithListInputRequest], where you can send arbitrary bytes in the body.
class UserApiCreateUsersWithListInputRequestUnsafe extends UserApiCreateUsersWithListInputRequest {
  final Stream<Uint8List>? body;

  @override
  final String contentType;

  const UserApiCreateUsersWithListInputRequestUnsafe({
    this.body,
    this.contentType = 'application/octet-stream',
    
    super.extraHeaders,
    super.extraQueryParameters,
    super.extraCookies,
  });

  Stream<List<int>> getResolvedBody({
    required MediaType resolvedMediaType,
    Map<String, dynamic> context = const {},
  }) async* {
    final body = this.body;
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

  const UserApiCreateUsersWithListInputRequestApplicationJson({
    required this.data,
    
    super.extraHeaders,
    super.extraQueryParameters,
    super.extraCookies,
  });

  @override
  Stream<List<int>> getResolvedBody({
    required MediaType resolvedMediaType,
    Map<String, dynamic> context = const {},
  }) {
    //TODO: serialize model, then encode it according to media type.
    final v = data;
    var serialized = v.map((v) => v.serialize()).toList();
    final charset = resolvedMediaType.parameters['charset'] ?? 'utf8';
    final encoding = Encoding.getByName(charset) ?? utf8;
    Stream<List<int>> _stringResult(String src) {
      return encoding.encoder.bind(Stream.value(src));
    }
    // Since the user can override mime type at runtime, we need to check the
    // mime type and serialize the model accordingly.
    switch (resolvedMediaType) {
      case MediaType(type: 'application', subtype: 'json'):
        return _stringResult(json.encode(serialized));
      case MediaType(type: 'application', subtype: 'x-www-form-urlencoded'):
        var serialized = v.map((v) => v.serialize()).toList();
        //_stringResult();
        break;
      case MediaType(type: 'application', subtype: 'xml'):
        break;
      case MediaType(type: 'application', subtype: 'octet-stream'):
        break;
      case MediaType(type: 'multipart'):
        List<HttpPacketMixin> parts;
        if (resolvedMediaType.subtype == 'form-data') {
          //final memberEncodings = ;
          parts = MultiPartBodySerializer.getFormDataParts(
            fields: {
            },
            files: []
          );
        } else {
          parts = [];
        }
        final bodySerializer = MultiPartBodySerializer(
          parts: parts,
        );
        return bodySerializer.bodyBytesStream;
        break;
      default:

    }
    //var serialized = v.map((v) => v.serialize()).toList();
    // serialized is guaranteed to be a dart primitive (String, int, List, Map, Uint8List, XFile, XMLElement, etc...)
    //final encoded = json.encode(serialized);
    //final bytes = ;
  }

}


class UserApiCreateUsersWithListInputResponse {
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

    return CaseInsensitiveMap<String>.from(<String,String>{
      if (cookieParts.isNotEmpty)
        'Cookie': cookieParts.entries.map((e) => '${e.key}=${e.value}').join('; '),
      ...extraHeaders,
    });
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


class UserApiDeleteUserResponse {
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

    return CaseInsensitiveMap<String>.from(<String,String>{
      if (cookieParts.isNotEmpty)
        'Cookie': cookieParts.entries.map((e) => '${e.key}=${e.value}').join('; '),
      ...extraHeaders,
    });
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


class UserApiGetUserByNameResponse {
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

    return CaseInsensitiveMap<String>.from(<String,String>{
      if (cookieParts.isNotEmpty)
        'Cookie': cookieParts.entries.map((e) => '${e.key}=${e.value}').join('; '),
      ...extraHeaders,
    });
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


class UserApiLoginUserResponse {
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

    return CaseInsensitiveMap<String>.from(<String,String>{
      if (cookieParts.isNotEmpty)
        'Cookie': cookieParts.entries.map((e) => '${e.key}=${e.value}').join('; '),
      ...extraHeaders,
    });
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


class UserApiLogoutUserResponse {
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
    Stream<Uint8List>? body,
  }) = UserApiUpdateUserRequestUnsafe;

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

    return CaseInsensitiveMap<String>.from(<String,String>{
      'Content-Type': contentType,
      if (cookieParts.isNotEmpty)
        'Cookie': cookieParts.entries.map((e) => '${e.key}=${e.value}').join('; '),
      ...extraHeaders,
    });
  }


  Stream<List<int>> getResolvedBody({
    required MediaType resolvedMediaType,
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
    final contentType = headers['Content-Type']!;
    final parsedContentType = MediaType.parse(contentType).fillDefaults();
    return HttpRequestBase.stream(
      url: futureResults[0] as Uri,
      headers: headers,
      method: method,
      bodyBytesStream: getResolvedBody(context: context, resolvedMediaType: parsedContentType),
      context: context,
    );
  }
}

/// A version of [UserApiUpdateUserRequest], where you can send arbitrary bytes in the body.
class UserApiUpdateUserRequestUnsafe extends UserApiUpdateUserRequest {
  final Stream<Uint8List>? body;

  @override
  final String contentType;

  const UserApiUpdateUserRequestUnsafe({
    this.body,
    this.contentType = 'application/octet-stream',
    
    required super.username,
    
    
    super.extraHeaders,
    super.extraQueryParameters,
    super.extraCookies,
  });

  Stream<List<int>> getResolvedBody({
    required MediaType resolvedMediaType,
    Map<String, dynamic> context = const {},
  }) async* {
    final body = this.body;
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

  const UserApiUpdateUserRequestApplicationJson({
    required this.data,
    
    required super.username,
    
    
    super.extraHeaders,
    super.extraQueryParameters,
    super.extraCookies,
  });

  @override
  Stream<List<int>> getResolvedBody({
    required MediaType resolvedMediaType,
    Map<String, dynamic> context = const {},
  }) {
    //TODO: serialize model, then encode it according to media type.
    final v = data;
    var serialized = v.serialize();
    final charset = resolvedMediaType.parameters['charset'] ?? 'utf8';
    final encoding = Encoding.getByName(charset) ?? utf8;
    Stream<List<int>> _stringResult(String src) {
      return encoding.encoder.bind(Stream.value(src));
    }
    // Since the user can override mime type at runtime, we need to check the
    // mime type and serialize the model accordingly.
    switch (resolvedMediaType) {
      case MediaType(type: 'application', subtype: 'json'):
        return _stringResult(json.encode(serialized));
      case MediaType(type: 'application', subtype: 'x-www-form-urlencoded'):
        var serialized = v.serialize();
        //_stringResult();
        break;
      case MediaType(type: 'application', subtype: 'xml'):
        break;
      case MediaType(type: 'application', subtype: 'octet-stream'):
        break;
      case MediaType(type: 'multipart'):
        List<HttpPacketMixin> parts;
        if (resolvedMediaType.subtype == 'form-data') {
          //final memberEncodings = ;
          parts = MultiPartBodySerializer.getFormDataParts(
            fields: {
            },
            files: []
          );
        } else {
          parts = [];
        }
        final bodySerializer = MultiPartBodySerializer(
          parts: parts,
        );
        return bodySerializer.bodyBytesStream;
        break;
      default:

    }
    //var serialized = v.serialize();
    // serialized is guaranteed to be a dart primitive (String, int, List, Map, Uint8List, XFile, XMLElement, etc...)
    //final encoded = json.encode(serialized);
    //final bytes = ;
  }

}


class UserApiUpdateUserResponse {
}


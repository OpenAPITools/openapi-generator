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

    return {
      if (cookieParts.isNotEmpty)
        'Cookie': cookieParts.entries.map((e) => '${e.key}=${e.value}').join('; '),
      ...extraHeaders,
    };
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
    // Add any path/query parameters to the knownUrl.
    return HttpRequestBase.stream(
      url: futureResults[0] as Uri,
      headers: futureResults[1] as Map<String, String>,
      method: method,
      bodyBytesStream: Stream.empty(),
      context: context,
    );
  }
}




class FakeApiFakeGetFreeFormObjectGetResponse {
}


abstract class FakeApiFakeOuterBooleanSerializeRequest {
  static const pathTemplate = r'/fake/outer/boolean';
  static String method = r'POST';

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

    return {
      if (cookieParts.isNotEmpty)
        'Cookie': cookieParts.entries.map((e) => '${e.key}=${e.value}').join('; '),
      ...extraHeaders,
    };
  }


  Stream<List<int>> getResolvedBody({
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
    // Add any path/query parameters to the knownUrl.
    return HttpRequestBase.stream(
      url: futureResults[0] as Uri,
      headers: futureResults[1] as Map<String, String>,
      method: method,
      bodyBytesStream: getResolvedBody(context: context),
      context: context,
    );
  }
}

/// A version of [FakeApiFakeOuterBooleanSerializeRequest], where you can send arbitrary bytes in the body.
class FakeApiFakeOuterBooleanSerializeRequestUnsafe extends FakeApiFakeOuterBooleanSerializeRequest {
  final Stream<Uint8List>? body;
  const FakeApiFakeOuterBooleanSerializeRequestUnsafe({
    this.body,
  
    super.extraHeaders,
    super.extraQueryParameters,
    super.extraCookies,
  });

  Stream<List<int>> getResolvedBody({
    Map<String, dynamic> context = const {},
  }) async* {
    final body = this.body;
    if (body == null) {
      return;
    }
    yield* body;
  }
}

//generate a class for body
//OR
//generate a class for form params (multipart/formdata)


class FakeApiFakeOuterBooleanSerializeRequestSchemaForRequestBodyApplicationJson extends FakeApiFakeOuterBooleanSerializeRequest {
    final String mediaType = r'application/json';
    final UndefinedWrapper<bool> data;
}


class FakeApiFakeOuterBooleanSerializeResponse {
}


abstract class FakeApiFakeOuterCompositeSerializeRequest {
  static const pathTemplate = r'/fake/outer/composite';
  static String method = r'POST';

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

    return {
      if (cookieParts.isNotEmpty)
        'Cookie': cookieParts.entries.map((e) => '${e.key}=${e.value}').join('; '),
      ...extraHeaders,
    };
  }


  Stream<List<int>> getResolvedBody({
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
    // Add any path/query parameters to the knownUrl.
    return HttpRequestBase.stream(
      url: futureResults[0] as Uri,
      headers: futureResults[1] as Map<String, String>,
      method: method,
      bodyBytesStream: getResolvedBody(context: context),
      context: context,
    );
  }
}

/// A version of [FakeApiFakeOuterCompositeSerializeRequest], where you can send arbitrary bytes in the body.
class FakeApiFakeOuterCompositeSerializeRequestUnsafe extends FakeApiFakeOuterCompositeSerializeRequest {
  final Stream<Uint8List>? body;
  const FakeApiFakeOuterCompositeSerializeRequestUnsafe({
    this.body,
  
    super.extraHeaders,
    super.extraQueryParameters,
    super.extraCookies,
  });

  Stream<List<int>> getResolvedBody({
    Map<String, dynamic> context = const {},
  }) async* {
    final body = this.body;
    if (body == null) {
      return;
    }
    yield* body;
  }
}

//generate a class for body
//OR
//generate a class for form params (multipart/formdata)


class FakeApiFakeOuterCompositeSerializeRequestSchemaForRequestBodyApplicationJson extends FakeApiFakeOuterCompositeSerializeRequest {
    final String mediaType = r'application/json';
    final UndefinedWrapper<OuterComposite> data;
}


class FakeApiFakeOuterCompositeSerializeResponse {
}


abstract class FakeApiFakeOuterNumberSerializeRequest {
  static const pathTemplate = r'/fake/outer/number';
  static String method = r'POST';

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

    return {
      if (cookieParts.isNotEmpty)
        'Cookie': cookieParts.entries.map((e) => '${e.key}=${e.value}').join('; '),
      ...extraHeaders,
    };
  }


  Stream<List<int>> getResolvedBody({
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
    // Add any path/query parameters to the knownUrl.
    return HttpRequestBase.stream(
      url: futureResults[0] as Uri,
      headers: futureResults[1] as Map<String, String>,
      method: method,
      bodyBytesStream: getResolvedBody(context: context),
      context: context,
    );
  }
}

/// A version of [FakeApiFakeOuterNumberSerializeRequest], where you can send arbitrary bytes in the body.
class FakeApiFakeOuterNumberSerializeRequestUnsafe extends FakeApiFakeOuterNumberSerializeRequest {
  final Stream<Uint8List>? body;
  const FakeApiFakeOuterNumberSerializeRequestUnsafe({
    this.body,
  
    super.extraHeaders,
    super.extraQueryParameters,
    super.extraCookies,
  });

  Stream<List<int>> getResolvedBody({
    Map<String, dynamic> context = const {},
  }) async* {
    final body = this.body;
    if (body == null) {
      return;
    }
    yield* body;
  }
}

//generate a class for body
//OR
//generate a class for form params (multipart/formdata)


class FakeApiFakeOuterNumberSerializeRequestSchemaForRequestBodyApplicationJson extends FakeApiFakeOuterNumberSerializeRequest {
    final String mediaType = r'application/json';
    final UndefinedWrapper<num> data;
}


class FakeApiFakeOuterNumberSerializeResponse {
}


abstract class FakeApiFakeOuterStringSerializeRequest {
  static const pathTemplate = r'/fake/outer/string';
  static String method = r'POST';

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

    return {
      if (cookieParts.isNotEmpty)
        'Cookie': cookieParts.entries.map((e) => '${e.key}=${e.value}').join('; '),
      ...extraHeaders,
    };
  }


  Stream<List<int>> getResolvedBody({
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
    // Add any path/query parameters to the knownUrl.
    return HttpRequestBase.stream(
      url: futureResults[0] as Uri,
      headers: futureResults[1] as Map<String, String>,
      method: method,
      bodyBytesStream: getResolvedBody(context: context),
      context: context,
    );
  }
}

/// A version of [FakeApiFakeOuterStringSerializeRequest], where you can send arbitrary bytes in the body.
class FakeApiFakeOuterStringSerializeRequestUnsafe extends FakeApiFakeOuterStringSerializeRequest {
  final Stream<Uint8List>? body;
  const FakeApiFakeOuterStringSerializeRequestUnsafe({
    this.body,
  
    super.extraHeaders,
    super.extraQueryParameters,
    super.extraCookies,
  });

  Stream<List<int>> getResolvedBody({
    Map<String, dynamic> context = const {},
  }) async* {
    final body = this.body;
    if (body == null) {
      return;
    }
    yield* body;
  }
}

//generate a class for body
//OR
//generate a class for form params (multipart/formdata)


class FakeApiFakeOuterStringSerializeRequestSchemaForRequestBodyApplicationJson extends FakeApiFakeOuterStringSerializeRequest {
    final String mediaType = r'application/json';
    final UndefinedWrapper<String> data;
}


class FakeApiFakeOuterStringSerializeResponse {
}


abstract class FakeApiFakeUploadRefRequestBodiesRequest {
  static const pathTemplate = r'/fake/pet/{petId}/uploadImage';
  static String method = r'POST';

  final Map<String, String> extraHeaders;
  final Map<String, String> extraCookies;
  final Map<String, Object /* String | List<String> */> extraQueryParameters;

  
  /// ID of pet to update
  /// spec name: petId
  final int petId;
  
  
  

  const factory FakeApiFakeUploadRefRequestBodiesRequest.unsafe({

    required int petId,



    Map<String, String> extraHeaders,
    Map<String, Object> extraQueryParameters,
    Map<String, String> extraCookies,
    Stream<Uint8List>? body,
  }) = FakeApiFakeUploadRefRequestBodiesRequestUnsafe;

  const FakeApiFakeUploadRefRequestBodiesRequest({

    required this.petId,



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

    return {
      if (cookieParts.isNotEmpty)
        'Cookie': cookieParts.entries.map((e) => '${e.key}=${e.value}').join('; '),
      ...extraHeaders,
    };
  }


  Stream<List<int>> getResolvedBody({
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
    // Add any path/query parameters to the knownUrl.
    return HttpRequestBase.stream(
      url: futureResults[0] as Uri,
      headers: futureResults[1] as Map<String, String>,
      method: method,
      bodyBytesStream: getResolvedBody(context: context),
      context: context,
    );
  }
}

/// A version of [FakeApiFakeUploadRefRequestBodiesRequest], where you can send arbitrary bytes in the body.
class FakeApiFakeUploadRefRequestBodiesRequestUnsafe extends FakeApiFakeUploadRefRequestBodiesRequest {
  final Stream<Uint8List>? body;
  const FakeApiFakeUploadRefRequestBodiesRequestUnsafe({
    this.body,
  
    required super.petId,
  
  
  
    super.extraHeaders,
    super.extraQueryParameters,
    super.extraCookies,
  });

  Stream<List<int>> getResolvedBody({
    Map<String, dynamic> context = const {},
  }) async* {
    final body = this.body;
    if (body == null) {
      return;
    }
    yield* body;
  }
}

//generate a class for body
//OR
//generate a class for form params (multipart/formdata)



class FakeApiFakeUploadRefRequestBodiesResponse {
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

    return {
      if (cookieParts.isNotEmpty)
        'Cookie': cookieParts.entries.map((e) => '${e.key}=${e.value}').join('; '),
      ...extraHeaders,
    };
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
    // Add any path/query parameters to the knownUrl.
    return HttpRequestBase.stream(
      url: futureResults[0] as Uri,
      headers: futureResults[1] as Map<String, String>,
      method: method,
      bodyBytesStream: Stream.empty(),
      context: context,
    );
  }
}




class FakeApiGetFakeArrayofenumsResponse {
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

    return {
      if (cookieParts.isNotEmpty)
        'Cookie': cookieParts.entries.map((e) => '${e.key}=${e.value}').join('; '),
      ...extraHeaders,
    };
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
    // Add any path/query parameters to the knownUrl.
    return HttpRequestBase.stream(
      url: futureResults[0] as Uri,
      headers: futureResults[1] as Map<String, String>,
      method: method,
      bodyBytesStream: Stream.empty(),
      context: context,
    );
  }
}




class FakeApiGetFakeHealthResponse {
}


 class FakeApiGetParameterNameMappingRequest {
  static const pathTemplate = r'/fake/parameter-name-mapping';
  static String method = r'GET';

  final Map<String, String> extraHeaders;
  final Map<String, String> extraCookies;
  final Map<String, Object /* String | List<String> */> extraQueryParameters;

  
  /// _type
  /// spec name: _type
  final int $type;
  
  
  /// type
  /// spec name: type
  final String type;
  
  
  /// type_
  /// spec name: type_
  final String type$;
  


  const FakeApiGetParameterNameMappingRequest({

    required this.$type,


    required this.type,


    required this.type$,

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

    return {
      if (cookieParts.isNotEmpty)
        'Cookie': cookieParts.entries.map((e) => '${e.key}=${e.value}').join('; '),
      
        r'_type': OpenApiParameterSerializationHeader(parameterName: r'_type',explode: false).serialize($type),
      
        r'type_': OpenApiParameterSerializationHeader(parameterName: r'type_',explode: false).serialize(type$),
      ...extraHeaders,
    };
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
    // Add any path/query parameters to the knownUrl.
    return HttpRequestBase.stream(
      url: futureResults[0] as Uri,
      headers: futureResults[1] as Map<String, String>,
      method: method,
      bodyBytesStream: Stream.empty(),
      context: context,
    );
  }
}




class FakeApiGetParameterNameMappingResponse {
}


abstract class FakeApiTestAdditionalPropertiesReferenceRequest {
  static const pathTemplate = r'/fake/additionalProperties-reference';
  static String method = r'POST';

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

    return {
      if (cookieParts.isNotEmpty)
        'Cookie': cookieParts.entries.map((e) => '${e.key}=${e.value}').join('; '),
      ...extraHeaders,
    };
  }


  Stream<List<int>> getResolvedBody({
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
    // Add any path/query parameters to the knownUrl.
    return HttpRequestBase.stream(
      url: futureResults[0] as Uri,
      headers: futureResults[1] as Map<String, String>,
      method: method,
      bodyBytesStream: getResolvedBody(context: context),
      context: context,
    );
  }
}

/// A version of [FakeApiTestAdditionalPropertiesReferenceRequest], where you can send arbitrary bytes in the body.
class FakeApiTestAdditionalPropertiesReferenceRequestUnsafe extends FakeApiTestAdditionalPropertiesReferenceRequest {
  final Stream<Uint8List>? body;
  const FakeApiTestAdditionalPropertiesReferenceRequestUnsafe({
    this.body,
  
    super.extraHeaders,
    super.extraQueryParameters,
    super.extraCookies,
  });

  Stream<List<int>> getResolvedBody({
    Map<String, dynamic> context = const {},
  }) async* {
    final body = this.body;
    if (body == null) {
      return;
    }
    yield* body;
  }
}

//generate a class for body
//OR
//generate a class for form params (multipart/formdata)


class FakeApiTestAdditionalPropertiesReferenceRequestSchemaForRequestBodyApplicationJson extends FakeApiTestAdditionalPropertiesReferenceRequest {
    final String mediaType = r'application/json';
    final UndefinedWrapper<Map<String, Object?>> data;
}


class FakeApiTestAdditionalPropertiesReferenceResponse {
}


abstract class FakeApiTestBodyWithFileSchemaRequest {
  static const pathTemplate = r'/fake/body-with-file-schema';
  static String method = r'PUT';

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

    return {
      if (cookieParts.isNotEmpty)
        'Cookie': cookieParts.entries.map((e) => '${e.key}=${e.value}').join('; '),
      ...extraHeaders,
    };
  }


  Stream<List<int>> getResolvedBody({
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
    // Add any path/query parameters to the knownUrl.
    return HttpRequestBase.stream(
      url: futureResults[0] as Uri,
      headers: futureResults[1] as Map<String, String>,
      method: method,
      bodyBytesStream: getResolvedBody(context: context),
      context: context,
    );
  }
}

/// A version of [FakeApiTestBodyWithFileSchemaRequest], where you can send arbitrary bytes in the body.
class FakeApiTestBodyWithFileSchemaRequestUnsafe extends FakeApiTestBodyWithFileSchemaRequest {
  final Stream<Uint8List>? body;
  const FakeApiTestBodyWithFileSchemaRequestUnsafe({
    this.body,
  
    super.extraHeaders,
    super.extraQueryParameters,
    super.extraCookies,
  });

  Stream<List<int>> getResolvedBody({
    Map<String, dynamic> context = const {},
  }) async* {
    final body = this.body;
    if (body == null) {
      return;
    }
    yield* body;
  }
}

//generate a class for body
//OR
//generate a class for form params (multipart/formdata)


class FakeApiTestBodyWithFileSchemaRequestSchemaForRequestBodyApplicationJson extends FakeApiTestBodyWithFileSchemaRequest {
    final String mediaType = r'application/json';
    final UndefinedWrapper<FileSchemaTestClass> data;
}


class FakeApiTestBodyWithFileSchemaResponse {
}


abstract class FakeApiTestBodyWithQueryParamsRequest {
  static const pathTemplate = r'/fake/body-with-query-params';
  static String method = r'PUT';

  final Map<String, String> extraHeaders;
  final Map<String, String> extraCookies;
  final Map<String, Object /* String | List<String> */> extraQueryParameters;

  
  /// 
  /// spec name: query
  final String query;
  
  

  const factory FakeApiTestBodyWithQueryParamsRequest.unsafe({

    required String query,


    Map<String, String> extraHeaders,
    Map<String, Object> extraQueryParameters,
    Map<String, String> extraCookies,
    Stream<Uint8List>? body,
  }) = FakeApiTestBodyWithQueryParamsRequestUnsafe;

  const FakeApiTestBodyWithQueryParamsRequest({

    required this.query,


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

    return {
      if (cookieParts.isNotEmpty)
        'Cookie': cookieParts.entries.map((e) => '${e.key}=${e.value}').join('; '),
      ...extraHeaders,
    };
  }


  Stream<List<int>> getResolvedBody({
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
    // Add any path/query parameters to the knownUrl.
    return HttpRequestBase.stream(
      url: futureResults[0] as Uri,
      headers: futureResults[1] as Map<String, String>,
      method: method,
      bodyBytesStream: getResolvedBody(context: context),
      context: context,
    );
  }
}

/// A version of [FakeApiTestBodyWithQueryParamsRequest], where you can send arbitrary bytes in the body.
class FakeApiTestBodyWithQueryParamsRequestUnsafe extends FakeApiTestBodyWithQueryParamsRequest {
  final Stream<Uint8List>? body;
  const FakeApiTestBodyWithQueryParamsRequestUnsafe({
    this.body,
  
    required super.query,
  
  
    super.extraHeaders,
    super.extraQueryParameters,
    super.extraCookies,
  });

  Stream<List<int>> getResolvedBody({
    Map<String, dynamic> context = const {},
  }) async* {
    final body = this.body;
    if (body == null) {
      return;
    }
    yield* body;
  }
}

//generate a class for body
//OR
//generate a class for form params (multipart/formdata)


class FakeApiTestBodyWithQueryParamsRequestSchemaForRequestBodyApplicationJson extends FakeApiTestBodyWithQueryParamsRequest {
    final String mediaType = r'application/json';
    final UndefinedWrapper<User> data;
}


class FakeApiTestBodyWithQueryParamsResponse {
}


abstract class FakeApiTestClientModelRequest {
  static const pathTemplate = r'/fake';
  static String method = r'PATCH';

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

    return {
      if (cookieParts.isNotEmpty)
        'Cookie': cookieParts.entries.map((e) => '${e.key}=${e.value}').join('; '),
      ...extraHeaders,
    };
  }


  Stream<List<int>> getResolvedBody({
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
    // Add any path/query parameters to the knownUrl.
    return HttpRequestBase.stream(
      url: futureResults[0] as Uri,
      headers: futureResults[1] as Map<String, String>,
      method: method,
      bodyBytesStream: getResolvedBody(context: context),
      context: context,
    );
  }
}

/// A version of [FakeApiTestClientModelRequest], where you can send arbitrary bytes in the body.
class FakeApiTestClientModelRequestUnsafe extends FakeApiTestClientModelRequest {
  final Stream<Uint8List>? body;
  const FakeApiTestClientModelRequestUnsafe({
    this.body,
  
    super.extraHeaders,
    super.extraQueryParameters,
    super.extraCookies,
  });

  Stream<List<int>> getResolvedBody({
    Map<String, dynamic> context = const {},
  }) async* {
    final body = this.body;
    if (body == null) {
      return;
    }
    yield* body;
  }
}

//generate a class for body
//OR
//generate a class for form params (multipart/formdata)


class FakeApiTestClientModelRequestSchemaForRequestBodyApplicationJson extends FakeApiTestClientModelRequest {
    final String mediaType = r'application/json';
    final UndefinedWrapper<Client> data;
}


class FakeApiTestClientModelResponse {
}


abstract class FakeApiTestEndpointParametersRequest {
  static const pathTemplate = r'/fake';
  static String method = r'POST';

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

    return {
      if (cookieParts.isNotEmpty)
        'Cookie': cookieParts.entries.map((e) => '${e.key}=${e.value}').join('; '),
      ...extraHeaders,
    };
  }


  Stream<List<int>> getResolvedBody({
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
    // Add any path/query parameters to the knownUrl.
    return HttpRequestBase.stream(
      url: futureResults[0] as Uri,
      headers: futureResults[1] as Map<String, String>,
      method: method,
      bodyBytesStream: getResolvedBody(context: context),
      context: context,
    );
  }
}

/// A version of [FakeApiTestEndpointParametersRequest], where you can send arbitrary bytes in the body.
class FakeApiTestEndpointParametersRequestUnsafe extends FakeApiTestEndpointParametersRequest {
  final Stream<Uint8List>? body;
  const FakeApiTestEndpointParametersRequestUnsafe({
    this.body,
  
  
  
  
  
  
  
  
  
  
  
  
  
  
    super.extraHeaders,
    super.extraQueryParameters,
    super.extraCookies,
  });

  Stream<List<int>> getResolvedBody({
    Map<String, dynamic> context = const {},
  }) async* {
    final body = this.body;
    if (body == null) {
      return;
    }
    yield* body;
  }
}

//generate a class for body
//OR
//generate a class for form params (multipart/formdata)



class FakeApiTestEndpointParametersResponse {
}


 class FakeApiTestEnumParametersRequest {
  static const pathTemplate = r'/fake';
  static String method = r'GET';

  final Map<String, String> extraHeaders;
  final Map<String, String> extraCookies;
  final Map<String, Object /* String | List<String> */> extraQueryParameters;

  
  /// Header parameter enum test (string array)
  /// spec name: enum_header_string_array
  final UndefinedWrapper<List<EnumHeaderStringArrayEnum>> enumHeaderStringArray;
  
  
  /// Header parameter enum test (string)
  /// spec name: enum_header_string
  final UndefinedWrapper<EnumHeaderStringEnum> enumHeaderString;
  
  
  /// Query parameter enum test (string array)
  /// spec name: enum_query_string_array
  final UndefinedWrapper<List<EnumQueryStringArrayEnum>> enumQueryStringArray;
  
  
  /// Query parameter enum test (string)
  /// spec name: enum_query_string
  final UndefinedWrapper<EnumQueryStringEnum> enumQueryString;
  
  
  /// Query parameter enum test (double)
  /// spec name: enum_query_integer
  final UndefinedWrapper<EnumQueryIntegerEnum> enumQueryInteger;
  
  
  /// Query parameter enum test (double)
  /// spec name: enum_query_double
  final UndefinedWrapper<EnumQueryDoubleEnum> enumQueryDouble;
  
  
  


  const FakeApiTestEnumParametersRequest({

     this.enumHeaderStringArray= const UndefinedWrapper.undefined(),


     this.enumHeaderString= const UndefinedWrapper('-efg'),


     this.enumQueryStringArray= const UndefinedWrapper.undefined(),


     this.enumQueryString= const UndefinedWrapper('-efg'),


     this.enumQueryInteger= const UndefinedWrapper.undefined(),


     this.enumQueryDouble= const UndefinedWrapper.undefined(),



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

    return {
      if (cookieParts.isNotEmpty)
        'Cookie': cookieParts.entries.map((e) => '${e.key}=${e.value}').join('; '),
      if (enumHeaderStringArray.isDefined)
        r'enum_header_string_array': OpenApiParameterSerializationHeader(parameterName: r'enum_header_string_array',explode: false).serialize(enumHeaderStringArray.valueRequired),
      if (enumHeaderString.isDefined)
        r'enum_header_string': OpenApiParameterSerializationHeader(parameterName: r'enum_header_string',explode: false).serialize(enumHeaderString.valueRequired),
      ...extraHeaders,
    };
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
    // Add any path/query parameters to the knownUrl.
    return HttpRequestBase.stream(
      url: futureResults[0] as Uri,
      headers: futureResults[1] as Map<String, String>,
      method: method,
      bodyBytesStream: Stream.empty(),
      context: context,
    );
  }
}




class FakeApiTestEnumParametersResponse {
}


 class FakeApiTestGroupParametersRequest {
  static const pathTemplate = r'/fake';
  static String method = r'DELETE';

  final Map<String, String> extraHeaders;
  final Map<String, String> extraCookies;
  final Map<String, Object /* String | List<String> */> extraQueryParameters;

  
  /// Required String in group parameters
  /// spec name: required_string_group
  final int requiredStringGroup;
  
  
  /// Required Boolean in group parameters
  /// spec name: required_boolean_group
  final bool requiredBooleanGroup;
  
  
  /// Required Integer in group parameters
  /// spec name: required_int64_group
  final int requiredInt64Group;
  
  
  /// String in group parameters
  /// spec name: string_group
  final UndefinedWrapper<int> stringGroup;
  
  
  /// Boolean in group parameters
  /// spec name: boolean_group
  final UndefinedWrapper<bool> booleanGroup;
  
  
  /// Integer in group parameters
  /// spec name: int64_group
  final UndefinedWrapper<int> int64Group;
  


  const FakeApiTestGroupParametersRequest({

    required this.requiredStringGroup,


    required this.requiredBooleanGroup,


    required this.requiredInt64Group,


     this.stringGroup= const UndefinedWrapper.undefined(),


     this.booleanGroup= const UndefinedWrapper.undefined(),


     this.int64Group= const UndefinedWrapper.undefined(),

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

    return {
      if (cookieParts.isNotEmpty)
        'Cookie': cookieParts.entries.map((e) => '${e.key}=${e.value}').join('; '),
      
        r'required_boolean_group': OpenApiParameterSerializationHeader(parameterName: r'required_boolean_group',explode: false).serialize(requiredBooleanGroup),
      if (booleanGroup.isDefined)
        r'boolean_group': OpenApiParameterSerializationHeader(parameterName: r'boolean_group',explode: false).serialize(booleanGroup.valueRequired),
      ...extraHeaders,
    };
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
    // Add any path/query parameters to the knownUrl.
    return HttpRequestBase.stream(
      url: futureResults[0] as Uri,
      headers: futureResults[1] as Map<String, String>,
      method: method,
      bodyBytesStream: Stream.empty(),
      context: context,
    );
  }
}




class FakeApiTestGroupParametersResponse {
}


abstract class FakeApiTestInlineAdditionalPropertiesRequest {
  static const pathTemplate = r'/fake/inline-additionalProperties';
  static String method = r'POST';

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

    return {
      if (cookieParts.isNotEmpty)
        'Cookie': cookieParts.entries.map((e) => '${e.key}=${e.value}').join('; '),
      ...extraHeaders,
    };
  }


  Stream<List<int>> getResolvedBody({
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
    // Add any path/query parameters to the knownUrl.
    return HttpRequestBase.stream(
      url: futureResults[0] as Uri,
      headers: futureResults[1] as Map<String, String>,
      method: method,
      bodyBytesStream: getResolvedBody(context: context),
      context: context,
    );
  }
}

/// A version of [FakeApiTestInlineAdditionalPropertiesRequest], where you can send arbitrary bytes in the body.
class FakeApiTestInlineAdditionalPropertiesRequestUnsafe extends FakeApiTestInlineAdditionalPropertiesRequest {
  final Stream<Uint8List>? body;
  const FakeApiTestInlineAdditionalPropertiesRequestUnsafe({
    this.body,
  
    super.extraHeaders,
    super.extraQueryParameters,
    super.extraCookies,
  });

  Stream<List<int>> getResolvedBody({
    Map<String, dynamic> context = const {},
  }) async* {
    final body = this.body;
    if (body == null) {
      return;
    }
    yield* body;
  }
}

//generate a class for body
//OR
//generate a class for form params (multipart/formdata)


class FakeApiTestInlineAdditionalPropertiesRequestSchemaForRequestBodyApplicationJson extends FakeApiTestInlineAdditionalPropertiesRequest {
    final String mediaType = r'application/json';
    final UndefinedWrapper<Map<String, String>> data;
}


class FakeApiTestInlineAdditionalPropertiesResponse {
}


abstract class FakeApiTestInlineFreeformAdditionalPropertiesRequest {
  static const pathTemplate = r'/fake/inline-freeform-additionalProperties';
  static String method = r'POST';

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

    return {
      if (cookieParts.isNotEmpty)
        'Cookie': cookieParts.entries.map((e) => '${e.key}=${e.value}').join('; '),
      ...extraHeaders,
    };
  }


  Stream<List<int>> getResolvedBody({
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
    // Add any path/query parameters to the knownUrl.
    return HttpRequestBase.stream(
      url: futureResults[0] as Uri,
      headers: futureResults[1] as Map<String, String>,
      method: method,
      bodyBytesStream: getResolvedBody(context: context),
      context: context,
    );
  }
}

/// A version of [FakeApiTestInlineFreeformAdditionalPropertiesRequest], where you can send arbitrary bytes in the body.
class FakeApiTestInlineFreeformAdditionalPropertiesRequestUnsafe extends FakeApiTestInlineFreeformAdditionalPropertiesRequest {
  final Stream<Uint8List>? body;
  const FakeApiTestInlineFreeformAdditionalPropertiesRequestUnsafe({
    this.body,
  
    super.extraHeaders,
    super.extraQueryParameters,
    super.extraCookies,
  });

  Stream<List<int>> getResolvedBody({
    Map<String, dynamic> context = const {},
  }) async* {
    final body = this.body;
    if (body == null) {
      return;
    }
    yield* body;
  }
}

//generate a class for body
//OR
//generate a class for form params (multipart/formdata)


class FakeApiTestInlineFreeformAdditionalPropertiesRequestSchemaForRequestBodyApplicationJson extends FakeApiTestInlineFreeformAdditionalPropertiesRequest {
    final String mediaType = r'application/json';
    final UndefinedWrapper<TestInlineFreeformAdditionalPropertiesRequest> data;
}


class FakeApiTestInlineFreeformAdditionalPropertiesResponse {
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

    return {
      if (cookieParts.isNotEmpty)
        'Cookie': cookieParts.entries.map((e) => '${e.key}=${e.value}').join('; '),
      ...extraHeaders,
    };
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
    // Add any path/query parameters to the knownUrl.
    return HttpRequestBase.stream(
      url: futureResults[0] as Uri,
      headers: futureResults[1] as Map<String, String>,
      method: method,
      bodyBytesStream: Stream.empty(),
      context: context,
    );
  }
}




class FakeApiTestJsonFormDataResponse {
}


abstract class FakeApiTestQueryParameterCollectionFormatRequest {
  static const pathTemplate = r'/fake/test-query-parameters';
  static String method = r'PUT';

  final Map<String, String> extraHeaders;
  final Map<String, String> extraCookies;
  final Map<String, Object /* String | List<String> */> extraQueryParameters;

  
  /// 
  /// spec name: pipe
  final List<String> pipe;
  
  
  /// 
  /// spec name: ioutil
  final List<String> ioutil;
  
  
  /// 
  /// spec name: http
  final List<String> http;
  
  
  /// 
  /// spec name: url
  final List<String> url;
  
  
  /// 
  /// spec name: context
  final List<String> context;
  

  const factory FakeApiTestQueryParameterCollectionFormatRequest.unsafe({

    required List<String> pipe,


    required List<String> ioutil,


    required List<String> http,


    required List<String> url,


    required List<String> context,

    Map<String, String> extraHeaders,
    Map<String, Object> extraQueryParameters,
    Map<String, String> extraCookies,
    Stream<Uint8List>? body,
  }) = FakeApiTestQueryParameterCollectionFormatRequestUnsafe;

  const FakeApiTestQueryParameterCollectionFormatRequest({

    required this.pipe,


    required this.ioutil,


    required this.http,


    required this.url,


    required this.context,

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

    return {
      if (cookieParts.isNotEmpty)
        'Cookie': cookieParts.entries.map((e) => '${e.key}=${e.value}').join('; '),
      ...extraHeaders,
    };
  }


  Stream<List<int>> getResolvedBody({
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
    // Add any path/query parameters to the knownUrl.
    return HttpRequestBase.stream(
      url: futureResults[0] as Uri,
      headers: futureResults[1] as Map<String, String>,
      method: method,
      bodyBytesStream: getResolvedBody(context: context),
      context: context,
    );
  }
}

/// A version of [FakeApiTestQueryParameterCollectionFormatRequest], where you can send arbitrary bytes in the body.
class FakeApiTestQueryParameterCollectionFormatRequestUnsafe extends FakeApiTestQueryParameterCollectionFormatRequest {
  final Stream<Uint8List>? body;
  const FakeApiTestQueryParameterCollectionFormatRequestUnsafe({
    this.body,
  
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
    Map<String, dynamic> context = const {},
  }) async* {
    final body = this.body;
    if (body == null) {
      return;
    }
    yield* body;
  }
}




class FakeApiTestQueryParameterCollectionFormatResponse {
}


abstract class FakeApiTestStringMapReferenceRequest {
  static const pathTemplate = r'/fake/stringMap-reference';
  static String method = r'POST';

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

    return {
      if (cookieParts.isNotEmpty)
        'Cookie': cookieParts.entries.map((e) => '${e.key}=${e.value}').join('; '),
      ...extraHeaders,
    };
  }


  Stream<List<int>> getResolvedBody({
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
    // Add any path/query parameters to the knownUrl.
    return HttpRequestBase.stream(
      url: futureResults[0] as Uri,
      headers: futureResults[1] as Map<String, String>,
      method: method,
      bodyBytesStream: getResolvedBody(context: context),
      context: context,
    );
  }
}

/// A version of [FakeApiTestStringMapReferenceRequest], where you can send arbitrary bytes in the body.
class FakeApiTestStringMapReferenceRequestUnsafe extends FakeApiTestStringMapReferenceRequest {
  final Stream<Uint8List>? body;
  const FakeApiTestStringMapReferenceRequestUnsafe({
    this.body,
  
    super.extraHeaders,
    super.extraQueryParameters,
    super.extraCookies,
  });

  Stream<List<int>> getResolvedBody({
    Map<String, dynamic> context = const {},
  }) async* {
    final body = this.body;
    if (body == null) {
      return;
    }
    yield* body;
  }
}

//generate a class for body
//OR
//generate a class for form params (multipart/formdata)


class FakeApiTestStringMapReferenceRequestSchemaForRequestBodyApplicationJson extends FakeApiTestStringMapReferenceRequest {
    final String mediaType = r'application/json';
    final UndefinedWrapper<Map<String, String>> data;
}


class FakeApiTestStringMapReferenceResponse {
}


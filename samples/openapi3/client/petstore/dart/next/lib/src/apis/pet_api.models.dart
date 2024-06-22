part of 'pet_api.dart';


abstract class PetApiAddPetRequest {
  static const pathTemplate = r'/pet';
  static String method = r'POST';

  final Map<String, String> extraHeaders;
  final Map<String, String> extraCookies;
  final Map<String, Object /* String | List<String> */> extraQueryParameters;

  

  const factory PetApiAddPetRequest.unsafe({

    Map<String, String> extraHeaders,
    Map<String, Object> extraQueryParameters,
    Map<String, String> extraCookies,
    Stream<Uint8List>? body,
  }) = PetApiAddPetRequestUnsafe;

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

/// A version of [PetApiAddPetRequest], where you can send arbitrary bytes in the body.
class PetApiAddPetRequestUnsafe extends PetApiAddPetRequest {
  final Stream<Uint8List>? body;
  const PetApiAddPetRequestUnsafe({
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


class PetApiAddPetRequestApplicationJson extends PetApiAddPetRequest {
  static const mediaType = r'application/json';

  final UndefinedWrapper<
            Pet
> data;

  const PetApiAddPetRequestApplicationJson({
     this.data= const UndefinedWrapper
        .undefined()
,
    
    super.extraHeaders,
    super.extraQueryParameters,
    super.extraCookies,
  });

  @override
  Stream<List<int>> getResolvedBody({
    Map<String, dynamic> context = const {},
  }) async* {

  }
}

class PetApiAddPetRequestApplicationXml extends PetApiAddPetRequest {
  static const mediaType = r'application/xml';

  final UndefinedWrapper<
            Pet
> data;

  const PetApiAddPetRequestApplicationXml({
     this.data= const UndefinedWrapper
        .undefined()
,
    
    super.extraHeaders,
    super.extraQueryParameters,
    super.extraCookies,
  });

  @override
  Stream<List<int>> getResolvedBody({
    Map<String, dynamic> context = const {},
  }) async* {

  }
}


class PetApiAddPetResponse {
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

    return {
      if (cookieParts.isNotEmpty)
        'Cookie': cookieParts.entries.map((e) => '${e.key}=${e.value}').join('; '),
      if (apiKey.isDefined)
        r'api_key': OpenApiParameterSerializationHeader(parameterName: r'api_key',explode: false).serialize(apiKey.valueRequired),
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




class PetApiDeletePetResponse {
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




class PetApiFindPetsByStatusResponse {
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




@Deprecated('This operation has been deprecated')
class PetApiFindPetsByTagsResponse {
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




class PetApiGetPetByIdResponse {
}


abstract class PetApiUpdatePetRequest {
  static const pathTemplate = r'/pet';
  static String method = r'PUT';

  final Map<String, String> extraHeaders;
  final Map<String, String> extraCookies;
  final Map<String, Object /* String | List<String> */> extraQueryParameters;

  

  const factory PetApiUpdatePetRequest.unsafe({

    Map<String, String> extraHeaders,
    Map<String, Object> extraQueryParameters,
    Map<String, String> extraCookies,
    Stream<Uint8List>? body,
  }) = PetApiUpdatePetRequestUnsafe;

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

/// A version of [PetApiUpdatePetRequest], where you can send arbitrary bytes in the body.
class PetApiUpdatePetRequestUnsafe extends PetApiUpdatePetRequest {
  final Stream<Uint8List>? body;
  const PetApiUpdatePetRequestUnsafe({
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


class PetApiUpdatePetRequestApplicationJson extends PetApiUpdatePetRequest {
  static const mediaType = r'application/json';

  final UndefinedWrapper<
            Pet
> data;

  const PetApiUpdatePetRequestApplicationJson({
     this.data= const UndefinedWrapper
        .undefined()
,
    
    super.extraHeaders,
    super.extraQueryParameters,
    super.extraCookies,
  });

  @override
  Stream<List<int>> getResolvedBody({
    Map<String, dynamic> context = const {},
  }) async* {

  }
}

class PetApiUpdatePetRequestApplicationXml extends PetApiUpdatePetRequest {
  static const mediaType = r'application/xml';

  final UndefinedWrapper<
            Pet
> data;

  const PetApiUpdatePetRequestApplicationXml({
     this.data= const UndefinedWrapper
        .undefined()
,
    
    super.extraHeaders,
    super.extraQueryParameters,
    super.extraCookies,
  });

  @override
  Stream<List<int>> getResolvedBody({
    Map<String, dynamic> context = const {},
  }) async* {

  }
}


class PetApiUpdatePetResponse {
}


abstract class PetApiUpdatePetWithFormRequest {
  static const pathTemplate = r'/pet/{petId}';
  static String method = r'POST';

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
    Stream<Uint8List>? body,
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

/// A version of [PetApiUpdatePetWithFormRequest], where you can send arbitrary bytes in the body.
class PetApiUpdatePetWithFormRequestUnsafe extends PetApiUpdatePetWithFormRequest {
  final Stream<Uint8List>? body;
  const PetApiUpdatePetWithFormRequestUnsafe({
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



class PetApiUpdatePetWithFormResponse {
}


abstract class PetApiUploadFileRequest {
  static const pathTemplate = r'/pet/{petId}/uploadImage';
  static String method = r'POST';

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
    Stream<Uint8List>? body,
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

/// A version of [PetApiUploadFileRequest], where you can send arbitrary bytes in the body.
class PetApiUploadFileRequestUnsafe extends PetApiUploadFileRequest {
  final Stream<Uint8List>? body;
  const PetApiUploadFileRequestUnsafe({
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



class PetApiUploadFileResponse {
}


abstract class PetApiUploadFileWithRequiredFileRequest {
  static const pathTemplate = r'/fake/{petId}/uploadImageWithRequiredFile';
  static String method = r'POST';

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
    Stream<Uint8List>? body,
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

/// A version of [PetApiUploadFileWithRequiredFileRequest], where you can send arbitrary bytes in the body.
class PetApiUploadFileWithRequiredFileRequestUnsafe extends PetApiUploadFileWithRequiredFileRequest {
  final Stream<Uint8List>? body;
  const PetApiUploadFileWithRequiredFileRequestUnsafe({
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



class PetApiUploadFileWithRequiredFileResponse {
}


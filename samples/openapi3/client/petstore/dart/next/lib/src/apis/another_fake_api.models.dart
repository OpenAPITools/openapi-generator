part of 'another_fake_api.dart';


abstract class AnotherFakeApi$123testSpecialTagsRequest {
  static const pathTemplate = r'/another-fake/dummy';
  static String method = r'PATCH';

  final Map<String, String> extraHeaders;
  final Map<String, String> extraCookies;
  final Map<String, Object /* String | List<String> */> extraQueryParameters;

  

  const factory AnotherFakeApi$123testSpecialTagsRequest.unsafe({

    Map<String, String> extraHeaders,
    Map<String, Object> extraQueryParameters,
    Map<String, String> extraCookies,
    Stream<Uint8List>? body,
  }) = AnotherFakeApi$123testSpecialTagsRequestUnsafe;

  const AnotherFakeApi$123testSpecialTagsRequest({

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

/// A version of [AnotherFakeApi$123testSpecialTagsRequest], where you can send arbitrary bytes in the body.
class AnotherFakeApi$123testSpecialTagsRequestUnsafe extends AnotherFakeApi$123testSpecialTagsRequest {
  final Stream<Uint8List>? body;
  const AnotherFakeApi$123testSpecialTagsRequestUnsafe({
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


class AnotherFakeApi$123testSpecialTagsRequestApplicationJson extends AnotherFakeApi$123testSpecialTagsRequest {
  static const mediaType = r'application/json';

  final UndefinedWrapper<
            Client
> data;

  const AnotherFakeApi$123testSpecialTagsRequestApplicationJson({
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


class AnotherFakeApi$123testSpecialTagsResponse {
}


 class AnotherFakeApiGetParameterArrayNumberRequest {
  static const pathTemplate = r'/fake/parameter-array-number';
  static String method = r'GET';

  final Map<String, String> extraHeaders;
  final Map<String, String> extraCookies;
  final Map<String, Object /* String | List<String> */> extraQueryParameters;

  
  /// array integer
  /// spec name: array
  final 
    List<
        
            int
>
 array;
  


  const AnotherFakeApiGetParameterArrayNumberRequest({

     this.array    =
        const
        [1]
        
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
      
        r'array': OpenApiParameterSerializationHeader(parameterName: r'array',explode: false).serialize(array),
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




class AnotherFakeApiGetParameterArrayNumberResponse {
}


 class AnotherFakeApiGetParameterStringNumberRequest {
  static const pathTemplate = r'/fake/parameter-string-number';
  static String method = r'GET';

  final Map<String, String> extraHeaders;
  final Map<String, String> extraCookies;
  final Map<String, Object /* String | List<String> */> extraQueryParameters;

  
  /// string number
  /// spec name: string_number
  final 
            double
 stringNumber;
  


  const AnotherFakeApiGetParameterStringNumberRequest({

    required this.stringNumber    ,

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
      
        r'string_number': OpenApiParameterSerializationHeader(parameterName: r'string_number',explode: false).serialize(stringNumber),
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




class AnotherFakeApiGetParameterStringNumberResponse {
}


 class AnotherFakeApiNullRequestBodyRequest {
  static const pathTemplate = r'/fake/null-request-body';
  static String method = r'GET';

  final Map<String, String> extraHeaders;
  final Map<String, String> extraCookies;
  final Map<String, Object /* String | List<String> */> extraQueryParameters;

  
  /// 
  /// spec name: Accept-Language
  final UndefinedWrapper<
            String
> acceptLanguage;
  
  


  const AnotherFakeApiNullRequestBodyRequest({

     this.acceptLanguage= const UndefinedWrapper
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
      if (acceptLanguage.isDefined)
        r'Accept-Language': OpenApiParameterSerializationHeader(parameterName: r'Accept-Language',explode: false).serialize(acceptLanguage.valueRequired),
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



class AnotherFakeApiNullRequestBodyRequestTextPlain extends AnotherFakeApiNullRequestBodyRequest {
  static const mediaType = r'text/plain';

  final UndefinedWrapper<
            String
> data;

  const AnotherFakeApiNullRequestBodyRequestTextPlain({
     this.data= const UndefinedWrapper
        .undefined()
,
    
     super.acceptLanguage,
    
    
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


class AnotherFakeApiNullRequestBodyResponse {
}


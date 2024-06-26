part of 'another_fake_api.dart';

abstract class AnotherFakeApi$123testSpecialTagsRequest {
  static const pathTemplate = r'/another-fake/dummy';
  static String method = r'PATCH';

  String get contentType;
  final Map<String, String> extraHeaders;
  final Map<String, String> extraCookies;
  final Map<String, Object /* String | List<String> */ > extraQueryParameters;

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
    final cookieParts = <String, String>{
      ...extraCookies,
    };

    return CaseInsensitiveMap<String>.from(<String, String>{
      'Content-Type': contentType,
      if (cookieParts.isNotEmpty)
        'Cookie':
            cookieParts.entries.map((e) => '${e.key}=${e.value}').join('; '),
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
      bodyBytesStream: getResolvedBody(
          context: context, resolvedMediaType: parsedContentType),
      context: context,
    );
  }
}

/// A version of [AnotherFakeApi$123testSpecialTagsRequest], where you can send arbitrary bytes in the body.
class AnotherFakeApi$123testSpecialTagsRequestUnsafe
    extends AnotherFakeApi$123testSpecialTagsRequest {
  final Stream<Uint8List>? body;

  @override
  final String contentType;

  const AnotherFakeApi$123testSpecialTagsRequestUnsafe({
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

class AnotherFakeApi$123testSpecialTagsRequestApplicationJson
    extends AnotherFakeApi$123testSpecialTagsRequest {
  static const specMediaType = r'application/json';

  @override
  String get contentType => specMediaType;

  final Client data;

  const AnotherFakeApi$123testSpecialTagsRequestApplicationJson({
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
          parts =
              MultiPartBodySerializer.getFormDataParts(fields: {}, files: []);
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

class AnotherFakeApi$123testSpecialTagsResponse {}

class AnotherFakeApiGetParameterArrayNumberRequest {
  static const pathTemplate = r'/fake/parameter-array-number';
  static String method = r'GET';

  final Map<String, String> extraHeaders;
  final Map<String, String> extraCookies;
  final Map<String, Object /* String | List<String> */ > extraQueryParameters;

  /// array integer
  /// spec name: array
  final List<int> array;

  const AnotherFakeApiGetParameterArrayNumberRequest({
    this.array = const [1],
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
    final cookieParts = <String, String>{
      ...extraCookies,
    };

    return CaseInsensitiveMap<String>.from(<String, String>{
      if (cookieParts.isNotEmpty)
        'Cookie':
            cookieParts.entries.map((e) => '${e.key}=${e.value}').join('; '),
      r'array': OpenApiParameterSerializationHeader(
              parameterName: r'array', explode: false)
          .serialize(array),
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

class AnotherFakeApiGetParameterArrayNumberResponse {}

class AnotherFakeApiGetParameterStringNumberRequest {
  static const pathTemplate = r'/fake/parameter-string-number';
  static String method = r'GET';

  final Map<String, String> extraHeaders;
  final Map<String, String> extraCookies;
  final Map<String, Object /* String | List<String> */ > extraQueryParameters;

  /// string number
  /// spec name: string_number
  final double stringNumber;

  const AnotherFakeApiGetParameterStringNumberRequest({
    required this.stringNumber,
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
    final cookieParts = <String, String>{
      ...extraCookies,
    };

    return CaseInsensitiveMap<String>.from(<String, String>{
      if (cookieParts.isNotEmpty)
        'Cookie':
            cookieParts.entries.map((e) => '${e.key}=${e.value}').join('; '),
      r'string_number': OpenApiParameterSerializationHeader(
              parameterName: r'string_number', explode: false)
          .serialize(stringNumber),
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

class AnotherFakeApiGetParameterStringNumberResponse {}

class AnotherFakeApiNullRequestBodyRequest {
  static const pathTemplate = r'/fake/null-request-body';
  static String method = r'GET';

  final Map<String, String> extraHeaders;
  final Map<String, String> extraCookies;
  final Map<String, Object /* String | List<String> */ > extraQueryParameters;

  ///
  /// spec name: Accept-Language
  final UndefinedWrapper<String> acceptLanguage;

  const AnotherFakeApiNullRequestBodyRequest({
    this.acceptLanguage = const UndefinedWrapper.undefined(),
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
    final cookieParts = <String, String>{
      ...extraCookies,
    };

    return CaseInsensitiveMap<String>.from(<String, String>{
      if (cookieParts.isNotEmpty)
        'Cookie':
            cookieParts.entries.map((e) => '${e.key}=${e.value}').join('; '),
      if (acceptLanguage.isDefined)
        r'Accept-Language': OpenApiParameterSerializationHeader(
                parameterName: r'Accept-Language', explode: false)
            .serialize(acceptLanguage.valueRequired),
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

class AnotherFakeApiNullRequestBodyResponse {}

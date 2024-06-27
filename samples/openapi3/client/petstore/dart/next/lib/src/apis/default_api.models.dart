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


class DefaultApiFakeAnyOfWIthSameErasureGetResponse {
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


class DefaultApiFakeOneOfWIthSameErasureGetResponse {
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


class DefaultApiFooGetResponse {
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
    Stream<Uint8List>? body,
  }) = DefaultApiPetsMulticontentTestPostRequestUnsafe;

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
  final Stream<Uint8List>? body;

  @override
  final String contentType;

  const DefaultApiPetsMulticontentTestPostRequestUnsafe({
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


class DefaultApiPetsMulticontentTestPostRequestApplicationJson extends DefaultApiPetsMulticontentTestPostRequest {
  static const specMediaType = r'application/json';

  @override
  String get contentType => specMediaType;

  final 
            Pet
 data;

  const DefaultApiPetsMulticontentTestPostRequestApplicationJson({
    required this.data,
    
    super.extraHeaders,
    super.extraQueryParameters,
    super.extraCookies,
  });

  @override
  Stream<List<int>> getResolvedBody({
    required Map<String, String> headers,
    Map<String, dynamic> context = const {},
  }) {
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
    final encodingRules = <String, Map<String,dynamic>>{
      
    };

    // Since the user can override mime type at runtime, we need to check the
    // mime type and serialize the model accordingly.
    switch (resolvedMediaType) {
      case MediaType(type: 'application', subtype: 'json'):
        return _stringResult(json.encode(serialized));
      case MediaType(type: 'application', subtype: 'x-www-form-urlencoded'):
        if (serialized is! Map<String, dynamic>) {
          return _stringResult(serialized.toString());
        }
        var result = Uri();

        for (var e in serialized.entries) {
          final rule = encodingRules[e.key];
          final style = rule?['style'] ?? 'form';
          final explode = rule?['explode'] ?? (style == 'form');
          result = OpenApiParameterSerializationQuery.fromStyle(style, explode: explode, parameterName: e.key, allowEmptyValue: false,).expandUri(result, e.value);
        }
        var resultString = result.query.toString();
        if (resultString.startsWith('?')) {
          resultString= resultString.substring(1);
        }
        return _stringResult(resultString);
      case MediaType(type: 'application', subtype: 'xml'):
        break;
      case MediaType(type: 'application', subtype: 'octet-stream'):
        break;
      case MediaType(type: 'multipart'):
        List<HttpPacketMixin> parts;
        if (serialized is! Map<String, dynamic>) {
          throw ArgumentError('The serialized data must be a map in a multipart request.');
        }
        if (resolvedMediaType.subtype == 'form-data') {
          final fields = <String, String>{};
          final files = <MultiPartFormDataFileHttpPacket>[];
          for (final e in serialized.entries) {
            final rule = encodingRules[e.key];
            final headers = rule?['headers'];
            final contentType = rule?['contentType'];
          }
          //vars []
          parts = MultiPartBodySerializer.getFormDataParts(
            fields: {
            },
            files: []
          );
        } else {
          parts = [];
        }
        final bodySerializer = MultiPartBodySerializer(
          boundary: resolvedMediaType.parameters['boundary'],
          parts: parts,
        );
        return bodySerializer.bodyBytesStream;
      default:
    }
    //var serialized = v.serialize();
    // serialized is guaranteed to be a dart primitive (String, int, List, Map, Uint8List, XFile, XMLElement, etc...)
    //final encoded = json.encode(serialized);
    //final bytes = ;
  }
}

class DefaultApiPetsMulticontentTestPostRequestApplicationXml extends DefaultApiPetsMulticontentTestPostRequest {
  static const specMediaType = r'application/xml';

  @override
  String get contentType => specMediaType;

  final 
            NewPet
 data;

  const DefaultApiPetsMulticontentTestPostRequestApplicationXml({
    required this.data,
    
    super.extraHeaders,
    super.extraQueryParameters,
    super.extraCookies,
  });

  @override
  Stream<List<int>> getResolvedBody({
    required Map<String, String> headers,
    Map<String, dynamic> context = const {},
  }) {
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
    final encodingRules = <String, Map<String,dynamic>>{
      
    };

    // Since the user can override mime type at runtime, we need to check the
    // mime type and serialize the model accordingly.
    switch (resolvedMediaType) {
      case MediaType(type: 'application', subtype: 'json'):
        return _stringResult(json.encode(serialized));
      case MediaType(type: 'application', subtype: 'x-www-form-urlencoded'):
        if (serialized is! Map<String, dynamic>) {
          return _stringResult(serialized.toString());
        }
        var result = Uri();

        for (var e in serialized.entries) {
          final rule = encodingRules[e.key];
          final style = rule?['style'] ?? 'form';
          final explode = rule?['explode'] ?? (style == 'form');
          result = OpenApiParameterSerializationQuery.fromStyle(style, explode: explode, parameterName: e.key, allowEmptyValue: false,).expandUri(result, e.value);
        }
        var resultString = result.query.toString();
        if (resultString.startsWith('?')) {
          resultString= resultString.substring(1);
        }
        return _stringResult(resultString);
      case MediaType(type: 'application', subtype: 'xml'):
        break;
      case MediaType(type: 'application', subtype: 'octet-stream'):
        break;
      case MediaType(type: 'multipart'):
        List<HttpPacketMixin> parts;
        if (serialized is! Map<String, dynamic>) {
          throw ArgumentError('The serialized data must be a map in a multipart request.');
        }
        if (resolvedMediaType.subtype == 'form-data') {
          final fields = <String, String>{};
          final files = <MultiPartFormDataFileHttpPacket>[];
          for (final e in serialized.entries) {
            final rule = encodingRules[e.key];
            final headers = rule?['headers'];
            final contentType = rule?['contentType'];
          }
          //vars []
          parts = MultiPartBodySerializer.getFormDataParts(
            fields: {
            },
            files: []
          );
        } else {
          parts = [];
        }
        final bodySerializer = MultiPartBodySerializer(
          boundary: resolvedMediaType.parameters['boundary'],
          parts: parts,
        );
        return bodySerializer.bodyBytesStream;
      default:
    }
    //var serialized = v.serialize();
    // serialized is guaranteed to be a dart primitive (String, int, List, Map, Uint8List, XFile, XMLElement, etc...)
    //final encoded = serialized;
    //final bytes = ;
  }
}

class DefaultApiPetsMulticontentTestPostRequestApplicationXWwwFormUrlencoded extends DefaultApiPetsMulticontentTestPostRequest {
  static const specMediaType = r'application/x-www-form-urlencoded';

  @override
  String get contentType => specMediaType;

  final 
            Triangle
 data;

  const DefaultApiPetsMulticontentTestPostRequestApplicationXWwwFormUrlencoded({
    required this.data,
    
    super.extraHeaders,
    super.extraQueryParameters,
    super.extraCookies,
  });

  @override
  Stream<List<int>> getResolvedBody({
    required Map<String, String> headers,
    Map<String, dynamic> context = const {},
  }) {
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
    final encodingRules = <String, Map<String,dynamic>>{
      
      r'name': {
        'style': r'form',
        'explode': CodegenEncoding{contentType&#x3D;null, headers&#x3D;[], style&#x3D;form, explode&#x3D;true, allowReserved&#x3D;false, vendorExtensions&#x3D;{}},
        
        
      }
      
    };

    // Since the user can override mime type at runtime, we need to check the
    // mime type and serialize the model accordingly.
    switch (resolvedMediaType) {
      case MediaType(type: 'application', subtype: 'json'):
        return _stringResult(json.encode(serialized));
      case MediaType(type: 'application', subtype: 'x-www-form-urlencoded'):
        if (serialized is! Map<String, dynamic>) {
          return _stringResult(serialized.toString());
        }
        var result = Uri();

        for (var e in serialized.entries) {
          final rule = encodingRules[e.key];
          final style = rule?['style'] ?? 'form';
          final explode = rule?['explode'] ?? (style == 'form');
          result = OpenApiParameterSerializationQuery.fromStyle(style, explode: explode, parameterName: e.key, allowEmptyValue: false,).expandUri(result, e.value);
        }
        var resultString = result.query.toString();
        if (resultString.startsWith('?')) {
          resultString= resultString.substring(1);
        }
        return _stringResult(resultString);
      case MediaType(type: 'application', subtype: 'xml'):
        break;
      case MediaType(type: 'application', subtype: 'octet-stream'):
        break;
      case MediaType(type: 'multipart'):
        List<HttpPacketMixin> parts;
        if (serialized is! Map<String, dynamic>) {
          throw ArgumentError('The serialized data must be a map in a multipart request.');
        }
        if (resolvedMediaType.subtype == 'form-data') {
          final fields = <String, String>{};
          final files = <MultiPartFormDataFileHttpPacket>[];
          for (final e in serialized.entries) {
            final rule = encodingRules[e.key];
            final headers = rule?['headers'];
            final contentType = rule?['contentType'];
          }
          //vars []
          parts = MultiPartBodySerializer.getFormDataParts(
            fields: {
            },
            files: []
          );
        } else {
          parts = [];
        }
        final bodySerializer = MultiPartBodySerializer(
          boundary: resolvedMediaType.parameters['boundary'],
          parts: parts,
        );
        return bodySerializer.bodyBytesStream;
      default:
    }
    //var serialized = v.serialize();
    // serialized is guaranteed to be a dart primitive (String, int, List, Map, Uint8List, XFile, XMLElement, etc...)
    //final encoded = serialized;
    //final bytes = ;
  }
}

class DefaultApiPetsMulticontentTestPostRequestTextPlain extends DefaultApiPetsMulticontentTestPostRequest {
  static const specMediaType = r'text/plain';

  @override
  String get contentType => specMediaType;

  final 
            int
 data;

  const DefaultApiPetsMulticontentTestPostRequestTextPlain({
    required this.data,
    
    super.extraHeaders,
    super.extraQueryParameters,
    super.extraCookies,
  });

  @override
  Stream<List<int>> getResolvedBody({
    required Map<String, String> headers,
    Map<String, dynamic> context = const {},
  }) {
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
    final encodingRules = <String, Map<String,dynamic>>{
      
    };

    // Since the user can override mime type at runtime, we need to check the
    // mime type and serialize the model accordingly.
    switch (resolvedMediaType) {
      case MediaType(type: 'application', subtype: 'json'):
        return _stringResult(json.encode(serialized));
      case MediaType(type: 'application', subtype: 'x-www-form-urlencoded'):
        if (serialized is! Map<String, dynamic>) {
          return _stringResult(serialized.toString());
        }
        var result = Uri();

        for (var e in serialized.entries) {
          final rule = encodingRules[e.key];
          final style = rule?['style'] ?? 'form';
          final explode = rule?['explode'] ?? (style == 'form');
          result = OpenApiParameterSerializationQuery.fromStyle(style, explode: explode, parameterName: e.key, allowEmptyValue: false,).expandUri(result, e.value);
        }
        var resultString = result.query.toString();
        if (resultString.startsWith('?')) {
          resultString= resultString.substring(1);
        }
        return _stringResult(resultString);
      case MediaType(type: 'application', subtype: 'xml'):
        break;
      case MediaType(type: 'application', subtype: 'octet-stream'):
        break;
      case MediaType(type: 'multipart'):
        List<HttpPacketMixin> parts;
        if (serialized is! Map<String, dynamic>) {
          throw ArgumentError('The serialized data must be a map in a multipart request.');
        }
        if (resolvedMediaType.subtype == 'form-data') {
          final fields = <String, String>{};
          final files = <MultiPartFormDataFileHttpPacket>[];
          for (final e in serialized.entries) {
            final rule = encodingRules[e.key];
            final headers = rule?['headers'];
            final contentType = rule?['contentType'];
          }
          //vars []
          parts = MultiPartBodySerializer.getFormDataParts(
            fields: {
            },
            files: []
          );
        } else {
          parts = [];
        }
        final bodySerializer = MultiPartBodySerializer(
          boundary: resolvedMediaType.parameters['boundary'],
          parts: parts,
        );
        return bodySerializer.bodyBytesStream;
      default:
    }
    //var serialized = v;
    // serialized is guaranteed to be a dart primitive (String, int, List, Map, Uint8List, XFile, XMLElement, etc...)
    //final encoded = serialized;
    //final bytes = ;
  }
}

class DefaultApiPetsMulticontentTestPostRequestAnyAny extends DefaultApiPetsMulticontentTestPostRequest {
  static const specMediaType = r'*/*';

  @override
  String get contentType => specMediaType;

  final Object
? data;

  const DefaultApiPetsMulticontentTestPostRequestAnyAny({
    required this.data,
    
    super.extraHeaders,
    super.extraQueryParameters,
    super.extraCookies,
  });

  @override
  Stream<List<int>> getResolvedBody({
    required Map<String, String> headers,
    Map<String, dynamic> context = const {},
  }) {
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
    final encodingRules = <String, Map<String,dynamic>>{
      
    };

    // Since the user can override mime type at runtime, we need to check the
    // mime type and serialize the model accordingly.
    switch (resolvedMediaType) {
      case MediaType(type: 'application', subtype: 'json'):
        return _stringResult(json.encode(serialized));
      case MediaType(type: 'application', subtype: 'x-www-form-urlencoded'):
        if (serialized is! Map<String, dynamic>) {
          return _stringResult(serialized.toString());
        }
        var result = Uri();

        for (var e in serialized.entries) {
          final rule = encodingRules[e.key];
          final style = rule?['style'] ?? 'form';
          final explode = rule?['explode'] ?? (style == 'form');
          result = OpenApiParameterSerializationQuery.fromStyle(style, explode: explode, parameterName: e.key, allowEmptyValue: false,).expandUri(result, e.value);
        }
        var resultString = result.query.toString();
        if (resultString.startsWith('?')) {
          resultString= resultString.substring(1);
        }
        return _stringResult(resultString);
      case MediaType(type: 'application', subtype: 'xml'):
        break;
      case MediaType(type: 'application', subtype: 'octet-stream'):
        break;
      case MediaType(type: 'multipart'):
        List<HttpPacketMixin> parts;
        if (serialized is! Map<String, dynamic>) {
          throw ArgumentError('The serialized data must be a map in a multipart request.');
        }
        if (resolvedMediaType.subtype == 'form-data') {
          final fields = <String, String>{};
          final files = <MultiPartFormDataFileHttpPacket>[];
          for (final e in serialized.entries) {
            final rule = encodingRules[e.key];
            final headers = rule?['headers'];
            final contentType = rule?['contentType'];
          }
          //vars []
          parts = MultiPartBodySerializer.getFormDataParts(
            fields: {
            },
            files: []
          );
        } else {
          parts = [];
        }
        final bodySerializer = MultiPartBodySerializer(
          boundary: resolvedMediaType.parameters['boundary'],
          parts: parts,
        );
        return bodySerializer.bodyBytesStream;
      default:
    }
    //var serialized = v;
    // serialized is guaranteed to be a dart primitive (String, int, List, Map, Uint8List, XFile, XMLElement, etc...)
    //final encoded = serialized;
    //final bytes = ;
  }
}

class DefaultApiPetsMulticontentTestPostRequestTextAny extends DefaultApiPetsMulticontentTestPostRequest {
  static const specMediaType = r'text/*';

  @override
  String get contentType => specMediaType;

  final 
            String
 data;

  const DefaultApiPetsMulticontentTestPostRequestTextAny({
    required this.data,
    
    super.extraHeaders,
    super.extraQueryParameters,
    super.extraCookies,
  });

  @override
  Stream<List<int>> getResolvedBody({
    required Map<String, String> headers,
    Map<String, dynamic> context = const {},
  }) {
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
    final encodingRules = <String, Map<String,dynamic>>{
      
    };

    // Since the user can override mime type at runtime, we need to check the
    // mime type and serialize the model accordingly.
    switch (resolvedMediaType) {
      case MediaType(type: 'application', subtype: 'json'):
        return _stringResult(json.encode(serialized));
      case MediaType(type: 'application', subtype: 'x-www-form-urlencoded'):
        if (serialized is! Map<String, dynamic>) {
          return _stringResult(serialized.toString());
        }
        var result = Uri();

        for (var e in serialized.entries) {
          final rule = encodingRules[e.key];
          final style = rule?['style'] ?? 'form';
          final explode = rule?['explode'] ?? (style == 'form');
          result = OpenApiParameterSerializationQuery.fromStyle(style, explode: explode, parameterName: e.key, allowEmptyValue: false,).expandUri(result, e.value);
        }
        var resultString = result.query.toString();
        if (resultString.startsWith('?')) {
          resultString= resultString.substring(1);
        }
        return _stringResult(resultString);
      case MediaType(type: 'application', subtype: 'xml'):
        break;
      case MediaType(type: 'application', subtype: 'octet-stream'):
        break;
      case MediaType(type: 'multipart'):
        List<HttpPacketMixin> parts;
        if (serialized is! Map<String, dynamic>) {
          throw ArgumentError('The serialized data must be a map in a multipart request.');
        }
        if (resolvedMediaType.subtype == 'form-data') {
          final fields = <String, String>{};
          final files = <MultiPartFormDataFileHttpPacket>[];
          for (final e in serialized.entries) {
            final rule = encodingRules[e.key];
            final headers = rule?['headers'];
            final contentType = rule?['contentType'];
          }
          //vars []
          parts = MultiPartBodySerializer.getFormDataParts(
            fields: {
            },
            files: []
          );
        } else {
          parts = [];
        }
        final bodySerializer = MultiPartBodySerializer(
          boundary: resolvedMediaType.parameters['boundary'],
          parts: parts,
        );
        return bodySerializer.bodyBytesStream;
      default:
    }
    //var serialized = v;
    // serialized is guaranteed to be a dart primitive (String, int, List, Map, Uint8List, XFile, XMLElement, etc...)
    //final encoded = serialized;
    //final bytes = ;
  }
}

class DefaultApiPetsMulticontentTestPostRequestMultipartFormData extends DefaultApiPetsMulticontentTestPostRequest {
  static const specMediaType = r'multipart/form-data';

  @override
  String get contentType => specMediaType;

  final 
            PetsMulticontentTestPostRequest
 data;

  const DefaultApiPetsMulticontentTestPostRequestMultipartFormData({
    required this.data,
    
    super.extraHeaders,
    super.extraQueryParameters,
    super.extraCookies,
  });

  @override
  Stream<List<int>> getResolvedBody({
    required Map<String, String> headers,
    Map<String, dynamic> context = const {},
  }) {
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
    final encodingRules = <String, Map<String,dynamic>>{
      
      r'address': {
        'style': r'form',
        
        
        'contentType': r'application/json',
      }
      r'profileImages': {
        'style': r'form',
        
        
        
          'headers': <String, String>{
          //r'address': CodegenEncoding{contentType=application/json, headers=[], style=form, explode=false, allowReserved=false, vendorExtensions={}},r'profileImages': CodegenEncoding{contentType=null, headers=[CodegenParameter{isFormParam=false, isQueryParam=false, isPathParam=false, isHeaderParam=true, isCookieParam=false, isBodyParam=false, isContainer=false, isCollectionFormatMulti=false, isPrimitiveType=true, isModel=false, isExplode=false, baseName='hello', paramName='hello', dataType='String', datatypeWithEnum='null', dataFormat='null', collectionFormat='null', description='null', unescapedDescription='null', baseType='null', containerType='null', containerTypeMapped='null', defaultValue='null', enumDefaultValue='null', enumName='null', style='simple', deepObject='false', isMatrix='false', allowEmptyValue='false', example='hello_example', examples='null', jsonSchema='{
  "name" : "hello",
  "in" : "header",
  "style" : "simple",
  "explode" : false,
  "schema" : {
    "type" : "string"
  }
}', isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isAnyType=false, isArray=false, isMap=false, isOptional=false, isFile=false, isEnum=false, isEnumRef=false, _enum=null, allowableValues=null, items=null, mostInnerItems=null, additionalProperties=null, vars=[], requiredVars=[], vendorExtensions={}, hasValidation=false, maxProperties=null, minProperties=null, isNullable=false, isDeprecated=false, required=false, maximum='null', exclusiveMaximum=false, minimum='null', exclusiveMinimum=false, maxLength=null, minLength=null, pattern='null', maxItems=null, minItems=null, uniqueItems=false, uniqueItemsBoolean=null, contentType=null, multipleOf=null, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, schema=CodegenProperty{openApiType='string', baseName='hello', complexType='null', getter='getHello', setter='setHello', description='null', dataType='String', datatypeWithEnum='String', dataFormat='null', name='hello', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.hello;', baseType='String', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='hello', nameInPascalCase='Hello', nameInSnakeCase='HELLO', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}, content=null, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false}], style=form, explode=false, allowReserved=false, vendorExtensions={}},
          }
      }
      
    };

    // Since the user can override mime type at runtime, we need to check the
    // mime type and serialize the model accordingly.
    switch (resolvedMediaType) {
      case MediaType(type: 'application', subtype: 'json'):
        return _stringResult(json.encode(serialized));
      case MediaType(type: 'application', subtype: 'x-www-form-urlencoded'):
        if (serialized is! Map<String, dynamic>) {
          return _stringResult(serialized.toString());
        }
        var result = Uri();

        for (var e in serialized.entries) {
          final rule = encodingRules[e.key];
          final style = rule?['style'] ?? 'form';
          final explode = rule?['explode'] ?? (style == 'form');
          result = OpenApiParameterSerializationQuery.fromStyle(style, explode: explode, parameterName: e.key, allowEmptyValue: false,).expandUri(result, e.value);
        }
        var resultString = result.query.toString();
        if (resultString.startsWith('?')) {
          resultString= resultString.substring(1);
        }
        return _stringResult(resultString);
      case MediaType(type: 'application', subtype: 'xml'):
        break;
      case MediaType(type: 'application', subtype: 'octet-stream'):
        break;
      case MediaType(type: 'multipart'):
        List<HttpPacketMixin> parts;
        if (serialized is! Map<String, dynamic>) {
          throw ArgumentError('The serialized data must be a map in a multipart request.');
        }
        if (resolvedMediaType.subtype == 'form-data') {
          final fields = <String, String>{};
          final files = <MultiPartFormDataFileHttpPacket>[];
          for (final e in serialized.entries) {
            final rule = encodingRules[e.key];
            final headers = rule?['headers'];
            final contentType = rule?['contentType'];
          }
          //vars []
          parts = MultiPartBodySerializer.getFormDataParts(
            fields: {
            },
            files: []
          );
        } else {
          parts = [];
        }
        final bodySerializer = MultiPartBodySerializer(
          boundary: resolvedMediaType.parameters['boundary'],
          parts: parts,
        );
        return bodySerializer.bodyBytesStream;
      default:
    }
    //var serialized = v.serialize();
    // serialized is guaranteed to be a dart primitive (String, int, List, Map, Uint8List, XFile, XMLElement, etc...)
    //final encoded = serialized;
    //final bytes = ;
  }
}


class DefaultApiPetsMulticontentTestPostResponse {
}


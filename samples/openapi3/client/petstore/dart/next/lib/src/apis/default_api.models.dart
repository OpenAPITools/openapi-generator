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
          //final memberEncodings = {name=CodegenEncoding{contentType=application/json, headers=[], style=form, explode=false, allowReserved=false, vendorExtensions={}}};
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
    //final encoded = serialized;
    //final bytes = ;
  }

}

class DefaultApiPetsMulticontentTestPostRequestApplicationXWwwFormUrlencoded extends DefaultApiPetsMulticontentTestPostRequest {
  static const specMediaType = r'application/x-www-form-urlencoded';

  @override
  String get contentType => specMediaType;

  final 
            ShapeInterface
 data;

  const DefaultApiPetsMulticontentTestPostRequestApplicationXWwwFormUrlencoded({
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
    required MediaType resolvedMediaType,
    Map<String, dynamic> context = const {},
  }) {
    //TODO: serialize model, then encode it according to media type.
    final v = data;
    var serialized = v;
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
        var serialized = v;
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
    required MediaType resolvedMediaType,
    Map<String, dynamic> context = const {},
  }) {
    //TODO: serialize model, then encode it according to media type.
    final v = data;
    var serialized = v;
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
        var serialized = v;
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
    required MediaType resolvedMediaType,
    Map<String, dynamic> context = const {},
  }) {
    //TODO: serialize model, then encode it according to media type.
    final v = data;
    var serialized = v;
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
        var serialized = v;
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
          //final memberEncodings = {address=CodegenEncoding{contentType=application/json, headers=[], style=form, explode=false, allowReserved=false, vendorExtensions={}}};
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
    //final encoded = serialized;
    //final bytes = ;
  }

}


class DefaultApiPetsMulticontentTestPostResponse {
}


part of 'fake_classname_tags123_api.dart';


abstract class FakeClassnameTags123ApiTestClassnameRequest {
  static const pathTemplate = r'/fake_classname_test';
  static String method = r'PATCH';

  String get contentType;
  final Map<String, String> extraHeaders;
  final Map<String, String> extraCookies;
  final Map<String, Object /* String | List<String> */> extraQueryParameters;

  

  const factory FakeClassnameTags123ApiTestClassnameRequest.unsafe({
    
    Map<String, String> extraHeaders,
    Map<String, Object> extraQueryParameters,
    Map<String, String> extraCookies,
    Stream<Uint8List>? body,
  }) = FakeClassnameTags123ApiTestClassnameRequestUnsafe;

  const FakeClassnameTags123ApiTestClassnameRequest({

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

/// A version of [FakeClassnameTags123ApiTestClassnameRequest], where you can send arbitrary bytes in the body.
class FakeClassnameTags123ApiTestClassnameRequestUnsafe extends FakeClassnameTags123ApiTestClassnameRequest {
  final Stream<Uint8List>? body;

  @override
  final String contentType;

  const FakeClassnameTags123ApiTestClassnameRequestUnsafe({
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


class FakeClassnameTags123ApiTestClassnameRequestApplicationJson extends FakeClassnameTags123ApiTestClassnameRequest {
  static const specMediaType = r'application/json';

  @override
  String get contentType => specMediaType;

  final 
            Client
 data;

  const FakeClassnameTags123ApiTestClassnameRequestApplicationJson({
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


class FakeClassnameTags123ApiTestClassnameResponse {
}


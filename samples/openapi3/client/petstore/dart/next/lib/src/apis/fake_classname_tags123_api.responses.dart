// ignore_for_file: unnecessary_type_check

part of 'fake_classname_tags123_api.dart';


class FakeClassnameTags123ApiTestClassnameResponse {
  FakeClassnameTags123ApiTestClassnameResponse({
    required this.headers,
    required this.statusCode,
    required this.reasonPhrase,
    required this.context,
    this.bodyBytesStream,
  });

  final Map<String, String> headers;
  final int statusCode;
  final String? reasonPhrase;
  final Map<String, dynamic> context;
  /// This variable is only assigned if other response classes fail to read the response.
  /// Thus, handing the responsibility of reading the response to the user.
  final Stream<List<int>>? bodyBytesStream;

  T split<T>({
    required T Function(FakeClassnameTags123ApiTestClassnameResponse200 response) on200,
    required T Function(FakeClassnameTags123ApiTestClassnameResponse response) other,
  }) {
    return switch (this) {
      FakeClassnameTags123ApiTestClassnameResponse200 response => on200(response),
      _ => other(this),
    };
  }

  static Future<FakeClassnameTags123ApiTestClassnameResponse> fromResponse(HttpResponseBase response, {required Map<String,dynamic> context}) async {
    if (OASNetworkingUtils.matchesStatsuCodePattern(response.statusCode, r'200')) {
      return FakeClassnameTags123ApiTestClassnameResponse200.fromResponse(response, context: context);
    }
    return FakeClassnameTags123ApiTestClassnameResponse(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      context: context,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}

class FakeClassnameTags123ApiTestClassnameResponse200 extends FakeClassnameTags123ApiTestClassnameResponse {
  FakeClassnameTags123ApiTestClassnameResponse200({
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.context,
    super.bodyBytesStream,
  });

  T split200<T>({
    
    required T Function(FakeClassnameTags123ApiTestClassnameResponse200ApplicationJson response) onApplicationJson,
    
    required T Function(FakeClassnameTags123ApiTestClassnameResponse200 response) other,
  }) {
    return switch (this) {
      
      FakeClassnameTags123ApiTestClassnameResponse200ApplicationJson response => onApplicationJson(response),
      
      _ => other(this),
    };
  }

  static Future<FakeClassnameTags123ApiTestClassnameResponse200> fromResponse(HttpResponseBase response, {required Map<String,dynamic> context}) async {
    final headers = response.headers;
    final contentTypeRaw = headers['Content-Type'];
    final contentTypeParsed = contentTypeRaw == null ? null : MediaType.parse(contentTypeRaw);
    if (contentTypeParsed != null) {
      if (OASNetworkingUtils.matchesContentTypePattern(contentTypeParsed, MediaType.parse(r'application/json'))) {
        return FakeClassnameTags123ApiTestClassnameResponse200ApplicationJson.fromResponse(response, contentType: contentTypeParsed, context: context);
      }
    }
    return FakeClassnameTags123ApiTestClassnameResponse200(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      context: context,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}



/// Represent the response when content-type is application/json.
class FakeClassnameTags123ApiTestClassnameResponse200ApplicationJson extends FakeClassnameTags123ApiTestClassnameResponse200 {
  final UndefinedWrapper<
            Client
> body;

  /// The raw result of calling jsonDecode
  final Object? rawJson;

  FakeClassnameTags123ApiTestClassnameResponse200ApplicationJson({
     this.body = const UndefinedWrapper
        .undefined()
,
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.context,
    super.bodyBytesStream,
    this.rawJson,
  });

  static Future<FakeClassnameTags123ApiTestClassnameResponse200ApplicationJson> fromResponse(HttpResponseBase response, {required MediaType contentType, required Map<String,dynamic> context}) async {
    final charset = contentType.parameters['charset'] ?? 'utf-8';
    final encoding = Encoding.getByName(charset) ?? utf8;
    switch (contentType) {
      
      case MediaType(type: 'application', subtype: 'json'):
        final serialized = await encoding.decodeStream(response.bodyBytesStream);
        final v = jsonDecode(serialized);
        if (v == null ? false :
(

    
            Client.canDeserialize(v)
            
)) {
          final res = Client.deserialize
(

    
            v


)


;
          return FakeClassnameTags123ApiTestClassnameResponse200ApplicationJson(
            headers: response.headers,
            statusCode: response.statusCode,
            reasonPhrase: response.reasonPhrase,
            context: context,
            body: UndefinedWrapper(res),
          );
        } else {
          // since we consumed the stream, we need to publish our read result.
          return FakeClassnameTags123ApiTestClassnameResponse200ApplicationJson(
            headers: response.headers,
            statusCode: response.statusCode,
            reasonPhrase: response.reasonPhrase,
            context: context,
            rawJson: v,
          );
        }
      
      
    }
    return FakeClassnameTags123ApiTestClassnameResponse200ApplicationJson(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      context: context,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}





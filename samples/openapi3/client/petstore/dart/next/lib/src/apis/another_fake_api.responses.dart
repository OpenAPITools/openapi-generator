// ignore_for_file: unnecessary_type_check

part of 'another_fake_api.dart';


class AnotherFakeApi$123testSpecialTagsResponse {
  AnotherFakeApi$123testSpecialTagsResponse({
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
    required T Function(AnotherFakeApi$123testSpecialTagsResponse200 response) on200,
    required T Function(AnotherFakeApi$123testSpecialTagsResponse response) other,
  }) {
    return switch (this) {
      AnotherFakeApi$123testSpecialTagsResponse200 response => on200(response),
      _ => other(this),
    };
  }

  static Future<AnotherFakeApi$123testSpecialTagsResponse> fromResponse(HttpResponseBase response, {required Map<String,dynamic> context}) async {
    final matchedResponse = <(PatternMatchResult, Future<AnotherFakeApi$123testSpecialTagsResponse> Function())> [
    (
      OASNetworkingUtils.matchesStatusCodePattern(response.statusCode, r'200'),
      () => AnotherFakeApi$123testSpecialTagsResponse200.fromResponse(response, context: context)
    ),
    ].pickPrioritized();
    if (matchedResponse != null) {
      return matchedResponse();
    }
    return AnotherFakeApi$123testSpecialTagsResponse(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      context: context,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}

class AnotherFakeApi$123testSpecialTagsResponse200 extends AnotherFakeApi$123testSpecialTagsResponse {
  AnotherFakeApi$123testSpecialTagsResponse200({
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.context,
    super.bodyBytesStream,
  });

  T split200<T>({
    
    required T Function(AnotherFakeApi$123testSpecialTagsResponse200ApplicationJson response) onApplicationJson,
    
    required T Function(AnotherFakeApi$123testSpecialTagsResponse200 response) other,
  }) {
    return switch (this) {
      
      AnotherFakeApi$123testSpecialTagsResponse200ApplicationJson response => onApplicationJson(response),
      
      _ => other(this),
    };
  }

  static Future<AnotherFakeApi$123testSpecialTagsResponse200> fromResponse(HttpResponseBase response, {required Map<String,dynamic> context}) async {
    final headers = response.headers;
    final contentTypeRaw = headers['Content-Type'];
    final contentTypeParsed = contentTypeRaw == null ? null : MediaType.parse(contentTypeRaw);
    if (contentTypeParsed != null) {
      final matchedResponse = <(PatternMatchResult, Future<AnotherFakeApi$123testSpecialTagsResponse200> Function())>[
      (
        OASNetworkingUtils.matchesContentTypePattern(contentTypeParsed, MediaType.parse(r'application/json')),
        () => AnotherFakeApi$123testSpecialTagsResponse200ApplicationJson.fromResponse(response, contentType: contentTypeParsed, context: context)
      ),
      ].pickPrioritized();
      if (matchedResponse != null) {
        return matchedResponse();
      }
    }
    return AnotherFakeApi$123testSpecialTagsResponse200(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      context: context,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}



/// Represent the response when content-type is application/json.
class AnotherFakeApi$123testSpecialTagsResponse200ApplicationJson extends AnotherFakeApi$123testSpecialTagsResponse200 {
  final UndefinedWrapper<
            Client
> body;

  /// The raw result of calling jsonDecode
  final Object? rawJson;

  AnotherFakeApi$123testSpecialTagsResponse200ApplicationJson({
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

  static Future<AnotherFakeApi$123testSpecialTagsResponse200ApplicationJson> fromResponse(HttpResponseBase response, {required MediaType contentType, required Map<String,dynamic> context}) async {
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
          return AnotherFakeApi$123testSpecialTagsResponse200ApplicationJson(
            headers: response.headers,
            statusCode: response.statusCode,
            reasonPhrase: response.reasonPhrase,
            context: context,
            body: UndefinedWrapper(res),
          );
        } else {
          // since we consumed the stream, we need to publish our read result.
          return AnotherFakeApi$123testSpecialTagsResponse200ApplicationJson(
            headers: response.headers,
            statusCode: response.statusCode,
            reasonPhrase: response.reasonPhrase,
            context: context,
            rawJson: v,
          );
        }
      
      
    }
    return AnotherFakeApi$123testSpecialTagsResponse200ApplicationJson(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      context: context,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}


class AnotherFakeApiGetParameterArrayNumberResponse {
  AnotherFakeApiGetParameterArrayNumberResponse({
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
    required T Function(AnotherFakeApiGetParameterArrayNumberResponse200 response) on200,
    required T Function(AnotherFakeApiGetParameterArrayNumberResponse response) other,
  }) {
    return switch (this) {
      AnotherFakeApiGetParameterArrayNumberResponse200 response => on200(response),
      _ => other(this),
    };
  }

  static Future<AnotherFakeApiGetParameterArrayNumberResponse> fromResponse(HttpResponseBase response, {required Map<String,dynamic> context}) async {
    final matchedResponse = <(PatternMatchResult, Future<AnotherFakeApiGetParameterArrayNumberResponse> Function())> [
    (
      OASNetworkingUtils.matchesStatusCodePattern(response.statusCode, r'200'),
      () => AnotherFakeApiGetParameterArrayNumberResponse200.fromResponse(response, context: context)
    ),
    ].pickPrioritized();
    if (matchedResponse != null) {
      return matchedResponse();
    }
    return AnotherFakeApiGetParameterArrayNumberResponse(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      context: context,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}

class AnotherFakeApiGetParameterArrayNumberResponse200 extends AnotherFakeApiGetParameterArrayNumberResponse {
  AnotherFakeApiGetParameterArrayNumberResponse200({
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.context,
    super.bodyBytesStream,
  });



  static Future<AnotherFakeApiGetParameterArrayNumberResponse200> fromResponse(HttpResponseBase response, {required Map<String,dynamic> context}) async {
    return AnotherFakeApiGetParameterArrayNumberResponse200(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      context: context,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}




class AnotherFakeApiGetParameterStringNumberResponse {
  AnotherFakeApiGetParameterStringNumberResponse({
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
    required T Function(AnotherFakeApiGetParameterStringNumberResponse200 response) on200,
    required T Function(AnotherFakeApiGetParameterStringNumberResponse response) other,
  }) {
    return switch (this) {
      AnotherFakeApiGetParameterStringNumberResponse200 response => on200(response),
      _ => other(this),
    };
  }

  static Future<AnotherFakeApiGetParameterStringNumberResponse> fromResponse(HttpResponseBase response, {required Map<String,dynamic> context}) async {
    final matchedResponse = <(PatternMatchResult, Future<AnotherFakeApiGetParameterStringNumberResponse> Function())> [
    (
      OASNetworkingUtils.matchesStatusCodePattern(response.statusCode, r'200'),
      () => AnotherFakeApiGetParameterStringNumberResponse200.fromResponse(response, context: context)
    ),
    ].pickPrioritized();
    if (matchedResponse != null) {
      return matchedResponse();
    }
    return AnotherFakeApiGetParameterStringNumberResponse(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      context: context,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}

class AnotherFakeApiGetParameterStringNumberResponse200 extends AnotherFakeApiGetParameterStringNumberResponse {
  AnotherFakeApiGetParameterStringNumberResponse200({
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.context,
    super.bodyBytesStream,
  });



  static Future<AnotherFakeApiGetParameterStringNumberResponse200> fromResponse(HttpResponseBase response, {required Map<String,dynamic> context}) async {
    return AnotherFakeApiGetParameterStringNumberResponse200(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      context: context,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}




class AnotherFakeApiNullRequestBodyResponse {
  AnotherFakeApiNullRequestBodyResponse({
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
    required T Function(AnotherFakeApiNullRequestBodyResponse200 response) on200,
    required T Function(AnotherFakeApiNullRequestBodyResponse response) other,
  }) {
    return switch (this) {
      AnotherFakeApiNullRequestBodyResponse200 response => on200(response),
      _ => other(this),
    };
  }

  static Future<AnotherFakeApiNullRequestBodyResponse> fromResponse(HttpResponseBase response, {required Map<String,dynamic> context}) async {
    final matchedResponse = <(PatternMatchResult, Future<AnotherFakeApiNullRequestBodyResponse> Function())> [
    (
      OASNetworkingUtils.matchesStatusCodePattern(response.statusCode, r'200'),
      () => AnotherFakeApiNullRequestBodyResponse200.fromResponse(response, context: context)
    ),
    ].pickPrioritized();
    if (matchedResponse != null) {
      return matchedResponse();
    }
    return AnotherFakeApiNullRequestBodyResponse(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      context: context,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}

class AnotherFakeApiNullRequestBodyResponse200 extends AnotherFakeApiNullRequestBodyResponse {
  AnotherFakeApiNullRequestBodyResponse200({
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.context,
    super.bodyBytesStream,
  });



  static Future<AnotherFakeApiNullRequestBodyResponse200> fromResponse(HttpResponseBase response, {required Map<String,dynamic> context}) async {
    return AnotherFakeApiNullRequestBodyResponse200(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      context: context,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}







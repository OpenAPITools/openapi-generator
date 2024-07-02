// ignore_for_file: unnecessary_type_check

part of 'default_api.dart';


class DefaultApiFakeAnyOfWIthSameErasureGetResponse {
  DefaultApiFakeAnyOfWIthSameErasureGetResponse({
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
    required T Function(DefaultApiFakeAnyOfWIthSameErasureGetResponse200 response) on200,
    required T Function(DefaultApiFakeAnyOfWIthSameErasureGetResponse response) other,
  }) {
    return switch (this) {
      DefaultApiFakeAnyOfWIthSameErasureGetResponse200 response => on200(response),
      _ => other(this),
    };
  }

  static Future<DefaultApiFakeAnyOfWIthSameErasureGetResponse> fromResponse(HttpResponseBase response, {required Map<String,dynamic> context}) async {
    final matchedResponse = <(PatternMatchResult, Future<DefaultApiFakeAnyOfWIthSameErasureGetResponse> Function())> [
    (
      OASNetworkingUtils.matchesStatusCodePattern(response.statusCode, r'200'),
      () => DefaultApiFakeAnyOfWIthSameErasureGetResponse200.fromResponse(response, context: context)
    ),
    ].pickPrioritized();
    if (matchedResponse != null) {
      return matchedResponse();
    }
    return DefaultApiFakeAnyOfWIthSameErasureGetResponse(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      context: context,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}

class DefaultApiFakeAnyOfWIthSameErasureGetResponse200 extends DefaultApiFakeAnyOfWIthSameErasureGetResponse {
  DefaultApiFakeAnyOfWIthSameErasureGetResponse200({
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.context,
    super.bodyBytesStream,
  });

  T split200<T>({
    
    required T Function(DefaultApiFakeAnyOfWIthSameErasureGetResponse200ApplicationJson response) onApplicationJson,
    
    required T Function(DefaultApiFakeAnyOfWIthSameErasureGetResponse200 response) other,
  }) {
    return switch (this) {
      
      DefaultApiFakeAnyOfWIthSameErasureGetResponse200ApplicationJson response => onApplicationJson(response),
      
      _ => other(this),
    };
  }

  static Future<DefaultApiFakeAnyOfWIthSameErasureGetResponse200> fromResponse(HttpResponseBase response, {required Map<String,dynamic> context}) async {
    final headers = response.headers;
    final contentTypeRaw = headers['Content-Type'];
    final contentTypeParsed = contentTypeRaw == null ? null : MediaType.parse(contentTypeRaw);
    if (contentTypeParsed != null) {
      final matchedResponse = <(PatternMatchResult, Future<DefaultApiFakeAnyOfWIthSameErasureGetResponse200> Function())>[
      (
        OASNetworkingUtils.matchesContentTypePattern(contentTypeParsed, MediaType.parse(r'application/json')),
        () => DefaultApiFakeAnyOfWIthSameErasureGetResponse200ApplicationJson.fromResponse(response, contentType: contentTypeParsed, context: context)
      ),
      ].pickPrioritized();
      if (matchedResponse != null) {
        return matchedResponse();
      }
    }
    return DefaultApiFakeAnyOfWIthSameErasureGetResponse200(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      context: context,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}



/// Represent the response when content-type is application/json.
class DefaultApiFakeAnyOfWIthSameErasureGetResponse200ApplicationJson extends DefaultApiFakeAnyOfWIthSameErasureGetResponse200 {
  final UndefinedWrapper<
            FakeAnyOfWIthSameErasureGet200Response
> body;

  /// The raw result of calling jsonDecode
  final Object? rawJson;

  DefaultApiFakeAnyOfWIthSameErasureGetResponse200ApplicationJson({
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

  static Future<DefaultApiFakeAnyOfWIthSameErasureGetResponse200ApplicationJson> fromResponse(HttpResponseBase response, {required MediaType contentType, required Map<String,dynamic> context}) async {
    final charset = contentType.parameters['charset'] ?? 'utf-8';
    final encoding = Encoding.getByName(charset) ?? utf8;
    switch (contentType) {
      
      case MediaType(type: 'application', subtype: 'json'):
        final serialized = await encoding.decodeStream(response.bodyBytesStream);
        final v = jsonDecode(serialized);
        if (v == null ? false :
(

    
            FakeAnyOfWIthSameErasureGet200Response.canDeserialize(v)
            
)) {
          final res = FakeAnyOfWIthSameErasureGet200Response.deserialize
(

    
            v


)


;
          return DefaultApiFakeAnyOfWIthSameErasureGetResponse200ApplicationJson(
            headers: response.headers,
            statusCode: response.statusCode,
            reasonPhrase: response.reasonPhrase,
            context: context,
            body: UndefinedWrapper(res),
          );
        } else {
          // since we consumed the stream, we need to publish our read result.
          return DefaultApiFakeAnyOfWIthSameErasureGetResponse200ApplicationJson(
            headers: response.headers,
            statusCode: response.statusCode,
            reasonPhrase: response.reasonPhrase,
            context: context,
            rawJson: v,
          );
        }
      
      
    }
    return DefaultApiFakeAnyOfWIthSameErasureGetResponse200ApplicationJson(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      context: context,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}


class DefaultApiFakeOneOfWIthSameErasureGetResponse {
  DefaultApiFakeOneOfWIthSameErasureGetResponse({
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
    required T Function(DefaultApiFakeOneOfWIthSameErasureGetResponse200 response) on200,
    required T Function(DefaultApiFakeOneOfWIthSameErasureGetResponse response) other,
  }) {
    return switch (this) {
      DefaultApiFakeOneOfWIthSameErasureGetResponse200 response => on200(response),
      _ => other(this),
    };
  }

  static Future<DefaultApiFakeOneOfWIthSameErasureGetResponse> fromResponse(HttpResponseBase response, {required Map<String,dynamic> context}) async {
    final matchedResponse = <(PatternMatchResult, Future<DefaultApiFakeOneOfWIthSameErasureGetResponse> Function())> [
    (
      OASNetworkingUtils.matchesStatusCodePattern(response.statusCode, r'200'),
      () => DefaultApiFakeOneOfWIthSameErasureGetResponse200.fromResponse(response, context: context)
    ),
    ].pickPrioritized();
    if (matchedResponse != null) {
      return matchedResponse();
    }
    return DefaultApiFakeOneOfWIthSameErasureGetResponse(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      context: context,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}

class DefaultApiFakeOneOfWIthSameErasureGetResponse200 extends DefaultApiFakeOneOfWIthSameErasureGetResponse {
  DefaultApiFakeOneOfWIthSameErasureGetResponse200({
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.context,
    super.bodyBytesStream,
  });

  T split200<T>({
    
    required T Function(DefaultApiFakeOneOfWIthSameErasureGetResponse200ApplicationJson response) onApplicationJson,
    
    required T Function(DefaultApiFakeOneOfWIthSameErasureGetResponse200 response) other,
  }) {
    return switch (this) {
      
      DefaultApiFakeOneOfWIthSameErasureGetResponse200ApplicationJson response => onApplicationJson(response),
      
      _ => other(this),
    };
  }

  static Future<DefaultApiFakeOneOfWIthSameErasureGetResponse200> fromResponse(HttpResponseBase response, {required Map<String,dynamic> context}) async {
    final headers = response.headers;
    final contentTypeRaw = headers['Content-Type'];
    final contentTypeParsed = contentTypeRaw == null ? null : MediaType.parse(contentTypeRaw);
    if (contentTypeParsed != null) {
      final matchedResponse = <(PatternMatchResult, Future<DefaultApiFakeOneOfWIthSameErasureGetResponse200> Function())>[
      (
        OASNetworkingUtils.matchesContentTypePattern(contentTypeParsed, MediaType.parse(r'application/json')),
        () => DefaultApiFakeOneOfWIthSameErasureGetResponse200ApplicationJson.fromResponse(response, contentType: contentTypeParsed, context: context)
      ),
      ].pickPrioritized();
      if (matchedResponse != null) {
        return matchedResponse();
      }
    }
    return DefaultApiFakeOneOfWIthSameErasureGetResponse200(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      context: context,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}



/// Represent the response when content-type is application/json.
class DefaultApiFakeOneOfWIthSameErasureGetResponse200ApplicationJson extends DefaultApiFakeOneOfWIthSameErasureGetResponse200 {
  final UndefinedWrapper<
            FakeOneOfWIthSameErasureGet200Response
> body;

  /// The raw result of calling jsonDecode
  final Object? rawJson;

  DefaultApiFakeOneOfWIthSameErasureGetResponse200ApplicationJson({
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

  static Future<DefaultApiFakeOneOfWIthSameErasureGetResponse200ApplicationJson> fromResponse(HttpResponseBase response, {required MediaType contentType, required Map<String,dynamic> context}) async {
    final charset = contentType.parameters['charset'] ?? 'utf-8';
    final encoding = Encoding.getByName(charset) ?? utf8;
    switch (contentType) {
      
      case MediaType(type: 'application', subtype: 'json'):
        final serialized = await encoding.decodeStream(response.bodyBytesStream);
        final v = jsonDecode(serialized);
        if (v == null ? false :
(

    
            FakeOneOfWIthSameErasureGet200Response.canDeserialize(v)
            
)) {
          final res = FakeOneOfWIthSameErasureGet200Response.deserialize
(

    
            v


)


;
          return DefaultApiFakeOneOfWIthSameErasureGetResponse200ApplicationJson(
            headers: response.headers,
            statusCode: response.statusCode,
            reasonPhrase: response.reasonPhrase,
            context: context,
            body: UndefinedWrapper(res),
          );
        } else {
          // since we consumed the stream, we need to publish our read result.
          return DefaultApiFakeOneOfWIthSameErasureGetResponse200ApplicationJson(
            headers: response.headers,
            statusCode: response.statusCode,
            reasonPhrase: response.reasonPhrase,
            context: context,
            rawJson: v,
          );
        }
      
      
    }
    return DefaultApiFakeOneOfWIthSameErasureGetResponse200ApplicationJson(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      context: context,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}


class DefaultApiFooGetResponse {
  DefaultApiFooGetResponse({
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
    required T Function(DefaultApiFooGetResponse0 response) on0,
    required T Function(DefaultApiFooGetResponse response) other,
  }) {
    return switch (this) {
      DefaultApiFooGetResponse0 response => on0(response),
      _ => other(this),
    };
  }

  static Future<DefaultApiFooGetResponse> fromResponse(HttpResponseBase response, {required Map<String,dynamic> context}) async {
    final matchedResponse = <(PatternMatchResult, Future<DefaultApiFooGetResponse> Function())> [
    (
      OASNetworkingUtils.matchesStatusCodePattern(response.statusCode, r'0'),
      () => DefaultApiFooGetResponse0.fromResponse(response, context: context)
    ),
    ].pickPrioritized();
    if (matchedResponse != null) {
      return matchedResponse();
    }
    return DefaultApiFooGetResponse(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      context: context,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}

class DefaultApiFooGetResponse0 extends DefaultApiFooGetResponse {
  DefaultApiFooGetResponse0({
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.context,
    super.bodyBytesStream,
  });

  T split0<T>({
    
    required T Function(DefaultApiFooGetResponse0ApplicationJson response) onApplicationJson,
    
    required T Function(DefaultApiFooGetResponse0 response) other,
  }) {
    return switch (this) {
      
      DefaultApiFooGetResponse0ApplicationJson response => onApplicationJson(response),
      
      _ => other(this),
    };
  }

  static Future<DefaultApiFooGetResponse0> fromResponse(HttpResponseBase response, {required Map<String,dynamic> context}) async {
    final headers = response.headers;
    final contentTypeRaw = headers['Content-Type'];
    final contentTypeParsed = contentTypeRaw == null ? null : MediaType.parse(contentTypeRaw);
    if (contentTypeParsed != null) {
      final matchedResponse = <(PatternMatchResult, Future<DefaultApiFooGetResponse0> Function())>[
      (
        OASNetworkingUtils.matchesContentTypePattern(contentTypeParsed, MediaType.parse(r'application/json')),
        () => DefaultApiFooGetResponse0ApplicationJson.fromResponse(response, contentType: contentTypeParsed, context: context)
      ),
      ].pickPrioritized();
      if (matchedResponse != null) {
        return matchedResponse();
      }
    }
    return DefaultApiFooGetResponse0(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      context: context,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}



/// Represent the response when content-type is application/json.
class DefaultApiFooGetResponse0ApplicationJson extends DefaultApiFooGetResponse0 {
  final UndefinedWrapper<
            FooGetDefaultResponse
> body;

  /// The raw result of calling jsonDecode
  final Object? rawJson;

  DefaultApiFooGetResponse0ApplicationJson({
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

  static Future<DefaultApiFooGetResponse0ApplicationJson> fromResponse(HttpResponseBase response, {required MediaType contentType, required Map<String,dynamic> context}) async {
    final charset = contentType.parameters['charset'] ?? 'utf-8';
    final encoding = Encoding.getByName(charset) ?? utf8;
    switch (contentType) {
      
      case MediaType(type: 'application', subtype: 'json'):
        final serialized = await encoding.decodeStream(response.bodyBytesStream);
        final v = jsonDecode(serialized);
        if (v == null ? false :
(

    
            FooGetDefaultResponse.canDeserialize(v)
            
)) {
          final res = FooGetDefaultResponse.deserialize
(

    
            v


)


;
          return DefaultApiFooGetResponse0ApplicationJson(
            headers: response.headers,
            statusCode: response.statusCode,
            reasonPhrase: response.reasonPhrase,
            context: context,
            body: UndefinedWrapper(res),
          );
        } else {
          // since we consumed the stream, we need to publish our read result.
          return DefaultApiFooGetResponse0ApplicationJson(
            headers: response.headers,
            statusCode: response.statusCode,
            reasonPhrase: response.reasonPhrase,
            context: context,
            rawJson: v,
          );
        }
      
      
    }
    return DefaultApiFooGetResponse0ApplicationJson(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      context: context,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}


class DefaultApiPetsMulticontentTestPostResponse {
  DefaultApiPetsMulticontentTestPostResponse({
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
    required T Function(DefaultApiPetsMulticontentTestPostResponse201 response) on201,
    required T Function(DefaultApiPetsMulticontentTestPostResponse response) other,
  }) {
    return switch (this) {
      DefaultApiPetsMulticontentTestPostResponse201 response => on201(response),
      _ => other(this),
    };
  }

  static Future<DefaultApiPetsMulticontentTestPostResponse> fromResponse(HttpResponseBase response, {required Map<String,dynamic> context}) async {
    final matchedResponse = <(PatternMatchResult, Future<DefaultApiPetsMulticontentTestPostResponse> Function())> [
    (
      OASNetworkingUtils.matchesStatusCodePattern(response.statusCode, r'201'),
      () => DefaultApiPetsMulticontentTestPostResponse201.fromResponse(response, context: context)
    ),
    ].pickPrioritized();
    if (matchedResponse != null) {
      return matchedResponse();
    }
    return DefaultApiPetsMulticontentTestPostResponse(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      context: context,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}

class DefaultApiPetsMulticontentTestPostResponse201 extends DefaultApiPetsMulticontentTestPostResponse {
  DefaultApiPetsMulticontentTestPostResponse201({
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.context,
    super.bodyBytesStream,
  });



  static Future<DefaultApiPetsMulticontentTestPostResponse201> fromResponse(HttpResponseBase response, {required Map<String,dynamic> context}) async {
    return DefaultApiPetsMulticontentTestPostResponse201(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      context: context,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}







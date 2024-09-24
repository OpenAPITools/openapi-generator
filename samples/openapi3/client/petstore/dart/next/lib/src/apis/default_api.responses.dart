// ignore_for_file: unnecessary_type_check, unnecessary_null_comparison, unnecessary_cast

part of 'default_api.dart';


class DefaultApiFakeAnyOfWIthSameErasureGetResponse {
  DefaultApiFakeAnyOfWIthSameErasureGetResponse({
    required this.headers,
    required this.statusCode,
    required this.reasonPhrase,
    required this.userContext,
    this.bodyBytesStream,
  });

  final Map<String, String> headers;
  final int statusCode;
  final String? reasonPhrase;
  final Map<String, dynamic> userContext;
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

  static Future<DefaultApiFakeAnyOfWIthSameErasureGetResponse> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    final matchedResponse = <(PatternMatchResult, Future<DefaultApiFakeAnyOfWIthSameErasureGetResponse> Function())> [
    (
      OASNetworkingUtils.matchesStatusCodePattern(response.statusCode, r'200'),
      () => DefaultApiFakeAnyOfWIthSameErasureGetResponse200.fromResponse(response, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
    ),
    ].pickPrioritized();
    if (matchedResponse != null) {
      return matchedResponse();
    }
    return DefaultApiFakeAnyOfWIthSameErasureGetResponse(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}



class DefaultApiFakeAnyOfWIthSameErasureGetResponse200 extends DefaultApiFakeAnyOfWIthSameErasureGetResponse {
  DefaultApiFakeAnyOfWIthSameErasureGetResponse200({
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
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

  static Future<DefaultApiFakeAnyOfWIthSameErasureGetResponse200> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    final headers = response.headers;
    final contentTypeRaw = headers['Content-Type'];
    final contentTypeParsed = contentTypeRaw == null ? null : MediaType.parse(contentTypeRaw);
    if (contentTypeParsed != null) {
      final matchedResponse = <(PatternMatchResult, Future<DefaultApiFakeAnyOfWIthSameErasureGetResponse200> Function())>[
      (
        OASNetworkingUtils.matchesContentTypePattern(contentTypeParsed, MediaType.parse(r'application/json')),
        () => DefaultApiFakeAnyOfWIthSameErasureGetResponse200ApplicationJson.fromResponse(response, contentType: contentTypeParsed, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
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
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}








/// Represent the response when content-type is application/json.
class DefaultApiFakeAnyOfWIthSameErasureGetResponse200ApplicationJson extends DefaultApiFakeAnyOfWIthSameErasureGetResponse200 {
  final UndefinedWrapper<
            FakeAnyOfWIthSameErasureGet200Response
> body;

  static const bodyReflection = XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                FakeAnyOfWIthSameErasureGet200Response.$reflection
        
,
)
;

  /// The raw result of calling jsonDecode
  final Object? rawJson;

  DefaultApiFakeAnyOfWIthSameErasureGetResponse200ApplicationJson({
    this.body = const UndefinedWrapper.undefined(),
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
    this.rawJson,
  });

  static Future<DefaultApiFakeAnyOfWIthSameErasureGetResponse200ApplicationJson> fromResponse(HttpResponseBase response, {required MediaType contentType, required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
  final encodingRules = <String, PropertyEncodingRule>{
      
    };

    final context = wireSerializationOptions.createSerializationContext(contentType);

    switch (contentType) {
      case MediaType(type: 'application', subtype: 'json'):
        final encoding = OASNetworkingUtils.getEncodingOrDefault(contentType);
        final serialized = await encoding.decodeStream(response.bodyBytesStream);
        final v = jsonDecode(serialized);
        if (bodyReflection.canDeserializeFunction(v, context)) {
          final res = bodyReflection.deserializeFunction(v, context);
          return DefaultApiFakeAnyOfWIthSameErasureGetResponse200ApplicationJson(
            headers: response.headers,
            statusCode: response.statusCode,
            reasonPhrase: response.reasonPhrase,
            userContext: userContext,
            body: UndefinedWrapper(res),
            
          );
        } else {
          // since we consumed the stream, we need to publish our read result.
          return DefaultApiFakeAnyOfWIthSameErasureGetResponse200ApplicationJson(
            headers: response.headers,
            statusCode: response.statusCode,
            reasonPhrase: response.reasonPhrase,
            userContext: userContext,
            rawJson: v,
            
          );
        }
      default:
    }
    return DefaultApiFakeAnyOfWIthSameErasureGetResponse200ApplicationJson(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
      
    );
  }
}


class DefaultApiFakeOneOfWIthSameErasureGetResponse {
  DefaultApiFakeOneOfWIthSameErasureGetResponse({
    required this.headers,
    required this.statusCode,
    required this.reasonPhrase,
    required this.userContext,
    this.bodyBytesStream,
  });

  final Map<String, String> headers;
  final int statusCode;
  final String? reasonPhrase;
  final Map<String, dynamic> userContext;
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

  static Future<DefaultApiFakeOneOfWIthSameErasureGetResponse> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    final matchedResponse = <(PatternMatchResult, Future<DefaultApiFakeOneOfWIthSameErasureGetResponse> Function())> [
    (
      OASNetworkingUtils.matchesStatusCodePattern(response.statusCode, r'200'),
      () => DefaultApiFakeOneOfWIthSameErasureGetResponse200.fromResponse(response, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
    ),
    ].pickPrioritized();
    if (matchedResponse != null) {
      return matchedResponse();
    }
    return DefaultApiFakeOneOfWIthSameErasureGetResponse(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}



class DefaultApiFakeOneOfWIthSameErasureGetResponse200 extends DefaultApiFakeOneOfWIthSameErasureGetResponse {
  DefaultApiFakeOneOfWIthSameErasureGetResponse200({
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
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

  static Future<DefaultApiFakeOneOfWIthSameErasureGetResponse200> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    final headers = response.headers;
    final contentTypeRaw = headers['Content-Type'];
    final contentTypeParsed = contentTypeRaw == null ? null : MediaType.parse(contentTypeRaw);
    if (contentTypeParsed != null) {
      final matchedResponse = <(PatternMatchResult, Future<DefaultApiFakeOneOfWIthSameErasureGetResponse200> Function())>[
      (
        OASNetworkingUtils.matchesContentTypePattern(contentTypeParsed, MediaType.parse(r'application/json')),
        () => DefaultApiFakeOneOfWIthSameErasureGetResponse200ApplicationJson.fromResponse(response, contentType: contentTypeParsed, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
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
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}








/// Represent the response when content-type is application/json.
class DefaultApiFakeOneOfWIthSameErasureGetResponse200ApplicationJson extends DefaultApiFakeOneOfWIthSameErasureGetResponse200 {
  final UndefinedWrapper<
            FakeOneOfWIthSameErasureGet200Response
> body;

  static const bodyReflection = XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                FakeOneOfWIthSameErasureGet200Response.$reflection
        
,
)
;

  /// The raw result of calling jsonDecode
  final Object? rawJson;

  DefaultApiFakeOneOfWIthSameErasureGetResponse200ApplicationJson({
    this.body = const UndefinedWrapper.undefined(),
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
    this.rawJson,
  });

  static Future<DefaultApiFakeOneOfWIthSameErasureGetResponse200ApplicationJson> fromResponse(HttpResponseBase response, {required MediaType contentType, required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
  final encodingRules = <String, PropertyEncodingRule>{
      
    };

    final context = wireSerializationOptions.createSerializationContext(contentType);

    switch (contentType) {
      case MediaType(type: 'application', subtype: 'json'):
        final encoding = OASNetworkingUtils.getEncodingOrDefault(contentType);
        final serialized = await encoding.decodeStream(response.bodyBytesStream);
        final v = jsonDecode(serialized);
        if (bodyReflection.canDeserializeFunction(v, context)) {
          final res = bodyReflection.deserializeFunction(v, context);
          return DefaultApiFakeOneOfWIthSameErasureGetResponse200ApplicationJson(
            headers: response.headers,
            statusCode: response.statusCode,
            reasonPhrase: response.reasonPhrase,
            userContext: userContext,
            body: UndefinedWrapper(res),
            
          );
        } else {
          // since we consumed the stream, we need to publish our read result.
          return DefaultApiFakeOneOfWIthSameErasureGetResponse200ApplicationJson(
            headers: response.headers,
            statusCode: response.statusCode,
            reasonPhrase: response.reasonPhrase,
            userContext: userContext,
            rawJson: v,
            
          );
        }
      default:
    }
    return DefaultApiFakeOneOfWIthSameErasureGetResponse200ApplicationJson(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
      
    );
  }
}


class DefaultApiFooGetResponse {
  DefaultApiFooGetResponse({
    required this.headers,
    required this.statusCode,
    required this.reasonPhrase,
    required this.userContext,
    this.bodyBytesStream,
  });

  final Map<String, String> headers;
  final int statusCode;
  final String? reasonPhrase;
  final Map<String, dynamic> userContext;
  /// This variable is only assigned if other response classes fail to read the response.
  /// Thus, handing the responsibility of reading the response to the user.
  final Stream<List<int>>? bodyBytesStream;

  T split<T>({
    required T Function(DefaultApiFooGetResponseDefault response) onDefault,
    required T Function(DefaultApiFooGetResponse response) other,
  }) {
    return switch (this) {
      DefaultApiFooGetResponseDefault response => onDefault(response),
      _ => other(this),
    };
  }

  static Future<DefaultApiFooGetResponse> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    final matchedResponse = <(PatternMatchResult, Future<DefaultApiFooGetResponse> Function())> [
    (
      OASNetworkingUtils.matchesStatusCodePattern(response.statusCode, r'default'),
      () => DefaultApiFooGetResponseDefault.fromResponse(response, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
    ),
    ].pickPrioritized();
    if (matchedResponse != null) {
      return matchedResponse();
    }
    return DefaultApiFooGetResponse(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}



class DefaultApiFooGetResponseDefault extends DefaultApiFooGetResponse {
  DefaultApiFooGetResponseDefault({
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
  });


  T splitDefault<T>({
    
    required T Function(DefaultApiFooGetResponseDefaultApplicationJson response) onApplicationJson,
    
    required T Function(DefaultApiFooGetResponseDefault response) other,
  }) {
    return switch (this) {
      
      DefaultApiFooGetResponseDefaultApplicationJson response => onApplicationJson(response),
      
      _ => other(this),
    };
  }

  static Future<DefaultApiFooGetResponseDefault> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    final headers = response.headers;
    final contentTypeRaw = headers['Content-Type'];
    final contentTypeParsed = contentTypeRaw == null ? null : MediaType.parse(contentTypeRaw);
    if (contentTypeParsed != null) {
      final matchedResponse = <(PatternMatchResult, Future<DefaultApiFooGetResponseDefault> Function())>[
      (
        OASNetworkingUtils.matchesContentTypePattern(contentTypeParsed, MediaType.parse(r'application/json')),
        () => DefaultApiFooGetResponseDefaultApplicationJson.fromResponse(response, contentType: contentTypeParsed, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
      ),
      ].pickPrioritized();
      if (matchedResponse != null) {
        return matchedResponse();
      }
    }
    return DefaultApiFooGetResponseDefault(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}








/// Represent the response when content-type is application/json.
class DefaultApiFooGetResponseDefaultApplicationJson extends DefaultApiFooGetResponseDefault {
  final UndefinedWrapper<
            FooGetDefaultResponse
> body;

  static const bodyReflection = XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                FooGetDefaultResponse.$reflection
        
,
)
;

  /// The raw result of calling jsonDecode
  final Object? rawJson;

  DefaultApiFooGetResponseDefaultApplicationJson({
    this.body = const UndefinedWrapper.undefined(),
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
    this.rawJson,
  });

  static Future<DefaultApiFooGetResponseDefaultApplicationJson> fromResponse(HttpResponseBase response, {required MediaType contentType, required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
  final encodingRules = <String, PropertyEncodingRule>{
      
    };

    final context = wireSerializationOptions.createSerializationContext(contentType);

    switch (contentType) {
      case MediaType(type: 'application', subtype: 'json'):
        final encoding = OASNetworkingUtils.getEncodingOrDefault(contentType);
        final serialized = await encoding.decodeStream(response.bodyBytesStream);
        final v = jsonDecode(serialized);
        if (bodyReflection.canDeserializeFunction(v, context)) {
          final res = bodyReflection.deserializeFunction(v, context);
          return DefaultApiFooGetResponseDefaultApplicationJson(
            headers: response.headers,
            statusCode: response.statusCode,
            reasonPhrase: response.reasonPhrase,
            userContext: userContext,
            body: UndefinedWrapper(res),
            
          );
        } else {
          // since we consumed the stream, we need to publish our read result.
          return DefaultApiFooGetResponseDefaultApplicationJson(
            headers: response.headers,
            statusCode: response.statusCode,
            reasonPhrase: response.reasonPhrase,
            userContext: userContext,
            rawJson: v,
            
          );
        }
      default:
    }
    return DefaultApiFooGetResponseDefaultApplicationJson(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
      
    );
  }
}


class DefaultApiPetsMulticontentTestPostResponse {
  DefaultApiPetsMulticontentTestPostResponse({
    required this.headers,
    required this.statusCode,
    required this.reasonPhrase,
    required this.userContext,
    this.bodyBytesStream,
  });

  final Map<String, String> headers;
  final int statusCode;
  final String? reasonPhrase;
  final Map<String, dynamic> userContext;
  /// This variable is only assigned if other response classes fail to read the response.
  /// Thus, handing the responsibility of reading the response to the user.
  final Stream<List<int>>? bodyBytesStream;

  T split<T>({
    required T Function(DefaultApiPetsMulticontentTestPostResponse200 response) on200,
    required T Function(DefaultApiPetsMulticontentTestPostResponse201 response) on201,
    required T Function(DefaultApiPetsMulticontentTestPostResponse2XX response) on2XX,
    required T Function(DefaultApiPetsMulticontentTestPostResponseDefault response) onDefault,
    required T Function(DefaultApiPetsMulticontentTestPostResponse response) other,
  }) {
    return switch (this) {
      DefaultApiPetsMulticontentTestPostResponse200 response => on200(response),
      DefaultApiPetsMulticontentTestPostResponse201 response => on201(response),
      DefaultApiPetsMulticontentTestPostResponse2XX response => on2XX(response),
      DefaultApiPetsMulticontentTestPostResponseDefault response => onDefault(response),
      _ => other(this),
    };
  }

  static Future<DefaultApiPetsMulticontentTestPostResponse> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    final matchedResponse = <(PatternMatchResult, Future<DefaultApiPetsMulticontentTestPostResponse> Function())> [
    (
      OASNetworkingUtils.matchesStatusCodePattern(response.statusCode, r'200'),
      () => DefaultApiPetsMulticontentTestPostResponse200.fromResponse(response, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
    ),
    (
      OASNetworkingUtils.matchesStatusCodePattern(response.statusCode, r'201'),
      () => DefaultApiPetsMulticontentTestPostResponse201.fromResponse(response, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
    ),
    (
      OASNetworkingUtils.matchesStatusCodePattern(response.statusCode, r'2XX'),
      () => DefaultApiPetsMulticontentTestPostResponse2XX.fromResponse(response, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
    ),
    (
      OASNetworkingUtils.matchesStatusCodePattern(response.statusCode, r'default'),
      () => DefaultApiPetsMulticontentTestPostResponseDefault.fromResponse(response, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
    ),
    ].pickPrioritized();
    if (matchedResponse != null) {
      return matchedResponse();
    }
    return DefaultApiPetsMulticontentTestPostResponse(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}



class DefaultApiPetsMulticontentTestPostResponse200 extends DefaultApiPetsMulticontentTestPostResponse {
  DefaultApiPetsMulticontentTestPostResponse200({
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
  });


  T split200<T>({
    
    required T Function(DefaultApiPetsMulticontentTestPostResponse200TextPlain response) onTextPlain,
    
    required T Function(DefaultApiPetsMulticontentTestPostResponse200AnyAny response) onAnyAny,
    
    required T Function(DefaultApiPetsMulticontentTestPostResponse200TextAny response) onTextAny,
    
    required T Function(DefaultApiPetsMulticontentTestPostResponse200MultipartFormData response) onMultipartFormData,
    
    required T Function(DefaultApiPetsMulticontentTestPostResponse200 response) other,
  }) {
    return switch (this) {
      
      DefaultApiPetsMulticontentTestPostResponse200TextPlain response => onTextPlain(response),
      
      DefaultApiPetsMulticontentTestPostResponse200AnyAny response => onAnyAny(response),
      
      DefaultApiPetsMulticontentTestPostResponse200TextAny response => onTextAny(response),
      
      DefaultApiPetsMulticontentTestPostResponse200MultipartFormData response => onMultipartFormData(response),
      
      _ => other(this),
    };
  }

  static Future<DefaultApiPetsMulticontentTestPostResponse200> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    final headers = response.headers;
    final contentTypeRaw = headers['Content-Type'];
    final contentTypeParsed = contentTypeRaw == null ? null : MediaType.parse(contentTypeRaw);
    if (contentTypeParsed != null) {
      final matchedResponse = <(PatternMatchResult, Future<DefaultApiPetsMulticontentTestPostResponse200> Function())>[
      (
        OASNetworkingUtils.matchesContentTypePattern(contentTypeParsed, MediaType.parse(r'text/plain')),
        () => DefaultApiPetsMulticontentTestPostResponse200TextPlain.fromResponse(response, contentType: contentTypeParsed, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
      ),
      (
        OASNetworkingUtils.matchesContentTypePattern(contentTypeParsed, MediaType.parse(r'*/*')),
        () => DefaultApiPetsMulticontentTestPostResponse200AnyAny.fromResponse(response, contentType: contentTypeParsed, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
      ),
      (
        OASNetworkingUtils.matchesContentTypePattern(contentTypeParsed, MediaType.parse(r'text/*')),
        () => DefaultApiPetsMulticontentTestPostResponse200TextAny.fromResponse(response, contentType: contentTypeParsed, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
      ),
      (
        OASNetworkingUtils.matchesContentTypePattern(contentTypeParsed, MediaType.parse(r'multipart/form-data')),
        () => DefaultApiPetsMulticontentTestPostResponse200MultipartFormData.fromResponse(response, contentType: contentTypeParsed, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
      ),
      ].pickPrioritized();
      if (matchedResponse != null) {
        return matchedResponse();
      }
    }
    return DefaultApiPetsMulticontentTestPostResponse200(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}








/// Represent the response when content-type is text/plain.
class DefaultApiPetsMulticontentTestPostResponse200TextPlain extends DefaultApiPetsMulticontentTestPostResponse200 {
  final UndefinedWrapper<
            int
> body;

  static const bodyReflection = XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forint
        
,
)
;

  /// The raw result of decoding bytes to text.
  final String? rawText;

  DefaultApiPetsMulticontentTestPostResponse200TextPlain({
    this.body = const UndefinedWrapper.undefined(),
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
    this.rawText,
  });

  static Future<DefaultApiPetsMulticontentTestPostResponse200TextPlain> fromResponse(HttpResponseBase response, {required MediaType contentType, required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
  final encodingRules = <String, PropertyEncodingRule>{
      
    };

    final context = wireSerializationOptions.createSerializationContext(contentType);

    switch (contentType) {
      case MediaType(type: 'text'):
        final encoding = OASNetworkingUtils.getEncodingOrDefault(contentType);
        final serialized = await encoding.decodeStream(response.bodyBytesStream);
        final v = serialized;
        if (bodyReflection.canDeserializeFunction(v, context)) {
          final res = bodyReflection.deserializeFunction(v, context);
          return DefaultApiPetsMulticontentTestPostResponse200TextPlain(
            headers: response.headers,
            statusCode: response.statusCode,
            reasonPhrase: response.reasonPhrase,
            userContext: userContext,
            body: UndefinedWrapper(res),
            
          );
        } else {
          // since we consumed the stream, we need to publish our read result.
          return DefaultApiPetsMulticontentTestPostResponse200TextPlain(
            headers: response.headers,
            statusCode: response.statusCode,
            reasonPhrase: response.reasonPhrase,
            userContext: userContext,
            rawText: v,
            
          );
        }
      default:
    }
    return DefaultApiPetsMulticontentTestPostResponse200TextPlain(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
      
    );
  }
}






/// Represent the response when content-type is */*.
class DefaultApiPetsMulticontentTestPostResponse200AnyAny extends DefaultApiPetsMulticontentTestPostResponse200 {
  final UndefinedWrapper<Object
?> body;

  static const bodyReflection = XmlReflectionWrapper(
    xml: XmlReflection(
),
    NullableReflection(ObjectReflection()
),
)
;


  DefaultApiPetsMulticontentTestPostResponse200AnyAny({
    this.body = const UndefinedWrapper.undefined(),
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
  });

  static Future<DefaultApiPetsMulticontentTestPostResponse200AnyAny> fromResponse(HttpResponseBase response, {required MediaType contentType, required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
  final encodingRules = <String, PropertyEncodingRule>{
      
    };

    final context = wireSerializationOptions.createSerializationContext(contentType);

    switch (contentType) {
      default:
    }
    return DefaultApiPetsMulticontentTestPostResponse200AnyAny(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
      
    );
  }
}






/// Represent the response when content-type is text/*.
class DefaultApiPetsMulticontentTestPostResponse200TextAny extends DefaultApiPetsMulticontentTestPostResponse200 {
  final UndefinedWrapper<
            String
> body;

  static const bodyReflection = XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forString
        
,
)
;

  /// The raw result of decoding bytes to text.
  final String? rawText;

  DefaultApiPetsMulticontentTestPostResponse200TextAny({
    this.body = const UndefinedWrapper.undefined(),
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
    this.rawText,
  });

  static Future<DefaultApiPetsMulticontentTestPostResponse200TextAny> fromResponse(HttpResponseBase response, {required MediaType contentType, required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
  final encodingRules = <String, PropertyEncodingRule>{
      
    };

    final context = wireSerializationOptions.createSerializationContext(contentType);

    switch (contentType) {
      case MediaType(type: 'text'):
        final encoding = OASNetworkingUtils.getEncodingOrDefault(contentType);
        final serialized = await encoding.decodeStream(response.bodyBytesStream);
        final v = serialized;
        if (bodyReflection.canDeserializeFunction(v, context)) {
          final res = bodyReflection.deserializeFunction(v, context);
          return DefaultApiPetsMulticontentTestPostResponse200TextAny(
            headers: response.headers,
            statusCode: response.statusCode,
            reasonPhrase: response.reasonPhrase,
            userContext: userContext,
            body: UndefinedWrapper(res),
            
          );
        } else {
          // since we consumed the stream, we need to publish our read result.
          return DefaultApiPetsMulticontentTestPostResponse200TextAny(
            headers: response.headers,
            statusCode: response.statusCode,
            reasonPhrase: response.reasonPhrase,
            userContext: userContext,
            rawText: v,
            
          );
        }
      default:
    }
    return DefaultApiPetsMulticontentTestPostResponse200TextAny(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
      
    );
  }
}






/// Represent the response when content-type is multipart/form-data.
class DefaultApiPetsMulticontentTestPostResponse200MultipartFormData extends DefaultApiPetsMulticontentTestPostResponse200 {
  final UndefinedWrapper<
            PetsMulticontentTestPostRequest
> body;

  static const bodyReflection = XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PetsMulticontentTestPostRequest.$reflection
        
,
)
;


  DefaultApiPetsMulticontentTestPostResponse200MultipartFormData({
    this.body = const UndefinedWrapper.undefined(),
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
  });

  static Future<DefaultApiPetsMulticontentTestPostResponse200MultipartFormData> fromResponse(HttpResponseBase response, {required MediaType contentType, required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
  final encodingRules = <String, PropertyEncodingRule>{
      
      r'address': PropertyEncodingRule(
        style: r'form',
        
        
        contentType: MediaType.parse(r'application/json'),
      ),
      r'profileImages': PropertyEncodingRule(
        style: r'form',
        
        
        
      ),
      
    };

    final context = wireSerializationOptions.createSerializationContext(contentType);

    switch (contentType) {
      default:
    }
    return DefaultApiPetsMulticontentTestPostResponse200MultipartFormData(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
      
    );
  }
}



class DefaultApiPetsMulticontentTestPostResponse201 extends DefaultApiPetsMulticontentTestPostResponse {
  DefaultApiPetsMulticontentTestPostResponse201({
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
  });


  T split201<T>({
    
    required T Function(DefaultApiPetsMulticontentTestPostResponse201ApplicationJson response) onApplicationJson,
    
    required T Function(DefaultApiPetsMulticontentTestPostResponse201ApplicationXml response) onApplicationXml,
    
    required T Function(DefaultApiPetsMulticontentTestPostResponse201ApplicationXWwwFormUrlencoded response) onApplicationXWwwFormUrlencoded,
    
    required T Function(DefaultApiPetsMulticontentTestPostResponse201 response) other,
  }) {
    return switch (this) {
      
      DefaultApiPetsMulticontentTestPostResponse201ApplicationJson response => onApplicationJson(response),
      
      DefaultApiPetsMulticontentTestPostResponse201ApplicationXml response => onApplicationXml(response),
      
      DefaultApiPetsMulticontentTestPostResponse201ApplicationXWwwFormUrlencoded response => onApplicationXWwwFormUrlencoded(response),
      
      _ => other(this),
    };
  }

  static Future<DefaultApiPetsMulticontentTestPostResponse201> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    final headers = response.headers;
    final contentTypeRaw = headers['Content-Type'];
    final contentTypeParsed = contentTypeRaw == null ? null : MediaType.parse(contentTypeRaw);
    if (contentTypeParsed != null) {
      final matchedResponse = <(PatternMatchResult, Future<DefaultApiPetsMulticontentTestPostResponse201> Function())>[
      (
        OASNetworkingUtils.matchesContentTypePattern(contentTypeParsed, MediaType.parse(r'application/json')),
        () => DefaultApiPetsMulticontentTestPostResponse201ApplicationJson.fromResponse(response, contentType: contentTypeParsed, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
      ),
      (
        OASNetworkingUtils.matchesContentTypePattern(contentTypeParsed, MediaType.parse(r'application/xml')),
        () => DefaultApiPetsMulticontentTestPostResponse201ApplicationXml.fromResponse(response, contentType: contentTypeParsed, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
      ),
      (
        OASNetworkingUtils.matchesContentTypePattern(contentTypeParsed, MediaType.parse(r'application/x-www-form-urlencoded')),
        () => DefaultApiPetsMulticontentTestPostResponse201ApplicationXWwwFormUrlencoded.fromResponse(response, contentType: contentTypeParsed, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
      ),
      ].pickPrioritized();
      if (matchedResponse != null) {
        return matchedResponse();
      }
    }
    return DefaultApiPetsMulticontentTestPostResponse201(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}








/// Represent the response when content-type is application/json.
class DefaultApiPetsMulticontentTestPostResponse201ApplicationJson extends DefaultApiPetsMulticontentTestPostResponse201 {
  final UndefinedWrapper<
            Pet
> body;

  static const bodyReflection = XmlReflectionWrapper(
    xml: XmlReflection(
    xmlName: r'Pet',
),
    
            
        
        
            
                Pet.$reflection
        
,
)
;

  /// The raw result of calling jsonDecode
  final Object? rawJson;

  DefaultApiPetsMulticontentTestPostResponse201ApplicationJson({
    this.body = const UndefinedWrapper.undefined(),
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
    this.rawJson,
  });

  static Future<DefaultApiPetsMulticontentTestPostResponse201ApplicationJson> fromResponse(HttpResponseBase response, {required MediaType contentType, required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
  final encodingRules = <String, PropertyEncodingRule>{
      
    };

    final context = wireSerializationOptions.createSerializationContext(contentType);

    switch (contentType) {
      case MediaType(type: 'application', subtype: 'json'):
        final encoding = OASNetworkingUtils.getEncodingOrDefault(contentType);
        final serialized = await encoding.decodeStream(response.bodyBytesStream);
        final v = jsonDecode(serialized);
        if (bodyReflection.canDeserializeFunction(v, context)) {
          final res = bodyReflection.deserializeFunction(v, context);
          return DefaultApiPetsMulticontentTestPostResponse201ApplicationJson(
            headers: response.headers,
            statusCode: response.statusCode,
            reasonPhrase: response.reasonPhrase,
            userContext: userContext,
            body: UndefinedWrapper(res),
            
          );
        } else {
          // since we consumed the stream, we need to publish our read result.
          return DefaultApiPetsMulticontentTestPostResponse201ApplicationJson(
            headers: response.headers,
            statusCode: response.statusCode,
            reasonPhrase: response.reasonPhrase,
            userContext: userContext,
            rawJson: v,
            
          );
        }
      default:
    }
    return DefaultApiPetsMulticontentTestPostResponse201ApplicationJson(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
      
    );
  }
}






/// Represent the response when content-type is application/xml.
class DefaultApiPetsMulticontentTestPostResponse201ApplicationXml extends DefaultApiPetsMulticontentTestPostResponse201 {
  final UndefinedWrapper<
            NewPet
> body;

  static const bodyReflection = XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                NewPet.$reflection
        
,
)
;

  /// The raw result of calling XmlDocumentFragment.parse
  final XmlDocumentFragment? rawXml;

  DefaultApiPetsMulticontentTestPostResponse201ApplicationXml({
    this.body = const UndefinedWrapper.undefined(),
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
    this.rawXml,
  });

  static Future<DefaultApiPetsMulticontentTestPostResponse201ApplicationXml> fromResponse(HttpResponseBase response, {required MediaType contentType, required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
  final encodingRules = <String, PropertyEncodingRule>{
      
    };

    final context = wireSerializationOptions.createSerializationContext(contentType);

    switch (contentType) {
      case MediaType(type: 'application', subtype: 'xml'):
        final encoding = OASNetworkingUtils.getEncodingOrDefault(contentType);
        final serialized = await encoding.decodeStream(response.bodyBytesStream);
        final v = XmlDocumentFragment.parse(serialized);
        // check if v can be deserialized to xml
        return DefaultApiPetsMulticontentTestPostResponse201ApplicationXml(
          headers: response.headers,
          statusCode: response.statusCode,
          reasonPhrase: response.reasonPhrase,
          userContext: userContext,
          rawXml: v,
          
        );
      default:
    }
    return DefaultApiPetsMulticontentTestPostResponse201ApplicationXml(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
      
    );
  }
}






/// Represent the response when content-type is application/x-www-form-urlencoded.
class DefaultApiPetsMulticontentTestPostResponse201ApplicationXWwwFormUrlencoded extends DefaultApiPetsMulticontentTestPostResponse201 {
  final UndefinedWrapper<
            Triangle
> body;

  static const bodyReflection = XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                Triangle.$reflection
        
,
)
;

  final Map<String, dynamic>? rawQueryParameters;

  DefaultApiPetsMulticontentTestPostResponse201ApplicationXWwwFormUrlencoded({
    this.body = const UndefinedWrapper.undefined(),
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
    this.rawQueryParameters,
  });

  static Future<DefaultApiPetsMulticontentTestPostResponse201ApplicationXWwwFormUrlencoded> fromResponse(HttpResponseBase response, {required MediaType contentType, required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
  final encodingRules = <String, PropertyEncodingRule>{
      
      r'name': PropertyEncodingRule(
        style: r'form',
        explode: true,
        
        
      ),
      
    };

    final context = wireSerializationOptions.createSerializationContext(contentType);

    switch (contentType) {
      case MediaType(type: 'application', subtype: 'x-www-form-urlencoded'):
        final encoding = OASNetworkingUtils.getEncodingOrDefault(contentType);
        final serialized = await encoding.decodeStream(response.bodyBytesStream);
        final queryParametersAll = Uri(query: serialized).queryParametersAll;
        final v = queryParametersAll.map((k,v) => MapEntry<String, dynamic>(k, v.isEmpty ? null : v.length == 1 ? v.first.isEmpty ? null : v.first : v));
        if (bodyReflection.canDeserializeFunction(v, context)) {
          final res = bodyReflection.deserializeFunction(v, context);
          return DefaultApiPetsMulticontentTestPostResponse201ApplicationXWwwFormUrlencoded(
            headers: response.headers,
            statusCode: response.statusCode,
            reasonPhrase: response.reasonPhrase,
            userContext: userContext,
            body: UndefinedWrapper(res),
            
          );
        } else {
          // since we consumed the stream, we need to publish our read result.
          return DefaultApiPetsMulticontentTestPostResponse201ApplicationXWwwFormUrlencoded(
            headers: response.headers,
            statusCode: response.statusCode,
            reasonPhrase: response.reasonPhrase,
            userContext: userContext,
            rawQueryParameters: v,
            
          );
        }
      default:
    }
    return DefaultApiPetsMulticontentTestPostResponse201ApplicationXWwwFormUrlencoded(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
      
    );
  }
}















class DefaultApiPetsMulticontentTestPostResponse2XX extends DefaultApiPetsMulticontentTestPostResponse {
  DefaultApiPetsMulticontentTestPostResponse2XX({
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
     this.xRateLimit= const UndefinedWrapper
        .undefined()
,
     this.xRateLimitRemaining= const UndefinedWrapper
        .undefined()
,
     this.xRateLimitReset= const UndefinedWrapper
        .undefined()
,
  });

  final UndefinedWrapper<
            int
> xRateLimit;
  final UndefinedWrapper<
            int
> xRateLimitRemaining;
  final UndefinedWrapper<
            DateTime
> xRateLimitReset;

  T split2XX<T>({
    
    required T Function(DefaultApiPetsMulticontentTestPostResponse2XXApplicationJson response) onApplicationJson,
    
    required T Function(DefaultApiPetsMulticontentTestPostResponse2XX response) other,
  }) {
    return switch (this) {
      
      DefaultApiPetsMulticontentTestPostResponse2XXApplicationJson response => onApplicationJson(response),
      
      _ => other(this),
    };
  }

  static Future<DefaultApiPetsMulticontentTestPostResponse2XX> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    final headers = response.headers;
    final contentTypeRaw = headers['Content-Type'];
    final contentTypeParsed = contentTypeRaw == null ? null : MediaType.parse(contentTypeRaw);
    if (contentTypeParsed != null) {
      final matchedResponse = <(PatternMatchResult, Future<DefaultApiPetsMulticontentTestPostResponse2XX> Function())>[
      (
        OASNetworkingUtils.matchesContentTypePattern(contentTypeParsed, MediaType.parse(r'application/json')),
        () => DefaultApiPetsMulticontentTestPostResponse2XXApplicationJson.fromResponse(response, contentType: contentTypeParsed, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
      ),
      ].pickPrioritized();
      if (matchedResponse != null) {
        return matchedResponse();
      }
    }
    return DefaultApiPetsMulticontentTestPostResponse2XX(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}








/// Represent the response when content-type is application/json.
class DefaultApiPetsMulticontentTestPostResponse2XXApplicationJson extends DefaultApiPetsMulticontentTestPostResponse2XX {
  final UndefinedWrapper<
            Pet
> body;

  static const bodyReflection = XmlReflectionWrapper(
    xml: XmlReflection(
    xmlName: r'Pet',
),
    
            
        
        
            
                Pet.$reflection
        
,
)
;

  /// The raw result of calling jsonDecode
  final Object? rawJson;

  DefaultApiPetsMulticontentTestPostResponse2XXApplicationJson({
    this.body = const UndefinedWrapper.undefined(),
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
     super.xRateLimit,
     super.xRateLimitRemaining,
     super.xRateLimitReset,
    this.rawJson,
  });

  static Future<DefaultApiPetsMulticontentTestPostResponse2XXApplicationJson> fromResponse(HttpResponseBase response, {required MediaType contentType, required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
  final encodingRules = <String, PropertyEncodingRule>{
      
    };

    final context = wireSerializationOptions.createSerializationContext(contentType);

    switch (contentType) {
      case MediaType(type: 'application', subtype: 'json'):
        final encoding = OASNetworkingUtils.getEncodingOrDefault(contentType);
        final serialized = await encoding.decodeStream(response.bodyBytesStream);
        final v = jsonDecode(serialized);
        if (bodyReflection.canDeserializeFunction(v, context)) {
          final res = bodyReflection.deserializeFunction(v, context);
          return DefaultApiPetsMulticontentTestPostResponse2XXApplicationJson(
            headers: response.headers,
            statusCode: response.statusCode,
            reasonPhrase: response.reasonPhrase,
            userContext: userContext,
            body: UndefinedWrapper(res),
            xRateLimit: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forint
        
,
)
).deserializeFunction(response.headers.getOrUndefined(r'X-Rate-Limit')),
xRateLimitRemaining: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forint
        
,
)
).deserializeFunction(response.headers.getOrUndefined(r'X-RateLimit-Remaining')),
xRateLimitReset: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forDateTime
        
,
)
).deserializeFunction(response.headers.getOrUndefined(r'X-RateLimit-Reset')),

          );
        } else {
          // since we consumed the stream, we need to publish our read result.
          return DefaultApiPetsMulticontentTestPostResponse2XXApplicationJson(
            headers: response.headers,
            statusCode: response.statusCode,
            reasonPhrase: response.reasonPhrase,
            userContext: userContext,
            rawJson: v,
            xRateLimit: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forint
        
,
)
).deserializeFunction(response.headers.getOrUndefined(r'X-Rate-Limit')),
xRateLimitRemaining: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forint
        
,
)
).deserializeFunction(response.headers.getOrUndefined(r'X-RateLimit-Remaining')),
xRateLimitReset: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forDateTime
        
,
)
).deserializeFunction(response.headers.getOrUndefined(r'X-RateLimit-Reset')),

          );
        }
      default:
    }
    return DefaultApiPetsMulticontentTestPostResponse2XXApplicationJson(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
      xRateLimit: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forint
        
,
)
).deserializeFunction(response.headers.getOrUndefined(r'X-Rate-Limit')),
xRateLimitRemaining: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forint
        
,
)
).deserializeFunction(response.headers.getOrUndefined(r'X-RateLimit-Remaining')),
xRateLimitReset: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forDateTime
        
,
)
).deserializeFunction(response.headers.getOrUndefined(r'X-RateLimit-Reset')),

    );
  }
}



class DefaultApiPetsMulticontentTestPostResponseDefault extends DefaultApiPetsMulticontentTestPostResponse {
  DefaultApiPetsMulticontentTestPostResponseDefault({
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
  });


  T splitDefault<T>({
    
    required T Function(DefaultApiPetsMulticontentTestPostResponseDefaultApplicationJson response) onApplicationJson,
    
    required T Function(DefaultApiPetsMulticontentTestPostResponseDefaultApplicationXWwwFormUrlencoded response) onApplicationXWwwFormUrlencoded,
    
    required T Function(DefaultApiPetsMulticontentTestPostResponseDefault response) other,
  }) {
    return switch (this) {
      
      DefaultApiPetsMulticontentTestPostResponseDefaultApplicationJson response => onApplicationJson(response),
      
      DefaultApiPetsMulticontentTestPostResponseDefaultApplicationXWwwFormUrlencoded response => onApplicationXWwwFormUrlencoded(response),
      
      _ => other(this),
    };
  }

  static Future<DefaultApiPetsMulticontentTestPostResponseDefault> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    final headers = response.headers;
    final contentTypeRaw = headers['Content-Type'];
    final contentTypeParsed = contentTypeRaw == null ? null : MediaType.parse(contentTypeRaw);
    if (contentTypeParsed != null) {
      final matchedResponse = <(PatternMatchResult, Future<DefaultApiPetsMulticontentTestPostResponseDefault> Function())>[
      (
        OASNetworkingUtils.matchesContentTypePattern(contentTypeParsed, MediaType.parse(r'application/json')),
        () => DefaultApiPetsMulticontentTestPostResponseDefaultApplicationJson.fromResponse(response, contentType: contentTypeParsed, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
      ),
      (
        OASNetworkingUtils.matchesContentTypePattern(contentTypeParsed, MediaType.parse(r'application/x-www-form-urlencoded')),
        () => DefaultApiPetsMulticontentTestPostResponseDefaultApplicationXWwwFormUrlencoded.fromResponse(response, contentType: contentTypeParsed, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
      ),
      ].pickPrioritized();
      if (matchedResponse != null) {
        return matchedResponse();
      }
    }
    return DefaultApiPetsMulticontentTestPostResponseDefault(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}








/// Represent the response when content-type is application/json.
class DefaultApiPetsMulticontentTestPostResponseDefaultApplicationJson extends DefaultApiPetsMulticontentTestPostResponseDefault {
  final UndefinedWrapper<
            Pet
> body;

  static const bodyReflection = XmlReflectionWrapper(
    xml: XmlReflection(
    xmlName: r'Pet',
),
    
            
        
        
            
                Pet.$reflection
        
,
)
;

  /// The raw result of calling jsonDecode
  final Object? rawJson;

  DefaultApiPetsMulticontentTestPostResponseDefaultApplicationJson({
    this.body = const UndefinedWrapper.undefined(),
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
    this.rawJson,
  });

  static Future<DefaultApiPetsMulticontentTestPostResponseDefaultApplicationJson> fromResponse(HttpResponseBase response, {required MediaType contentType, required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
  final encodingRules = <String, PropertyEncodingRule>{
      
    };

    final context = wireSerializationOptions.createSerializationContext(contentType);

    switch (contentType) {
      case MediaType(type: 'application', subtype: 'json'):
        final encoding = OASNetworkingUtils.getEncodingOrDefault(contentType);
        final serialized = await encoding.decodeStream(response.bodyBytesStream);
        final v = jsonDecode(serialized);
        if (bodyReflection.canDeserializeFunction(v, context)) {
          final res = bodyReflection.deserializeFunction(v, context);
          return DefaultApiPetsMulticontentTestPostResponseDefaultApplicationJson(
            headers: response.headers,
            statusCode: response.statusCode,
            reasonPhrase: response.reasonPhrase,
            userContext: userContext,
            body: UndefinedWrapper(res),
            
          );
        } else {
          // since we consumed the stream, we need to publish our read result.
          return DefaultApiPetsMulticontentTestPostResponseDefaultApplicationJson(
            headers: response.headers,
            statusCode: response.statusCode,
            reasonPhrase: response.reasonPhrase,
            userContext: userContext,
            rawJson: v,
            
          );
        }
      default:
    }
    return DefaultApiPetsMulticontentTestPostResponseDefaultApplicationJson(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
      
    );
  }
}






/// Represent the response when content-type is application/x-www-form-urlencoded.
class DefaultApiPetsMulticontentTestPostResponseDefaultApplicationXWwwFormUrlencoded extends DefaultApiPetsMulticontentTestPostResponseDefault {
  final UndefinedWrapper<
            Triangle
> body;

  static const bodyReflection = XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                Triangle.$reflection
        
,
)
;

  final Map<String, dynamic>? rawQueryParameters;

  DefaultApiPetsMulticontentTestPostResponseDefaultApplicationXWwwFormUrlencoded({
    this.body = const UndefinedWrapper.undefined(),
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
    this.rawQueryParameters,
  });

  static Future<DefaultApiPetsMulticontentTestPostResponseDefaultApplicationXWwwFormUrlencoded> fromResponse(HttpResponseBase response, {required MediaType contentType, required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
  final encodingRules = <String, PropertyEncodingRule>{
      
      r'name': PropertyEncodingRule(
        style: r'form',
        explode: true,
        
        
      ),
      
    };

    final context = wireSerializationOptions.createSerializationContext(contentType);

    switch (contentType) {
      case MediaType(type: 'application', subtype: 'x-www-form-urlencoded'):
        final encoding = OASNetworkingUtils.getEncodingOrDefault(contentType);
        final serialized = await encoding.decodeStream(response.bodyBytesStream);
        final queryParametersAll = Uri(query: serialized).queryParametersAll;
        final v = queryParametersAll.map((k,v) => MapEntry<String, dynamic>(k, v.isEmpty ? null : v.length == 1 ? v.first.isEmpty ? null : v.first : v));
        if (bodyReflection.canDeserializeFunction(v, context)) {
          final res = bodyReflection.deserializeFunction(v, context);
          return DefaultApiPetsMulticontentTestPostResponseDefaultApplicationXWwwFormUrlencoded(
            headers: response.headers,
            statusCode: response.statusCode,
            reasonPhrase: response.reasonPhrase,
            userContext: userContext,
            body: UndefinedWrapper(res),
            
          );
        } else {
          // since we consumed the stream, we need to publish our read result.
          return DefaultApiPetsMulticontentTestPostResponseDefaultApplicationXWwwFormUrlencoded(
            headers: response.headers,
            statusCode: response.statusCode,
            reasonPhrase: response.reasonPhrase,
            userContext: userContext,
            rawQueryParameters: v,
            
          );
        }
      default:
    }
    return DefaultApiPetsMulticontentTestPostResponseDefaultApplicationXWwwFormUrlencoded(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
      
    );
  }
}





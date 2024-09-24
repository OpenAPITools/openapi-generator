// ignore_for_file: unnecessary_type_check, unnecessary_null_comparison, unnecessary_cast

part of 'fake_api.dart';


class FakeApiFakeGetFreeFormObjectGetResponse {
  FakeApiFakeGetFreeFormObjectGetResponse({
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
    required T Function(FakeApiFakeGetFreeFormObjectGetResponse200 response) on200,
    required T Function(FakeApiFakeGetFreeFormObjectGetResponse response) other,
  }) {
    return switch (this) {
      FakeApiFakeGetFreeFormObjectGetResponse200 response => on200(response),
      _ => other(this),
    };
  }

  static Future<FakeApiFakeGetFreeFormObjectGetResponse> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    final matchedResponse = <(PatternMatchResult, Future<FakeApiFakeGetFreeFormObjectGetResponse> Function())> [
    (
      OASNetworkingUtils.matchesStatusCodePattern(response.statusCode, r'200'),
      () => FakeApiFakeGetFreeFormObjectGetResponse200.fromResponse(response, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
    ),
    ].pickPrioritized();
    if (matchedResponse != null) {
      return matchedResponse();
    }
    return FakeApiFakeGetFreeFormObjectGetResponse(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}



class FakeApiFakeGetFreeFormObjectGetResponse200 extends FakeApiFakeGetFreeFormObjectGetResponse {
  FakeApiFakeGetFreeFormObjectGetResponse200({
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
  });


  T split200<T>({
    
    required T Function(FakeApiFakeGetFreeFormObjectGetResponse200ApplicationJson response) onApplicationJson,
    
    required T Function(FakeApiFakeGetFreeFormObjectGetResponse200 response) other,
  }) {
    return switch (this) {
      
      FakeApiFakeGetFreeFormObjectGetResponse200ApplicationJson response => onApplicationJson(response),
      
      _ => other(this),
    };
  }

  static Future<FakeApiFakeGetFreeFormObjectGetResponse200> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    final headers = response.headers;
    final contentTypeRaw = headers['Content-Type'];
    final contentTypeParsed = contentTypeRaw == null ? null : MediaType.parse(contentTypeRaw);
    if (contentTypeParsed != null) {
      final matchedResponse = <(PatternMatchResult, Future<FakeApiFakeGetFreeFormObjectGetResponse200> Function())>[
      (
        OASNetworkingUtils.matchesContentTypePattern(contentTypeParsed, MediaType.parse(r'application/json')),
        () => FakeApiFakeGetFreeFormObjectGetResponse200ApplicationJson.fromResponse(response, contentType: contentTypeParsed, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
      ),
      ].pickPrioritized();
      if (matchedResponse != null) {
        return matchedResponse();
      }
    }
    return FakeApiFakeGetFreeFormObjectGetResponse200(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}








/// Represent the response when content-type is application/json.
class FakeApiFakeGetFreeFormObjectGetResponse200ApplicationJson extends FakeApiFakeGetFreeFormObjectGetResponse200 {
  final UndefinedWrapper<
            FreeFormObjectTestClass
> body;

  static const bodyReflection = XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                FreeFormObjectTestClass.$reflection
        
,
)
;

  /// The raw result of calling jsonDecode
  final Object? rawJson;

  FakeApiFakeGetFreeFormObjectGetResponse200ApplicationJson({
    this.body = const UndefinedWrapper.undefined(),
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
    this.rawJson,
  });

  static Future<FakeApiFakeGetFreeFormObjectGetResponse200ApplicationJson> fromResponse(HttpResponseBase response, {required MediaType contentType, required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
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
          return FakeApiFakeGetFreeFormObjectGetResponse200ApplicationJson(
            headers: response.headers,
            statusCode: response.statusCode,
            reasonPhrase: response.reasonPhrase,
            userContext: userContext,
            body: UndefinedWrapper(res),
            
          );
        } else {
          // since we consumed the stream, we need to publish our read result.
          return FakeApiFakeGetFreeFormObjectGetResponse200ApplicationJson(
            headers: response.headers,
            statusCode: response.statusCode,
            reasonPhrase: response.reasonPhrase,
            userContext: userContext,
            rawJson: v,
            
          );
        }
      default:
    }
    return FakeApiFakeGetFreeFormObjectGetResponse200ApplicationJson(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
      
    );
  }
}


class FakeApiFakeOuterBooleanSerializeResponse {
  FakeApiFakeOuterBooleanSerializeResponse({
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
    required T Function(FakeApiFakeOuterBooleanSerializeResponse200 response) on200,
    required T Function(FakeApiFakeOuterBooleanSerializeResponse response) other,
  }) {
    return switch (this) {
      FakeApiFakeOuterBooleanSerializeResponse200 response => on200(response),
      _ => other(this),
    };
  }

  static Future<FakeApiFakeOuterBooleanSerializeResponse> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    final matchedResponse = <(PatternMatchResult, Future<FakeApiFakeOuterBooleanSerializeResponse> Function())> [
    (
      OASNetworkingUtils.matchesStatusCodePattern(response.statusCode, r'200'),
      () => FakeApiFakeOuterBooleanSerializeResponse200.fromResponse(response, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
    ),
    ].pickPrioritized();
    if (matchedResponse != null) {
      return matchedResponse();
    }
    return FakeApiFakeOuterBooleanSerializeResponse(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}



class FakeApiFakeOuterBooleanSerializeResponse200 extends FakeApiFakeOuterBooleanSerializeResponse {
  FakeApiFakeOuterBooleanSerializeResponse200({
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
  });


  T split200<T>({
    
    required T Function(FakeApiFakeOuterBooleanSerializeResponse200AnyAny response) onAnyAny,
    
    required T Function(FakeApiFakeOuterBooleanSerializeResponse200 response) other,
  }) {
    return switch (this) {
      
      FakeApiFakeOuterBooleanSerializeResponse200AnyAny response => onAnyAny(response),
      
      _ => other(this),
    };
  }

  static Future<FakeApiFakeOuterBooleanSerializeResponse200> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    final headers = response.headers;
    final contentTypeRaw = headers['Content-Type'];
    final contentTypeParsed = contentTypeRaw == null ? null : MediaType.parse(contentTypeRaw);
    if (contentTypeParsed != null) {
      final matchedResponse = <(PatternMatchResult, Future<FakeApiFakeOuterBooleanSerializeResponse200> Function())>[
      (
        OASNetworkingUtils.matchesContentTypePattern(contentTypeParsed, MediaType.parse(r'*/*')),
        () => FakeApiFakeOuterBooleanSerializeResponse200AnyAny.fromResponse(response, contentType: contentTypeParsed, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
      ),
      ].pickPrioritized();
      if (matchedResponse != null) {
        return matchedResponse();
      }
    }
    return FakeApiFakeOuterBooleanSerializeResponse200(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}








/// Represent the response when content-type is */*.
class FakeApiFakeOuterBooleanSerializeResponse200AnyAny extends FakeApiFakeOuterBooleanSerializeResponse200 {
  final UndefinedWrapper<
            bool
> body;

  static const bodyReflection = XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forbool
        
,
)
;


  FakeApiFakeOuterBooleanSerializeResponse200AnyAny({
    this.body = const UndefinedWrapper.undefined(),
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
  });

  static Future<FakeApiFakeOuterBooleanSerializeResponse200AnyAny> fromResponse(HttpResponseBase response, {required MediaType contentType, required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
  final encodingRules = <String, PropertyEncodingRule>{
      
    };

    final context = wireSerializationOptions.createSerializationContext(contentType);

    switch (contentType) {
      default:
    }
    return FakeApiFakeOuterBooleanSerializeResponse200AnyAny(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
      
    );
  }
}


class FakeApiFakeOuterCompositeSerializeResponse {
  FakeApiFakeOuterCompositeSerializeResponse({
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
    required T Function(FakeApiFakeOuterCompositeSerializeResponse200 response) on200,
    required T Function(FakeApiFakeOuterCompositeSerializeResponse response) other,
  }) {
    return switch (this) {
      FakeApiFakeOuterCompositeSerializeResponse200 response => on200(response),
      _ => other(this),
    };
  }

  static Future<FakeApiFakeOuterCompositeSerializeResponse> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    final matchedResponse = <(PatternMatchResult, Future<FakeApiFakeOuterCompositeSerializeResponse> Function())> [
    (
      OASNetworkingUtils.matchesStatusCodePattern(response.statusCode, r'200'),
      () => FakeApiFakeOuterCompositeSerializeResponse200.fromResponse(response, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
    ),
    ].pickPrioritized();
    if (matchedResponse != null) {
      return matchedResponse();
    }
    return FakeApiFakeOuterCompositeSerializeResponse(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}



class FakeApiFakeOuterCompositeSerializeResponse200 extends FakeApiFakeOuterCompositeSerializeResponse {
  FakeApiFakeOuterCompositeSerializeResponse200({
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
  });


  T split200<T>({
    
    required T Function(FakeApiFakeOuterCompositeSerializeResponse200AnyAny response) onAnyAny,
    
    required T Function(FakeApiFakeOuterCompositeSerializeResponse200 response) other,
  }) {
    return switch (this) {
      
      FakeApiFakeOuterCompositeSerializeResponse200AnyAny response => onAnyAny(response),
      
      _ => other(this),
    };
  }

  static Future<FakeApiFakeOuterCompositeSerializeResponse200> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    final headers = response.headers;
    final contentTypeRaw = headers['Content-Type'];
    final contentTypeParsed = contentTypeRaw == null ? null : MediaType.parse(contentTypeRaw);
    if (contentTypeParsed != null) {
      final matchedResponse = <(PatternMatchResult, Future<FakeApiFakeOuterCompositeSerializeResponse200> Function())>[
      (
        OASNetworkingUtils.matchesContentTypePattern(contentTypeParsed, MediaType.parse(r'*/*')),
        () => FakeApiFakeOuterCompositeSerializeResponse200AnyAny.fromResponse(response, contentType: contentTypeParsed, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
      ),
      ].pickPrioritized();
      if (matchedResponse != null) {
        return matchedResponse();
      }
    }
    return FakeApiFakeOuterCompositeSerializeResponse200(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}








/// Represent the response when content-type is */*.
class FakeApiFakeOuterCompositeSerializeResponse200AnyAny extends FakeApiFakeOuterCompositeSerializeResponse200 {
  final UndefinedWrapper<
            OuterComposite
> body;

  static const bodyReflection = XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                OuterComposite.$reflection
        
,
)
;


  FakeApiFakeOuterCompositeSerializeResponse200AnyAny({
    this.body = const UndefinedWrapper.undefined(),
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
  });

  static Future<FakeApiFakeOuterCompositeSerializeResponse200AnyAny> fromResponse(HttpResponseBase response, {required MediaType contentType, required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
  final encodingRules = <String, PropertyEncodingRule>{
      
    };

    final context = wireSerializationOptions.createSerializationContext(contentType);

    switch (contentType) {
      default:
    }
    return FakeApiFakeOuterCompositeSerializeResponse200AnyAny(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
      
    );
  }
}


class FakeApiFakeOuterNumberSerializeResponse {
  FakeApiFakeOuterNumberSerializeResponse({
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
    required T Function(FakeApiFakeOuterNumberSerializeResponse200 response) on200,
    required T Function(FakeApiFakeOuterNumberSerializeResponse response) other,
  }) {
    return switch (this) {
      FakeApiFakeOuterNumberSerializeResponse200 response => on200(response),
      _ => other(this),
    };
  }

  static Future<FakeApiFakeOuterNumberSerializeResponse> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    final matchedResponse = <(PatternMatchResult, Future<FakeApiFakeOuterNumberSerializeResponse> Function())> [
    (
      OASNetworkingUtils.matchesStatusCodePattern(response.statusCode, r'200'),
      () => FakeApiFakeOuterNumberSerializeResponse200.fromResponse(response, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
    ),
    ].pickPrioritized();
    if (matchedResponse != null) {
      return matchedResponse();
    }
    return FakeApiFakeOuterNumberSerializeResponse(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}



class FakeApiFakeOuterNumberSerializeResponse200 extends FakeApiFakeOuterNumberSerializeResponse {
  FakeApiFakeOuterNumberSerializeResponse200({
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
  });


  T split200<T>({
    
    required T Function(FakeApiFakeOuterNumberSerializeResponse200AnyAny response) onAnyAny,
    
    required T Function(FakeApiFakeOuterNumberSerializeResponse200 response) other,
  }) {
    return switch (this) {
      
      FakeApiFakeOuterNumberSerializeResponse200AnyAny response => onAnyAny(response),
      
      _ => other(this),
    };
  }

  static Future<FakeApiFakeOuterNumberSerializeResponse200> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    final headers = response.headers;
    final contentTypeRaw = headers['Content-Type'];
    final contentTypeParsed = contentTypeRaw == null ? null : MediaType.parse(contentTypeRaw);
    if (contentTypeParsed != null) {
      final matchedResponse = <(PatternMatchResult, Future<FakeApiFakeOuterNumberSerializeResponse200> Function())>[
      (
        OASNetworkingUtils.matchesContentTypePattern(contentTypeParsed, MediaType.parse(r'*/*')),
        () => FakeApiFakeOuterNumberSerializeResponse200AnyAny.fromResponse(response, contentType: contentTypeParsed, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
      ),
      ].pickPrioritized();
      if (matchedResponse != null) {
        return matchedResponse();
      }
    }
    return FakeApiFakeOuterNumberSerializeResponse200(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}








/// Represent the response when content-type is */*.
class FakeApiFakeOuterNumberSerializeResponse200AnyAny extends FakeApiFakeOuterNumberSerializeResponse200 {
  final UndefinedWrapper<
            num
> body;

  static const bodyReflection = XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.fornum
        
,
)
;


  FakeApiFakeOuterNumberSerializeResponse200AnyAny({
    this.body = const UndefinedWrapper.undefined(),
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
  });

  static Future<FakeApiFakeOuterNumberSerializeResponse200AnyAny> fromResponse(HttpResponseBase response, {required MediaType contentType, required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
  final encodingRules = <String, PropertyEncodingRule>{
      
    };

    final context = wireSerializationOptions.createSerializationContext(contentType);

    switch (contentType) {
      default:
    }
    return FakeApiFakeOuterNumberSerializeResponse200AnyAny(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
      
    );
  }
}


class FakeApiFakeOuterStringSerializeResponse {
  FakeApiFakeOuterStringSerializeResponse({
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
    required T Function(FakeApiFakeOuterStringSerializeResponse200 response) on200,
    required T Function(FakeApiFakeOuterStringSerializeResponse response) other,
  }) {
    return switch (this) {
      FakeApiFakeOuterStringSerializeResponse200 response => on200(response),
      _ => other(this),
    };
  }

  static Future<FakeApiFakeOuterStringSerializeResponse> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    final matchedResponse = <(PatternMatchResult, Future<FakeApiFakeOuterStringSerializeResponse> Function())> [
    (
      OASNetworkingUtils.matchesStatusCodePattern(response.statusCode, r'200'),
      () => FakeApiFakeOuterStringSerializeResponse200.fromResponse(response, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
    ),
    ].pickPrioritized();
    if (matchedResponse != null) {
      return matchedResponse();
    }
    return FakeApiFakeOuterStringSerializeResponse(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}



class FakeApiFakeOuterStringSerializeResponse200 extends FakeApiFakeOuterStringSerializeResponse {
  FakeApiFakeOuterStringSerializeResponse200({
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
  });


  T split200<T>({
    
    required T Function(FakeApiFakeOuterStringSerializeResponse200AnyAny response) onAnyAny,
    
    required T Function(FakeApiFakeOuterStringSerializeResponse200 response) other,
  }) {
    return switch (this) {
      
      FakeApiFakeOuterStringSerializeResponse200AnyAny response => onAnyAny(response),
      
      _ => other(this),
    };
  }

  static Future<FakeApiFakeOuterStringSerializeResponse200> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    final headers = response.headers;
    final contentTypeRaw = headers['Content-Type'];
    final contentTypeParsed = contentTypeRaw == null ? null : MediaType.parse(contentTypeRaw);
    if (contentTypeParsed != null) {
      final matchedResponse = <(PatternMatchResult, Future<FakeApiFakeOuterStringSerializeResponse200> Function())>[
      (
        OASNetworkingUtils.matchesContentTypePattern(contentTypeParsed, MediaType.parse(r'*/*')),
        () => FakeApiFakeOuterStringSerializeResponse200AnyAny.fromResponse(response, contentType: contentTypeParsed, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
      ),
      ].pickPrioritized();
      if (matchedResponse != null) {
        return matchedResponse();
      }
    }
    return FakeApiFakeOuterStringSerializeResponse200(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}








/// Represent the response when content-type is */*.
class FakeApiFakeOuterStringSerializeResponse200AnyAny extends FakeApiFakeOuterStringSerializeResponse200 {
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


  FakeApiFakeOuterStringSerializeResponse200AnyAny({
    this.body = const UndefinedWrapper.undefined(),
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
  });

  static Future<FakeApiFakeOuterStringSerializeResponse200AnyAny> fromResponse(HttpResponseBase response, {required MediaType contentType, required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
  final encodingRules = <String, PropertyEncodingRule>{
      
    };

    final context = wireSerializationOptions.createSerializationContext(contentType);

    switch (contentType) {
      default:
    }
    return FakeApiFakeOuterStringSerializeResponse200AnyAny(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
      
    );
  }
}


class FakeApiFakeUploadRefRequestBodiesResponse {
  FakeApiFakeUploadRefRequestBodiesResponse({
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
    required T Function(FakeApiFakeUploadRefRequestBodiesResponse200 response) on200,
    required T Function(FakeApiFakeUploadRefRequestBodiesResponse response) other,
  }) {
    return switch (this) {
      FakeApiFakeUploadRefRequestBodiesResponse200 response => on200(response),
      _ => other(this),
    };
  }

  static Future<FakeApiFakeUploadRefRequestBodiesResponse> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    final matchedResponse = <(PatternMatchResult, Future<FakeApiFakeUploadRefRequestBodiesResponse> Function())> [
    (
      OASNetworkingUtils.matchesStatusCodePattern(response.statusCode, r'200'),
      () => FakeApiFakeUploadRefRequestBodiesResponse200.fromResponse(response, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
    ),
    ].pickPrioritized();
    if (matchedResponse != null) {
      return matchedResponse();
    }
    return FakeApiFakeUploadRefRequestBodiesResponse(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}



class FakeApiFakeUploadRefRequestBodiesResponse200 extends FakeApiFakeUploadRefRequestBodiesResponse {
  FakeApiFakeUploadRefRequestBodiesResponse200({
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
  });


  T split200<T>({
    
    required T Function(FakeApiFakeUploadRefRequestBodiesResponse200ApplicationJson response) onApplicationJson,
    
    required T Function(FakeApiFakeUploadRefRequestBodiesResponse200 response) other,
  }) {
    return switch (this) {
      
      FakeApiFakeUploadRefRequestBodiesResponse200ApplicationJson response => onApplicationJson(response),
      
      _ => other(this),
    };
  }

  static Future<FakeApiFakeUploadRefRequestBodiesResponse200> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    final headers = response.headers;
    final contentTypeRaw = headers['Content-Type'];
    final contentTypeParsed = contentTypeRaw == null ? null : MediaType.parse(contentTypeRaw);
    if (contentTypeParsed != null) {
      final matchedResponse = <(PatternMatchResult, Future<FakeApiFakeUploadRefRequestBodiesResponse200> Function())>[
      (
        OASNetworkingUtils.matchesContentTypePattern(contentTypeParsed, MediaType.parse(r'application/json')),
        () => FakeApiFakeUploadRefRequestBodiesResponse200ApplicationJson.fromResponse(response, contentType: contentTypeParsed, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
      ),
      ].pickPrioritized();
      if (matchedResponse != null) {
        return matchedResponse();
      }
    }
    return FakeApiFakeUploadRefRequestBodiesResponse200(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}








/// Represent the response when content-type is application/json.
class FakeApiFakeUploadRefRequestBodiesResponse200ApplicationJson extends FakeApiFakeUploadRefRequestBodiesResponse200 {
  final UndefinedWrapper<
            ApiResponse
> body;

  static const bodyReflection = XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                ApiResponse.$reflection
        
,
)
;

  /// The raw result of calling jsonDecode
  final Object? rawJson;

  FakeApiFakeUploadRefRequestBodiesResponse200ApplicationJson({
    this.body = const UndefinedWrapper.undefined(),
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
    this.rawJson,
  });

  static Future<FakeApiFakeUploadRefRequestBodiesResponse200ApplicationJson> fromResponse(HttpResponseBase response, {required MediaType contentType, required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
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
          return FakeApiFakeUploadRefRequestBodiesResponse200ApplicationJson(
            headers: response.headers,
            statusCode: response.statusCode,
            reasonPhrase: response.reasonPhrase,
            userContext: userContext,
            body: UndefinedWrapper(res),
            
          );
        } else {
          // since we consumed the stream, we need to publish our read result.
          return FakeApiFakeUploadRefRequestBodiesResponse200ApplicationJson(
            headers: response.headers,
            statusCode: response.statusCode,
            reasonPhrase: response.reasonPhrase,
            userContext: userContext,
            rawJson: v,
            
          );
        }
      default:
    }
    return FakeApiFakeUploadRefRequestBodiesResponse200ApplicationJson(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
      
    );
  }
}


class FakeApiGetFakeArrayofenumsResponse {
  FakeApiGetFakeArrayofenumsResponse({
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
    required T Function(FakeApiGetFakeArrayofenumsResponse200 response) on200,
    required T Function(FakeApiGetFakeArrayofenumsResponse response) other,
  }) {
    return switch (this) {
      FakeApiGetFakeArrayofenumsResponse200 response => on200(response),
      _ => other(this),
    };
  }

  static Future<FakeApiGetFakeArrayofenumsResponse> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    final matchedResponse = <(PatternMatchResult, Future<FakeApiGetFakeArrayofenumsResponse> Function())> [
    (
      OASNetworkingUtils.matchesStatusCodePattern(response.statusCode, r'200'),
      () => FakeApiGetFakeArrayofenumsResponse200.fromResponse(response, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
    ),
    ].pickPrioritized();
    if (matchedResponse != null) {
      return matchedResponse();
    }
    return FakeApiGetFakeArrayofenumsResponse(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}



class FakeApiGetFakeArrayofenumsResponse200 extends FakeApiGetFakeArrayofenumsResponse {
  FakeApiGetFakeArrayofenumsResponse200({
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
  });


  T split200<T>({
    
    required T Function(FakeApiGetFakeArrayofenumsResponse200ApplicationJson response) onApplicationJson,
    
    required T Function(FakeApiGetFakeArrayofenumsResponse200 response) other,
  }) {
    return switch (this) {
      
      FakeApiGetFakeArrayofenumsResponse200ApplicationJson response => onApplicationJson(response),
      
      _ => other(this),
    };
  }

  static Future<FakeApiGetFakeArrayofenumsResponse200> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    final headers = response.headers;
    final contentTypeRaw = headers['Content-Type'];
    final contentTypeParsed = contentTypeRaw == null ? null : MediaType.parse(contentTypeRaw);
    if (contentTypeParsed != null) {
      final matchedResponse = <(PatternMatchResult, Future<FakeApiGetFakeArrayofenumsResponse200> Function())>[
      (
        OASNetworkingUtils.matchesContentTypePattern(contentTypeParsed, MediaType.parse(r'application/json')),
        () => FakeApiGetFakeArrayofenumsResponse200ApplicationJson.fromResponse(response, contentType: contentTypeParsed, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
      ),
      ].pickPrioritized();
      if (matchedResponse != null) {
        return matchedResponse();
      }
    }
    return FakeApiGetFakeArrayofenumsResponse200(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}










/// Represent the response when content-type is application/json.
class FakeApiGetFakeArrayofenumsResponse200ApplicationJson extends FakeApiGetFakeArrayofenumsResponse200 {
  final UndefinedWrapper<
    List<
        
            OuterEnum
?>
> body;

  static const bodyReflection = XmlReflectionWrapper(
    xml: XmlReflection(
),
    
    ListReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    NullableReflection(
            
        
        
            OuterEnum.$reflection
        
),
)
)
,
)
;

  /// The raw result of calling jsonDecode
  final Object? rawJson;

  FakeApiGetFakeArrayofenumsResponse200ApplicationJson({
    this.body = const UndefinedWrapper.undefined(),
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
    this.rawJson,
  });

  static Future<FakeApiGetFakeArrayofenumsResponse200ApplicationJson> fromResponse(HttpResponseBase response, {required MediaType contentType, required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
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
          return FakeApiGetFakeArrayofenumsResponse200ApplicationJson(
            headers: response.headers,
            statusCode: response.statusCode,
            reasonPhrase: response.reasonPhrase,
            userContext: userContext,
            body: UndefinedWrapper(res),
            
          );
        } else {
          // since we consumed the stream, we need to publish our read result.
          return FakeApiGetFakeArrayofenumsResponse200ApplicationJson(
            headers: response.headers,
            statusCode: response.statusCode,
            reasonPhrase: response.reasonPhrase,
            userContext: userContext,
            rawJson: v,
            
          );
        }
      default:
    }
    return FakeApiGetFakeArrayofenumsResponse200ApplicationJson(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
      
    );
  }
}


class FakeApiGetFakeHealthResponse {
  FakeApiGetFakeHealthResponse({
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
    required T Function(FakeApiGetFakeHealthResponse200 response) on200,
    required T Function(FakeApiGetFakeHealthResponse response) other,
  }) {
    return switch (this) {
      FakeApiGetFakeHealthResponse200 response => on200(response),
      _ => other(this),
    };
  }

  static Future<FakeApiGetFakeHealthResponse> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    final matchedResponse = <(PatternMatchResult, Future<FakeApiGetFakeHealthResponse> Function())> [
    (
      OASNetworkingUtils.matchesStatusCodePattern(response.statusCode, r'200'),
      () => FakeApiGetFakeHealthResponse200.fromResponse(response, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
    ),
    ].pickPrioritized();
    if (matchedResponse != null) {
      return matchedResponse();
    }
    return FakeApiGetFakeHealthResponse(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}



class FakeApiGetFakeHealthResponse200 extends FakeApiGetFakeHealthResponse {
  FakeApiGetFakeHealthResponse200({
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
  });


  T split200<T>({
    
    required T Function(FakeApiGetFakeHealthResponse200ApplicationJson response) onApplicationJson,
    
    required T Function(FakeApiGetFakeHealthResponse200 response) other,
  }) {
    return switch (this) {
      
      FakeApiGetFakeHealthResponse200ApplicationJson response => onApplicationJson(response),
      
      _ => other(this),
    };
  }

  static Future<FakeApiGetFakeHealthResponse200> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    final headers = response.headers;
    final contentTypeRaw = headers['Content-Type'];
    final contentTypeParsed = contentTypeRaw == null ? null : MediaType.parse(contentTypeRaw);
    if (contentTypeParsed != null) {
      final matchedResponse = <(PatternMatchResult, Future<FakeApiGetFakeHealthResponse200> Function())>[
      (
        OASNetworkingUtils.matchesContentTypePattern(contentTypeParsed, MediaType.parse(r'application/json')),
        () => FakeApiGetFakeHealthResponse200ApplicationJson.fromResponse(response, contentType: contentTypeParsed, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
      ),
      ].pickPrioritized();
      if (matchedResponse != null) {
        return matchedResponse();
      }
    }
    return FakeApiGetFakeHealthResponse200(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}








/// Represent the response when content-type is application/json.
class FakeApiGetFakeHealthResponse200ApplicationJson extends FakeApiGetFakeHealthResponse200 {
  final UndefinedWrapper<
            HealthCheckResult
> body;

  static const bodyReflection = XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                HealthCheckResult.$reflection
        
,
)
;

  /// The raw result of calling jsonDecode
  final Object? rawJson;

  FakeApiGetFakeHealthResponse200ApplicationJson({
    this.body = const UndefinedWrapper.undefined(),
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
    this.rawJson,
  });

  static Future<FakeApiGetFakeHealthResponse200ApplicationJson> fromResponse(HttpResponseBase response, {required MediaType contentType, required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
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
          return FakeApiGetFakeHealthResponse200ApplicationJson(
            headers: response.headers,
            statusCode: response.statusCode,
            reasonPhrase: response.reasonPhrase,
            userContext: userContext,
            body: UndefinedWrapper(res),
            
          );
        } else {
          // since we consumed the stream, we need to publish our read result.
          return FakeApiGetFakeHealthResponse200ApplicationJson(
            headers: response.headers,
            statusCode: response.statusCode,
            reasonPhrase: response.reasonPhrase,
            userContext: userContext,
            rawJson: v,
            
          );
        }
      default:
    }
    return FakeApiGetFakeHealthResponse200ApplicationJson(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
      
    );
  }
}


class FakeApiGetParameterNameMappingResponse {
  FakeApiGetParameterNameMappingResponse({
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
    required T Function(FakeApiGetParameterNameMappingResponse200 response) on200,
    required T Function(FakeApiGetParameterNameMappingResponse response) other,
  }) {
    return switch (this) {
      FakeApiGetParameterNameMappingResponse200 response => on200(response),
      _ => other(this),
    };
  }

  static Future<FakeApiGetParameterNameMappingResponse> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    final matchedResponse = <(PatternMatchResult, Future<FakeApiGetParameterNameMappingResponse> Function())> [
    (
      OASNetworkingUtils.matchesStatusCodePattern(response.statusCode, r'200'),
      () => FakeApiGetParameterNameMappingResponse200.fromResponse(response, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
    ),
    ].pickPrioritized();
    if (matchedResponse != null) {
      return matchedResponse();
    }
    return FakeApiGetParameterNameMappingResponse(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}



class FakeApiGetParameterNameMappingResponse200 extends FakeApiGetParameterNameMappingResponse {
  FakeApiGetParameterNameMappingResponse200({
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
  });




  static Future<FakeApiGetParameterNameMappingResponse200> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    return FakeApiGetParameterNameMappingResponse200(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}




class FakeApiTestAdditionalPropertiesReferenceResponse {
  FakeApiTestAdditionalPropertiesReferenceResponse({
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
    required T Function(FakeApiTestAdditionalPropertiesReferenceResponse200 response) on200,
    required T Function(FakeApiTestAdditionalPropertiesReferenceResponse response) other,
  }) {
    return switch (this) {
      FakeApiTestAdditionalPropertiesReferenceResponse200 response => on200(response),
      _ => other(this),
    };
  }

  static Future<FakeApiTestAdditionalPropertiesReferenceResponse> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    final matchedResponse = <(PatternMatchResult, Future<FakeApiTestAdditionalPropertiesReferenceResponse> Function())> [
    (
      OASNetworkingUtils.matchesStatusCodePattern(response.statusCode, r'200'),
      () => FakeApiTestAdditionalPropertiesReferenceResponse200.fromResponse(response, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
    ),
    ].pickPrioritized();
    if (matchedResponse != null) {
      return matchedResponse();
    }
    return FakeApiTestAdditionalPropertiesReferenceResponse(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}



class FakeApiTestAdditionalPropertiesReferenceResponse200 extends FakeApiTestAdditionalPropertiesReferenceResponse {
  FakeApiTestAdditionalPropertiesReferenceResponse200({
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
  });




  static Future<FakeApiTestAdditionalPropertiesReferenceResponse200> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    return FakeApiTestAdditionalPropertiesReferenceResponse200(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}




class FakeApiTestBodyWithFileSchemaResponse {
  FakeApiTestBodyWithFileSchemaResponse({
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
    required T Function(FakeApiTestBodyWithFileSchemaResponse200 response) on200,
    required T Function(FakeApiTestBodyWithFileSchemaResponse response) other,
  }) {
    return switch (this) {
      FakeApiTestBodyWithFileSchemaResponse200 response => on200(response),
      _ => other(this),
    };
  }

  static Future<FakeApiTestBodyWithFileSchemaResponse> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    final matchedResponse = <(PatternMatchResult, Future<FakeApiTestBodyWithFileSchemaResponse> Function())> [
    (
      OASNetworkingUtils.matchesStatusCodePattern(response.statusCode, r'200'),
      () => FakeApiTestBodyWithFileSchemaResponse200.fromResponse(response, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
    ),
    ].pickPrioritized();
    if (matchedResponse != null) {
      return matchedResponse();
    }
    return FakeApiTestBodyWithFileSchemaResponse(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}



class FakeApiTestBodyWithFileSchemaResponse200 extends FakeApiTestBodyWithFileSchemaResponse {
  FakeApiTestBodyWithFileSchemaResponse200({
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
  });




  static Future<FakeApiTestBodyWithFileSchemaResponse200> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    return FakeApiTestBodyWithFileSchemaResponse200(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}




class FakeApiTestBodyWithQueryParamsResponse {
  FakeApiTestBodyWithQueryParamsResponse({
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
    required T Function(FakeApiTestBodyWithQueryParamsResponse200 response) on200,
    required T Function(FakeApiTestBodyWithQueryParamsResponse response) other,
  }) {
    return switch (this) {
      FakeApiTestBodyWithQueryParamsResponse200 response => on200(response),
      _ => other(this),
    };
  }

  static Future<FakeApiTestBodyWithQueryParamsResponse> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    final matchedResponse = <(PatternMatchResult, Future<FakeApiTestBodyWithQueryParamsResponse> Function())> [
    (
      OASNetworkingUtils.matchesStatusCodePattern(response.statusCode, r'200'),
      () => FakeApiTestBodyWithQueryParamsResponse200.fromResponse(response, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
    ),
    ].pickPrioritized();
    if (matchedResponse != null) {
      return matchedResponse();
    }
    return FakeApiTestBodyWithQueryParamsResponse(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}



class FakeApiTestBodyWithQueryParamsResponse200 extends FakeApiTestBodyWithQueryParamsResponse {
  FakeApiTestBodyWithQueryParamsResponse200({
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
  });




  static Future<FakeApiTestBodyWithQueryParamsResponse200> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    return FakeApiTestBodyWithQueryParamsResponse200(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}




class FakeApiTestClientModelResponse {
  FakeApiTestClientModelResponse({
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
    required T Function(FakeApiTestClientModelResponse200 response) on200,
    required T Function(FakeApiTestClientModelResponse response) other,
  }) {
    return switch (this) {
      FakeApiTestClientModelResponse200 response => on200(response),
      _ => other(this),
    };
  }

  static Future<FakeApiTestClientModelResponse> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    final matchedResponse = <(PatternMatchResult, Future<FakeApiTestClientModelResponse> Function())> [
    (
      OASNetworkingUtils.matchesStatusCodePattern(response.statusCode, r'200'),
      () => FakeApiTestClientModelResponse200.fromResponse(response, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
    ),
    ].pickPrioritized();
    if (matchedResponse != null) {
      return matchedResponse();
    }
    return FakeApiTestClientModelResponse(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}



class FakeApiTestClientModelResponse200 extends FakeApiTestClientModelResponse {
  FakeApiTestClientModelResponse200({
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
  });


  T split200<T>({
    
    required T Function(FakeApiTestClientModelResponse200ApplicationJson response) onApplicationJson,
    
    required T Function(FakeApiTestClientModelResponse200 response) other,
  }) {
    return switch (this) {
      
      FakeApiTestClientModelResponse200ApplicationJson response => onApplicationJson(response),
      
      _ => other(this),
    };
  }

  static Future<FakeApiTestClientModelResponse200> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    final headers = response.headers;
    final contentTypeRaw = headers['Content-Type'];
    final contentTypeParsed = contentTypeRaw == null ? null : MediaType.parse(contentTypeRaw);
    if (contentTypeParsed != null) {
      final matchedResponse = <(PatternMatchResult, Future<FakeApiTestClientModelResponse200> Function())>[
      (
        OASNetworkingUtils.matchesContentTypePattern(contentTypeParsed, MediaType.parse(r'application/json')),
        () => FakeApiTestClientModelResponse200ApplicationJson.fromResponse(response, contentType: contentTypeParsed, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
      ),
      ].pickPrioritized();
      if (matchedResponse != null) {
        return matchedResponse();
      }
    }
    return FakeApiTestClientModelResponse200(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}








/// Represent the response when content-type is application/json.
class FakeApiTestClientModelResponse200ApplicationJson extends FakeApiTestClientModelResponse200 {
  final UndefinedWrapper<
            Client
> body;

  static const bodyReflection = XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                Client.$reflection
        
,
)
;

  /// The raw result of calling jsonDecode
  final Object? rawJson;

  FakeApiTestClientModelResponse200ApplicationJson({
    this.body = const UndefinedWrapper.undefined(),
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
    this.rawJson,
  });

  static Future<FakeApiTestClientModelResponse200ApplicationJson> fromResponse(HttpResponseBase response, {required MediaType contentType, required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
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
          return FakeApiTestClientModelResponse200ApplicationJson(
            headers: response.headers,
            statusCode: response.statusCode,
            reasonPhrase: response.reasonPhrase,
            userContext: userContext,
            body: UndefinedWrapper(res),
            
          );
        } else {
          // since we consumed the stream, we need to publish our read result.
          return FakeApiTestClientModelResponse200ApplicationJson(
            headers: response.headers,
            statusCode: response.statusCode,
            reasonPhrase: response.reasonPhrase,
            userContext: userContext,
            rawJson: v,
            
          );
        }
      default:
    }
    return FakeApiTestClientModelResponse200ApplicationJson(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
      
    );
  }
}


class FakeApiTestEndpointParametersResponse {
  FakeApiTestEndpointParametersResponse({
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
    required T Function(FakeApiTestEndpointParametersResponse400 response) on400,
    required T Function(FakeApiTestEndpointParametersResponse404 response) on404,
    required T Function(FakeApiTestEndpointParametersResponse response) other,
  }) {
    return switch (this) {
      FakeApiTestEndpointParametersResponse400 response => on400(response),
      FakeApiTestEndpointParametersResponse404 response => on404(response),
      _ => other(this),
    };
  }

  static Future<FakeApiTestEndpointParametersResponse> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    final matchedResponse = <(PatternMatchResult, Future<FakeApiTestEndpointParametersResponse> Function())> [
    (
      OASNetworkingUtils.matchesStatusCodePattern(response.statusCode, r'400'),
      () => FakeApiTestEndpointParametersResponse400.fromResponse(response, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
    ),
    (
      OASNetworkingUtils.matchesStatusCodePattern(response.statusCode, r'404'),
      () => FakeApiTestEndpointParametersResponse404.fromResponse(response, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
    ),
    ].pickPrioritized();
    if (matchedResponse != null) {
      return matchedResponse();
    }
    return FakeApiTestEndpointParametersResponse(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}



class FakeApiTestEndpointParametersResponse400 extends FakeApiTestEndpointParametersResponse {
  FakeApiTestEndpointParametersResponse400({
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
  });




  static Future<FakeApiTestEndpointParametersResponse400> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    return FakeApiTestEndpointParametersResponse400(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}





class FakeApiTestEndpointParametersResponse404 extends FakeApiTestEndpointParametersResponse {
  FakeApiTestEndpointParametersResponse404({
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
  });




  static Future<FakeApiTestEndpointParametersResponse404> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    return FakeApiTestEndpointParametersResponse404(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}




class FakeApiTestEnumParametersResponse {
  FakeApiTestEnumParametersResponse({
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
    required T Function(FakeApiTestEnumParametersResponse400 response) on400,
    required T Function(FakeApiTestEnumParametersResponse404 response) on404,
    required T Function(FakeApiTestEnumParametersResponse response) other,
  }) {
    return switch (this) {
      FakeApiTestEnumParametersResponse400 response => on400(response),
      FakeApiTestEnumParametersResponse404 response => on404(response),
      _ => other(this),
    };
  }

  static Future<FakeApiTestEnumParametersResponse> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    final matchedResponse = <(PatternMatchResult, Future<FakeApiTestEnumParametersResponse> Function())> [
    (
      OASNetworkingUtils.matchesStatusCodePattern(response.statusCode, r'400'),
      () => FakeApiTestEnumParametersResponse400.fromResponse(response, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
    ),
    (
      OASNetworkingUtils.matchesStatusCodePattern(response.statusCode, r'404'),
      () => FakeApiTestEnumParametersResponse404.fromResponse(response, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
    ),
    ].pickPrioritized();
    if (matchedResponse != null) {
      return matchedResponse();
    }
    return FakeApiTestEnumParametersResponse(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}



class FakeApiTestEnumParametersResponse400 extends FakeApiTestEnumParametersResponse {
  FakeApiTestEnumParametersResponse400({
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
  });




  static Future<FakeApiTestEnumParametersResponse400> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    return FakeApiTestEnumParametersResponse400(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}





class FakeApiTestEnumParametersResponse404 extends FakeApiTestEnumParametersResponse {
  FakeApiTestEnumParametersResponse404({
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
  });




  static Future<FakeApiTestEnumParametersResponse404> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    return FakeApiTestEnumParametersResponse404(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}




class FakeApiTestGroupParametersResponse {
  FakeApiTestGroupParametersResponse({
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
    required T Function(FakeApiTestGroupParametersResponse400 response) on400,
    required T Function(FakeApiTestGroupParametersResponse response) other,
  }) {
    return switch (this) {
      FakeApiTestGroupParametersResponse400 response => on400(response),
      _ => other(this),
    };
  }

  static Future<FakeApiTestGroupParametersResponse> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    final matchedResponse = <(PatternMatchResult, Future<FakeApiTestGroupParametersResponse> Function())> [
    (
      OASNetworkingUtils.matchesStatusCodePattern(response.statusCode, r'400'),
      () => FakeApiTestGroupParametersResponse400.fromResponse(response, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
    ),
    ].pickPrioritized();
    if (matchedResponse != null) {
      return matchedResponse();
    }
    return FakeApiTestGroupParametersResponse(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}



class FakeApiTestGroupParametersResponse400 extends FakeApiTestGroupParametersResponse {
  FakeApiTestGroupParametersResponse400({
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
  });




  static Future<FakeApiTestGroupParametersResponse400> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    return FakeApiTestGroupParametersResponse400(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}




class FakeApiTestInlineAdditionalPropertiesResponse {
  FakeApiTestInlineAdditionalPropertiesResponse({
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
    required T Function(FakeApiTestInlineAdditionalPropertiesResponse200 response) on200,
    required T Function(FakeApiTestInlineAdditionalPropertiesResponse response) other,
  }) {
    return switch (this) {
      FakeApiTestInlineAdditionalPropertiesResponse200 response => on200(response),
      _ => other(this),
    };
  }

  static Future<FakeApiTestInlineAdditionalPropertiesResponse> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    final matchedResponse = <(PatternMatchResult, Future<FakeApiTestInlineAdditionalPropertiesResponse> Function())> [
    (
      OASNetworkingUtils.matchesStatusCodePattern(response.statusCode, r'200'),
      () => FakeApiTestInlineAdditionalPropertiesResponse200.fromResponse(response, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
    ),
    ].pickPrioritized();
    if (matchedResponse != null) {
      return matchedResponse();
    }
    return FakeApiTestInlineAdditionalPropertiesResponse(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}



class FakeApiTestInlineAdditionalPropertiesResponse200 extends FakeApiTestInlineAdditionalPropertiesResponse {
  FakeApiTestInlineAdditionalPropertiesResponse200({
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
  });




  static Future<FakeApiTestInlineAdditionalPropertiesResponse200> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    return FakeApiTestInlineAdditionalPropertiesResponse200(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}




class FakeApiTestInlineFreeformAdditionalPropertiesResponse {
  FakeApiTestInlineFreeformAdditionalPropertiesResponse({
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
    required T Function(FakeApiTestInlineFreeformAdditionalPropertiesResponse200 response) on200,
    required T Function(FakeApiTestInlineFreeformAdditionalPropertiesResponse response) other,
  }) {
    return switch (this) {
      FakeApiTestInlineFreeformAdditionalPropertiesResponse200 response => on200(response),
      _ => other(this),
    };
  }

  static Future<FakeApiTestInlineFreeformAdditionalPropertiesResponse> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    final matchedResponse = <(PatternMatchResult, Future<FakeApiTestInlineFreeformAdditionalPropertiesResponse> Function())> [
    (
      OASNetworkingUtils.matchesStatusCodePattern(response.statusCode, r'200'),
      () => FakeApiTestInlineFreeformAdditionalPropertiesResponse200.fromResponse(response, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
    ),
    ].pickPrioritized();
    if (matchedResponse != null) {
      return matchedResponse();
    }
    return FakeApiTestInlineFreeformAdditionalPropertiesResponse(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}



class FakeApiTestInlineFreeformAdditionalPropertiesResponse200 extends FakeApiTestInlineFreeformAdditionalPropertiesResponse {
  FakeApiTestInlineFreeformAdditionalPropertiesResponse200({
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
  });




  static Future<FakeApiTestInlineFreeformAdditionalPropertiesResponse200> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    return FakeApiTestInlineFreeformAdditionalPropertiesResponse200(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}




class FakeApiTestJsonFormDataResponse {
  FakeApiTestJsonFormDataResponse({
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
    required T Function(FakeApiTestJsonFormDataResponse200 response) on200,
    required T Function(FakeApiTestJsonFormDataResponse response) other,
  }) {
    return switch (this) {
      FakeApiTestJsonFormDataResponse200 response => on200(response),
      _ => other(this),
    };
  }

  static Future<FakeApiTestJsonFormDataResponse> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    final matchedResponse = <(PatternMatchResult, Future<FakeApiTestJsonFormDataResponse> Function())> [
    (
      OASNetworkingUtils.matchesStatusCodePattern(response.statusCode, r'200'),
      () => FakeApiTestJsonFormDataResponse200.fromResponse(response, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
    ),
    ].pickPrioritized();
    if (matchedResponse != null) {
      return matchedResponse();
    }
    return FakeApiTestJsonFormDataResponse(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}



class FakeApiTestJsonFormDataResponse200 extends FakeApiTestJsonFormDataResponse {
  FakeApiTestJsonFormDataResponse200({
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
  });




  static Future<FakeApiTestJsonFormDataResponse200> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    return FakeApiTestJsonFormDataResponse200(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}




class FakeApiTestQueryParameterCollectionFormatResponse {
  FakeApiTestQueryParameterCollectionFormatResponse({
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
    required T Function(FakeApiTestQueryParameterCollectionFormatResponse200 response) on200,
    required T Function(FakeApiTestQueryParameterCollectionFormatResponse response) other,
  }) {
    return switch (this) {
      FakeApiTestQueryParameterCollectionFormatResponse200 response => on200(response),
      _ => other(this),
    };
  }

  static Future<FakeApiTestQueryParameterCollectionFormatResponse> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    final matchedResponse = <(PatternMatchResult, Future<FakeApiTestQueryParameterCollectionFormatResponse> Function())> [
    (
      OASNetworkingUtils.matchesStatusCodePattern(response.statusCode, r'200'),
      () => FakeApiTestQueryParameterCollectionFormatResponse200.fromResponse(response, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
    ),
    ].pickPrioritized();
    if (matchedResponse != null) {
      return matchedResponse();
    }
    return FakeApiTestQueryParameterCollectionFormatResponse(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}



class FakeApiTestQueryParameterCollectionFormatResponse200 extends FakeApiTestQueryParameterCollectionFormatResponse {
  FakeApiTestQueryParameterCollectionFormatResponse200({
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
  });




  static Future<FakeApiTestQueryParameterCollectionFormatResponse200> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    return FakeApiTestQueryParameterCollectionFormatResponse200(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}




class FakeApiTestStringMapReferenceResponse {
  FakeApiTestStringMapReferenceResponse({
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
    required T Function(FakeApiTestStringMapReferenceResponse200 response) on200,
    required T Function(FakeApiTestStringMapReferenceResponse response) other,
  }) {
    return switch (this) {
      FakeApiTestStringMapReferenceResponse200 response => on200(response),
      _ => other(this),
    };
  }

  static Future<FakeApiTestStringMapReferenceResponse> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    final matchedResponse = <(PatternMatchResult, Future<FakeApiTestStringMapReferenceResponse> Function())> [
    (
      OASNetworkingUtils.matchesStatusCodePattern(response.statusCode, r'200'),
      () => FakeApiTestStringMapReferenceResponse200.fromResponse(response, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
    ),
    ].pickPrioritized();
    if (matchedResponse != null) {
      return matchedResponse();
    }
    return FakeApiTestStringMapReferenceResponse(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}



class FakeApiTestStringMapReferenceResponse200 extends FakeApiTestStringMapReferenceResponse {
  FakeApiTestStringMapReferenceResponse200({
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
  });




  static Future<FakeApiTestStringMapReferenceResponse200> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    return FakeApiTestStringMapReferenceResponse200(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}






